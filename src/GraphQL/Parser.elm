module GraphQL.Parser exposing (..)

import Char
import Set exposing (Set)
import GraphQL.AST as AST
import Parser exposing (..)
import Parser.LanguageKit as Parser


multiOr : List (a -> Bool) -> a -> Bool
multiOr conds val =
    List.foldl
        (\next memo ->
            if memo then
                memo
            else
                next val
        )
        False
        conds


escapables : Set Char
escapables =
    Set.fromList
        [ '\\'
        , '/'
        , 'b'
        , 'f'
        , 'n'
        , 'r'
        , 't'
        ]


keywords : Set String
keywords =
    Set.fromList
        [ "query"
        , "subscription"
        , "mutation"
        , "on"
        , "fragment"
        , "type"
        , "true"
        , "false"
        , "null"
        ]


ignoreChars : Set Char
ignoreChars =
    Set.fromList
        [ '\t'
        , '\n'
        , '\x0D'
        , '\xFEFF'
        , ' '
        , ','
        ]


ws : Parser ()
ws =
    ignore zeroOrMore ((flip Set.member) ignoreChars)


name : Parser AST.Name
name =
    Parser.variable
        (multiOr [ Char.isLower, Char.isUpper, (==) '_' ])
        (multiOr [ Char.isLower, Char.isUpper, Char.isDigit, (==) '_' ])
        keywords
        |> Parser.map AST.Name


variable : Parser AST.Variable
variable =
    succeed AST.Variable
        |. symbol "$"
        |= name


boolValue : Parser AST.Value
boolValue =
    Parser.oneOf
        [ Parser.map (\_ -> AST.BoolValue True) (keyword "true")
        , Parser.map (\_ -> AST.BoolValue False) (keyword "false")
        ]


floatValue : Parser AST.Value
floatValue =
    Parser.map AST.FloatValue Parser.float


intValue : Parser AST.Value
intValue =
    Parser.map AST.IntValue Parser.int


stringValue : Parser AST.Value
stringValue =
    succeed AST.StringValue
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '\x0D' && c /= '\n' && c /= '"')
        |. symbol "\""


enumValue : Parser AST.Value
enumValue =
    Parser.map AST.EnumValue name


listValue : (() -> Parser AST.Value) -> Parser AST.Value
listValue valueParser =
    Parser.map AST.ListValue <|
        Parser.sequence
            { start = "["
            , separator = ""
            , end = "]"
            , spaces = ws
            , item = lazy valueParser
            , trailing = Parser.Optional
            }


kvp_ : (() -> Parser AST.Value) -> Parser ( AST.Name, AST.Value )
kvp_ valueParser =
    succeed (,)
        |= name
        |. ws
        |. symbol ":"
        |. ws
        |= lazy valueParser


objectValue : (() -> Parser AST.Value) -> Parser AST.Value
objectValue valueParser =
    Parser.map AST.ObjectValue <|
        Parser.sequence
            { start = "{"
            , separator = ""
            , end = "}"
            , spaces = ws
            , item = kvp_ valueParser
            , trailing = Parser.Optional
            }


nullValue : Parser AST.Value
nullValue =
    Parser.map (\_ -> AST.NullValue) <| keyword "null"


value : Parser AST.Value
value =
    oneOf
        [ boolValue
        , intValue
        , floatValue
        , stringValue
        , enumValue
        , Parser.map AST.VariableValue variable
        , listValue (\() -> value)
        , objectValue (\() -> value)
        ]


kvp : Parser ( AST.Name, AST.Value )
kvp =
    kvp_ (\() -> value)


curlySeq itemParser =
    succeed identity
        |. symbol "{"
        |. ws
        |= andThen (\n -> curlySeqHelp itemParser [ n ]) itemParser
        |. ws
        |. symbol "}"


curlySeqHelp itemParser revItems =
    oneOf
        [ nextItem itemParser
            |> andThen (\n -> curlySeqHelp itemParser (n :: revItems))
        , succeed (List.reverse revItems)
        ]


nextItem item =
    delayedCommit ws <|
        succeed identity
            |. ws
            |= item


selectionSet : Parser (List AST.Selection)
selectionSet =
    curlySeq <| lazy (\() -> selection_ (\() -> selectionSet))


aliasedName : Parser ( Maybe AST.Name, AST.Name )
aliasedName =
    oneOf
        [ delayedCommitMap
            (\a b -> ( Just a, b ))
            name
            (delayedCommit (ws |. symbol ":" |. ws) name)
        , succeed (\a -> ( Nothing, a ))
            |= name
        ]


argument : Parser AST.Argument
argument =
    Parser.map (uncurry AST.Argument) kvp


arguments : Parser (List AST.Argument)
arguments =
    Parser.sequence
        { start = "("
        , separator = ""
        , end = ")"
        , spaces = ws
        , item = Parser.map (uncurry AST.Argument) kvp
        , trailing = Parser.Optional
        }


argumentsOpt : Parser (List AST.Argument)
argumentsOpt =
    oneOf
        [ arguments
        , Parser.succeed []
        ]


directive : Parser AST.Directive
directive =
    succeed AST.Directive
        |. symbol "@"
        |. ws
        |= name
        |. ws
        |= argumentsOpt


directives : Parser (List AST.Directive)
directives =
    repeat zeroOrMore (directive |. ws)


selectionSetOpt_ : (() -> Parser (List AST.Selection)) -> Parser (List AST.Selection)
selectionSetOpt_ selectionSetParser =
    oneOf
        [ lazy selectionSetParser
        , Parser.succeed []
        ]


field_ : (() -> Parser (List AST.Selection)) -> Parser AST.Field
field_ selectionSetParser =
    succeed (uncurry AST.Field)
        |= aliasedName
        |. ws
        |= argumentsOpt
        |. ws
        |= directives
        |. ws
        |= selectionSetOpt_ selectionSetParser


inlineOrSpread_ : (() -> Parser (List AST.Selection)) -> Parser AST.Selection
inlineOrSpread_ selectionSetParser =
    delayedCommit (symbol "..." |. ws) <|
        oneOf
            [ Parser.map AST.InlineFragmentSelection <|
                succeed AST.InlineFragment
                    |. keyword "on"
                    |. ws
                    |= Parser.map AST.NamedType name
                    |. ws
                    |= directives
                    |. ws
                    |= lazy selectionSetParser
            , Parser.map AST.FragmentSpreadSelection <|
                succeed AST.FragmentSpread
                    |= name
                    |. ws
                    |= directives
            ]


selection_ : (() -> Parser (List AST.Selection)) -> Parser AST.Selection
selection_ selectionSetParser =
    oneOf
        [ Parser.map AST.FieldSelection (field_ selectionSetParser)
        , inlineOrSpread_ selectionSetParser
        ]


fragment : Parser AST.Fragment
fragment =
    succeed AST.Fragment
        |. keyword "fragment"
        |. ws
        |= name
        |. ws
        |. keyword "on"
        |. ws
        |= Parser.map AST.NamedType name
        |. ws
        |= directives
        |. ws
        |= selectionSet


nameOpt : Parser (Maybe AST.Name)
nameOpt =
    oneOf
        [ Parser.map Just name
        , succeed Nothing
        ]


operationType : Parser AST.OperationType
operationType =
    oneOf
        [ Parser.map (\_ -> AST.Query) <| keyword "query"
        , Parser.map (\_ -> AST.Mutation) <| keyword "mutation"
        ]


defaultValue : Parser (Maybe AST.Value)
defaultValue =
    oneOf
        [ Parser.map Just <|
            succeed identity
                |. symbol "="
                |. ws
                |= value
        , succeed Nothing
        ]


namedType : Parser AST.NamedType
namedType =
    Parser.map AST.NamedType name


listType : (() -> Parser AST.Type) -> Parser AST.ListType
listType typeParser =
    succeed AST.ListType
        |. symbol "["
        |. ws
        |= lazy typeParser
        |. ws
        |. symbol "]"


nonNullType : (() -> Parser AST.Type) -> Parser AST.NonNullType
nonNullType typeParser =
    succeed identity
        |= (oneOf [ Parser.map AST.NamedNonNull namedType, Parser.map AST.ListNonNull (listType typeParser) ])
        |. symbol "!"


type_ : Parser AST.Type
type_ =
    oneOf
        [ Parser.map AST.NamedTypeType namedType
        , Parser.map AST.ListTypeType (listType (\_ -> type_))
        , Parser.map AST.NonNullTypeType (nonNullType (\_ -> type_))
        ]


variableDefinition : Parser AST.VariableDefinition
variableDefinition =
    succeed AST.VariableDefinition
        |= variable
        |. ws
        |. symbol ":"
        |. ws
        |= type_
        |. ws
        |= defaultValue


variableDefinitions : Parser (List AST.VariableDefinition)
variableDefinitions =
    oneOf
        [ Parser.sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = ws
            , item = variableDefinition
            , trailing = Parser.Optional
            }
        , Parser.succeed []
        ]


operation : Parser AST.Operation
operation =
    succeed AST.Operation
        |= operationType
        |. ws
        |= nameOpt
        |. ws
        |= variableDefinitions
        |. ws
        |= directives
        |. ws
        |= selectionSet


definition : Parser AST.Definition
definition =
    oneOf
        [ Parser.map AST.FragmentDefinition fragment
        , Parser.map AST.OperationDefinition operation
        ]


document : Parser AST.Document
document =
    succeed AST.Document
        |. ws
        |= repeat oneOrMore (definition |. ws)
        |. ws


parse : String -> Result Parser.Error AST.Document
parse doc =
    Parser.run document doc
