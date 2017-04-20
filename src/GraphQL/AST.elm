module GraphQL.AST exposing (..)

import GraphQL.PrettyPrinter as PP exposing (PrettyPrinter)

type Name
    = Name String

prettyPrintName : Name -> PrettyPrinter -> PrettyPrinter
prettyPrintName (Name value) pp =
    PP.write value pp


type alias Variable =
    { name : Name
    }


prettyPrintVariable : Variable -> PrettyPrinter -> PrettyPrinter
prettyPrintVariable variable pp =
    pp
        |> PP.write "$"
        |> prettyPrintName variable.name


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue
    | EnumValue Name
    | VariableValue Variable
    | ObjectValue (List ( Name, Value ))
    | ListValue (List Value)


prettyPrintValue : Value -> PrettyPrinter -> PrettyPrinter
prettyPrintValue value pp =
    case value of
        StringValue string ->
            PP.write ("\"" ++ string ++ "\"") pp

        IntValue int ->
            PP.write (toString int) pp

        FloatValue float ->
            PP.write (toString float) pp

        BoolValue bool ->
            PP.write (String.toLower <| toString bool) pp

        NullValue ->
            PP.write "null" pp

        EnumValue name ->
            prettyPrintName name pp

        VariableValue variable ->
            prettyPrintVariable variable pp

        ObjectValue fields ->
            pp
                |> PP.write "{\n"
                |> PP.indent
                |> (\pp_ -> List.foldl
                        (\memo (name, value) ->
                            memo
                                |> prettyPrintName name
                                |> PP.write ": "
                                |> prettyPrintValue value
                                |> PP.write "\n"
                        )
                        pp_
                        fields
                   )
                |> PP.unindent
                |> PP.write "}"

        ListValue values ->
            pp
                |> PP.write "[\n"
                |> PP.indent
                |> (\pp_ -> List.foldl
                        (\memo value ->
                            memo
                                |> prettyPrintValue value
                                |> PP.write "\n"
                        )
                        pp_
                        fields
                   )
                |> PP.unindent
                |> PP.write "]"

type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type alias Field =
    { alias_ : Maybe Name
    , name : Name
    , arguments : List Argument
    , directives : List Directive
    , selectionSet : List Selection
    }

prettyPrintField field pp =
    let
        name =
            prettyPrintName field.name

        withAlias =
            case field.alias_ of
                Just alias_ ->
                    prettyPrintName alias_ ++ ": " ++ name
                Nothing ->
                    name

        withArguments =
            case field.arguments of
                [] ->
                    withAlias
                _ ->
                    field.arguments
                        |> List.map (\{name, value} -> )
                        |> String.join ", "
                        |> \v -> withAlias ++ "(" ++ v ++ ")"

        withDirectives =
            case field.directives of
                [] ->
                    withArguments
                _ ->
                    field.directives
                        |> List.map (\{ )


type alias Argument =
    { name : Name
    , value : Value
    }

prettyPrintArgument argument =
    prettyPrintName argument.name ++ ": " ++ prettyPrintValue argument.value


type alias Directive =
    { name : Name
    , arguments : List Argument
    }


type alias FragmentSpread =
    { name : Name
    , directives : List Directive
    }


type alias InlineFragment =
    { typeCondition : NamedType
    , directives : List Directive
    , selectionSet : List Selection
    }


type alias NamedType =
    { name : Name
    }


type alias ListType =
    { type_ : Type
    }


type NonNullType
    = NamedNonNull NamedType
    | ListNonNull ListType


type Type
    = NamedTypeType NamedType
    | ListTypeType ListType
    | NonNullTypeType NonNullType


type alias Fragment =
    { name : Name
    , typeCondition : NamedType
    , directives : List Directive
    , selectionSet : List Selection
    }


type OperationType
    = Query
    | Mutation


type alias Operation =
    { operationType : OperationType
    , name : Maybe Name
    , variableDefinitions : List VariableDefinition
    , directives : List Directive
    , selectionSet : List Selection
    }


type alias VariableDefinition =
    { variable : Variable
    , type_ : Type
    , defaultValue : Maybe Value
    }


type Definition
    = FragmentDefinition Fragment
    | OperationDefinition Operation


type alias Document =
    { definitions : List Definition
    }

prettyPrint : Document -> String
prettyPrint document =
