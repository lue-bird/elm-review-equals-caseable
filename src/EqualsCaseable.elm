module EqualsCaseable exposing (forbid, ForbiddenLocation(..))

{-| Rule, reporting `==` when equivalent `case of` exists

@docs forbid, ForbiddenLocation

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (emptyRange)
import Review.Rule as Rule exposing (Rule)


{-| Reports when `==` is used when there is an equivalent `case of` available.

    config =
        [ EqualsCaseable.forbid EqualsCaseable.InIf
        ]

Where the given [`ForbiddenLocation`](#ForbiddenLocation)
can be either

  - "only when used in the test of an `if`" → `InIf`
  - "always" → `Everywhere`


## reported

    a =
        if list == [] then
            "empty"

        else
            "filled"


## not reported

    a =
        case list of
            [] ->
                "empty"

            _ :: _ ->
                "filled"

-}
forbid : ForbiddenLocation -> Rule
forbid reportedLocation =
    Rule.newModuleRuleSchemaUsingContextCreator "EqualsCaseable.forbid" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


{-| Configuration option for [`EqualsCaseable.forbid`](#forbid).
Can be either


### "forbid only when used in the test of an `if`" → `InIf`

reported

    a =
        if list == [] then
            "empty"

        else
            "filled"

not reported

    a =
        case list of
            [] ->
                "empty"

            _ :: _ ->
                "filled"


### "forbid always" → `Everywhere`

reported

    a =
        if list == [] then
            "empty"

        else
            "filled"

    b =
        users |> List.filter (\u -> u.role == Moderator)

not reported

    a =
        users
            |> List.filter
                (\u ->
                    case u.role of
                        Moderator ->
                            True

                        _ ->
                            False
                )

-}
type ForbiddenLocation
    = InIf
    | Everywhere


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        _ ->
            ( [], context )


expressionToPattern : Expression -> Maybe Pattern
expressionToPattern =
    \expression ->
        case expression of
            Expression.UnitExpr ->
                Pattern.UnitPattern |> Just

            Expression.CharLiteral char ->
                char |> Pattern.CharPattern |> Just

            Expression.Literal string ->
                string |> Pattern.StringPattern |> Just

            Expression.ParenthesizedExpression (Node _ inParens) ->
                inParens |> expressionToPattern

            Expression.TupledExpression parts ->
                parts
                    |> listAllJustMap (\(Node _ part) -> part |> expressionToPattern |> Maybe.map (Node emptyRange))
                    |> Maybe.map Pattern.TuplePattern

            Expression.ListExpr elements ->
                elements
                    |> listAllJustMap (\(Node _ el) -> el |> expressionToPattern |> Maybe.map (Node emptyRange))
                    |> Maybe.map Pattern.ListPattern

            Expression.Application ((Node _ (Expression.FunctionOrValue qualification appliedName)) :: arguments) ->
                if appliedName |> isVariantName then
                    arguments
                        |> listAllJustMap (\(Node _ arg) -> arg |> expressionToPattern |> Maybe.map (Node emptyRange))
                        |> Maybe.map (Pattern.NamedPattern { moduleName = qualification, name = appliedName })

                else
                    Nothing

            Expression.Application (_ :: _) ->
                Nothing

            Expression.Application [] ->
                Nothing

            Expression.OperatorApplication _ _ _ _ ->
                Nothing

            Expression.FunctionOrValue _ _ ->
                Nothing

            Expression.RecordAccess _ _ ->
                Nothing

            Expression.LetExpression _ ->
                Nothing

            Expression.IfBlock _ _ _ ->
                Nothing

            Expression.CaseExpression _ ->
                Nothing

            Expression.LambdaExpression _ ->
                Nothing

            Expression.RecordUpdateExpression _ _ ->
                Nothing

            Expression.RecordExpr _ ->
                Nothing

            Expression.Operator _ ->
                Nothing

            Expression.PrefixOperator _ ->
                Nothing

            Expression.RecordAccessFunction _ ->
                Nothing

            Expression.Integer _ ->
                Nothing

            Expression.Negation _ ->
                Nothing

            Expression.Hex _ ->
                Nothing

            Expression.Floatable _ ->
                Nothing

            Expression.GLSLExpression _ ->
                Nothing


{-| I know that this will give us false negatives but elm's checks are rather weird so... better than false positives
-}
isVariantName : String -> Bool
isVariantName name =
    case name |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


listAllJustMap : (a -> Maybe b) -> List a -> Maybe (List b)
listAllJustMap map =
    List.foldr
        (\el soFar ->
            case soFar of
                Nothing ->
                    Nothing

                Just soFarList ->
                    case el |> map of
                        Nothing ->
                            Just soFarList

                        Just elMapped ->
                            soFarList |> (::) elMapped |> Just
        )
        (Just [])
