module EqualsCaseable exposing (forbid, ForbiddenLocation(..))

{-| Rule, reporting `==` when equivalent `case of` exists

@docs forbid, ForbiddenLocation

-}

import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range, emptyRange)
import Pretty
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Util exposing (ListFilled, listAllJustMap, listFilledAll, listFilledAllOkMap, listFilledConcat, listFilledHead, listFilledMap, listFilledOne, listFirstJustMap)


{-| Reports when `==` is used but there's there is an equivalent `case of` available.

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
forbid forbiddenLocation =
    Rule.newModuleRuleSchemaUsingContextCreator "EqualsCaseable.forbid" initialContext
        |> Rule.providesFixesForModuleRule
        |> Rule.withExpressionEnterVisitor
            (\expression context ->
                if
                    context.alreadyCovered
                        |> List.any (\range -> range |> rangeIsInside (expression |> Node.range))
                then
                    ( [], context )

                else
                    expressionVisitor
                        { forbiddenLocation = forbiddenLocation
                        , context = context
                        , expression = expression
                        }
            )
        |> Rule.withExpressionExitVisitor
            (\(Node expressionRange _) context ->
                ( []
                , { context
                    | alreadyCovered =
                        context.alreadyCovered
                            |> List.filter (\range -> range /= expressionRange)
                  }
                )
            )
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


### What should I choose?

If `(\a -> a.something == Variant)` etc is a very common pattern
in your code base, you'll have an easier time
converting all the `if`s first.

To get the discussed benefits like "Now the compiler will tell me all the places I need to make a new decision",
you do need to refactor all these equality checks.

Your goal should be `EqualsCaseable.forbid Everywhere`

-}
type ForbiddenLocation
    = InIf
    | Everywhere


type alias Context =
    { extractSourceCode : Range -> String
    , alreadyCovered : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSourceCode () ->
            { extractSourceCode = extractSourceCode
            , alreadyCovered = []
            }
        )
        |> Rule.withSourceCodeExtractor


errorInfo : { message : String, details : List String }
errorInfo =
    { message = "equivalent `case of` exists"
    , details =
        [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
        , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
        , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
        ]
    }


errorInfoInconsistentEquals : { message : String, details : List String }
errorInfoInconsistentEquals =
    { message = "equivalent `case of` exists"
    , details =
        [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
        , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
        , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
        , """Note: Since your condition uses both /= and ==, fixing isn't automatic. You will need more than 2 cases. An example:

    if a == 'a' && b /= 'b' then
        aANotBB
    else
        maybeBBOrNotAA

as a case of:

    case a of
        'a' ->
            case b of
                'b' ->
                    maybeBBOrNotAA
                
                _ ->
                    aANotBB
        _ ->
            maybeBBOrNotAA
"""
        ]
    }


errorInfoNotAllAndEquals : { message : String, details : List String }
errorInfoNotAllAndEquals =
    { message = "equivalent `case of` exists"
    , details =
        [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
        , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
        , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
        , """Note: Since your condition isn't as simple as `(a == A) && (b == B) && ...`, fixing isn't automatic. You will need more than 2 cases. An example:

    if a == 'a' || b == 'b' then
        aAOrBB
    else
        notAAOrNotBB

as a case of:

    case ( a, b ) of
        ( A, _ ) ->
            aaOrBB
        
        ( _, B ) ->
            aaOrBB
        
        ( _, _ ) ->
            notAAOrNotBB
"""
        ]
    }


errorInfoCurriedPrefixOperation : { message : String, details : List String }
errorInfoCurriedPrefixOperation =
    { message = "equivalent `case of` exists"
    , details =
        [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
        , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
        , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
        , """Note: Since your condition is a curried prefix operation, I don't have a variable name I can use to fix it automatically. An example:

List.all ((/=) [])

as a case of:

List.all
    (\\list ->
        case list of
            [] ->
                False
            
            _ :: _ ->
                True
    )
"""
        ]
    }


matchToCaseOf :
    { matched : ListFilled String
    , pattern : ListFilled Pattern
    , cases :
        { match : String
        , mismatch : String
        }
    }
    -> String
matchToCaseOf match =
    [ "case "
    , match.matched |> toNestedTuple
    , " of\n"
    , [ match.pattern
            |> listFilledMap (\pattern -> pattern |> Elm.Pretty.prettyPattern |> Pretty.pretty 100)
            |> toNestedTuple
      , " ->\n"
      , match.cases.match |> removeSharedIndentation |> indent
      , "\n\n"
      , "_ ->\n"
      , match.cases.mismatch |> removeSharedIndentation |> indent
      ]
        |> String.concat
        |> indent
    ]
        |> String.concat


toNestedTuple : ListFilled String -> String
toNestedTuple =
    \( part0, part1Up ) ->
        case part1Up of
            [] ->
                part0

            part1 :: part2Up ->
                [ "( "
                , part0
                , ", "
                , ( part1, part2Up ) |> toNestedTuple
                , " )"
                ]
                    |> String.concat


indent : String -> String
indent =
    \code -> code |> indentBy 4


indentBy : Int -> String -> String
indentBy howMuch =
    \code ->
        code
            |> String.lines
            |> List.map (\line -> String.repeat howMuch " " ++ line)
            |> String.join "\n"


afterFirstIndentBy : Int -> String -> String
afterFirstIndentBy howMuch =
    \code ->
        code
            |> String.lines
            |> String.join ("\n" ++ String.repeat howMuch " ")


removeSharedIndentation : String -> String
removeSharedIndentation =
    \code ->
        let
            minIndentation : Maybe Int
            minIndentation =
                code
                    |> String.lines
                    |> List.map indentation
                    |> List.minimum
        in
        case minIndentation of
            Nothing ->
                code

            Just indentationToRemove ->
                code
                    |> String.lines
                    |> List.map (String.dropLeft indentationToRemove)
                    |> String.join "\n"


indentation : String -> Int
indentation =
    \line ->
        line
            |> String.foldl
                (\char soFar ->
                    if soFar.done then
                        soFar

                    else
                        case char of
                            ' ' ->
                                { done = False, indentation = soFar.indentation + 1 }

                            _ ->
                                { soFar | done = True }
                )
                { done = False, indentation = 0 }
            |> .indentation


parenthesize : String -> String
parenthesize =
    \code ->
        [ "("
        , code |> String.lines |> String.join "\n " |> indentBy 1
        , ")"
        ]
            |> String.concat


expressionVisitor :
    { forbiddenLocation : ForbiddenLocation
    , expression : Node Expression
    , context : Context
    }
    -> ( List (Rule.Error {}), Context )
expressionVisitor context =
    case context.expression |> Node.value of
        Expression.IfBlock condition (Node onTrueRange _) (Node onFalseRange _) ->
            let
                errors : List (Rule.Error {})
                errors =
                    ifCheck
                        { extractSourceCode = context.context.extractSourceCode
                        , expressionRange = context.expression |> Node.range
                        , onTrueRange = onTrueRange
                        , onFalseRange = onFalseRange
                        , condition = condition
                        }
            in
            case errors of
                [] ->
                    ( [], context.context )

                error0 :: error1Up ->
                    ( error0 :: error1Up
                    , context.context
                        |> addCovered (condition |> Node.range)
                    )

        nonIf ->
            case context.forbiddenLocation of
                InIf ->
                    ( [], context.context )

                Everywhere ->
                    let
                        errors : List (Rule.Error {})
                        errors =
                            nonIfCheck
                                { extractSourceCode = context.context.extractSourceCode
                                , expression = nonIf |> Node (context.expression |> Node.range)
                                }
                    in
                    case errors of
                        [] ->
                            ( [], context.context )

                        error0 :: error1Up ->
                            ( error0 :: error1Up
                            , context.context
                                |> addCovered (context.expression |> Node.range)
                            )


addCovered : Range -> Context -> Context
addCovered newCoveredRange =
    \context ->
        { context
            | alreadyCovered =
                context.alreadyCovered |> (::) newCoveredRange
        }


ifCheck :
    { condition : Node Expression
    , expressionRange : Range
    , extractSourceCode : Range -> String
    , onTrueRange : Range
    , onFalseRange : Range
    }
    -> List (Rule.Error {})
ifCheck context =
    case context.condition |> equalCaseablesPossiblyInAnd of
        Err nonAndOrCaseable ->
            case nonAndOrCaseable |> firstEqualsCaseable of
                Just equalsCaseable ->
                    [ Rule.error errorInfoNotAllAndEquals equalsCaseable.matchedRange ]

                Nothing ->
                    case context.condition |> firstEqualsCaseable of
                        Nothing ->
                            []

                        Just equalsCaseable ->
                            [ Rule.error errorInfoNotAllAndEquals equalsCaseable.matchedRange ]

        Ok parts ->
            case parts |> consistentEquals of
                Nothing ->
                    [ Rule.error errorInfoInconsistentEquals context.expressionRange ]

                Just equals ->
                    [ Rule.errorWithFix
                        errorInfo
                        (context.condition |> Node.range)
                        [ { matched = parts |> listFilledMap (\part -> context.extractSourceCode part.matchedRange)
                          , pattern = parts |> listFilledMap .pattern
                          , cases =
                                casesMatching equals
                                    { match = context.extractSourceCode context.onTrueRange
                                    , mismatch = context.extractSourceCode context.onFalseRange
                                    }
                          }
                            |> matchToCaseOf
                            |> afterFirstIndentBy (context.expressionRange.start.column - 1)
                            |> Fix.replaceRangeBy context.expressionRange
                        ]
                    ]


nonIfCheck : { expression : Node Expression, extractSourceCode : Range -> String } -> List (Rule.Error {})
nonIfCheck context =
    case context.expression |> equalCaseablesPossiblyInAnd of
        Err _ ->
            case context.expression |> Node.value of
                Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: (Node _ argument) :: []) ->
                    if [ "==", "/=" ] |> List.member operator then
                        case argument |> expressionToPattern of
                            Just _ ->
                                [ Rule.error errorInfoCurriedPrefixOperation (context.expression |> Node.range) ]

                            Nothing ->
                                []

                    else
                        []

                _ ->
                    []

        Ok parts ->
            case parts |> consistentEquals of
                Nothing ->
                    [ Rule.error errorInfoInconsistentEquals (context.expression |> Node.range) ]

                Just equals ->
                    [ Rule.errorWithFix
                        errorInfo
                        (context.expression |> Node.range)
                        [ { matched = parts |> listFilledMap (\part -> context.extractSourceCode part.matchedRange)
                          , pattern = parts |> listFilledMap .pattern
                          , cases =
                                casesMatching equals
                                    { match = "True", mismatch = "False" }
                          }
                            |> matchToCaseOf
                            |> parenthesize
                            |> afterFirstIndentBy ((context.expression |> Node.range).start.column - 1)
                            |> Fix.replaceRangeBy (context.expression |> Node.range)
                        ]
                    ]


casesMatching : Equality -> { mismatch : String, match : String } -> { mismatch : String, match : String }
casesMatching equals casesOnEqual =
    case equals of
        Equals ->
            casesOnEqual

        NotEquals ->
            { match = casesOnEqual.mismatch
            , mismatch = casesOnEqual.match
            }


consistentEquals : ListFilled { part_ | equality : Equality } -> Maybe Equality
consistentEquals =
    \parts ->
        if parts |> listFilledAll (\part -> part.equality == Equals) then
            Equals |> Just

        else if parts |> listFilledAll (\part -> part.equality == NotEquals) then
            NotEquals |> Just

        else
            Nothing


equalCaseablesPossiblyInAnd :
    Node Expression
    ->
        Result
            (Node Expression)
            (ListFilled
                { matchedRange : Range
                , pattern : Pattern
                , equality : Equality
                }
            )
equalCaseablesPossiblyInAnd expression =
    case expression |> Node.value of
        Expression.OperatorApplication symbol fixDirection left right ->
            let
                equalityWithCaseable :
                    Equality
                    ->
                        Result
                            (Node Expression)
                            (ListFilled
                                { matchedRange : Range
                                , pattern : Pattern
                                , equality : Equality
                                }
                            )
                equalityWithCaseable equality =
                    case left |> Node.value |> expressionToPattern of
                        Just leftAsPattern ->
                            listFilledOne { matchedRange = right |> Node.range, equality = equality, pattern = leftAsPattern } |> Ok

                        Nothing ->
                            case right |> Node.value |> expressionToPattern of
                                Nothing ->
                                    Expression.OperatorApplication symbol fixDirection left right
                                        |> Node (expression |> Node.range)
                                        |> Err

                                Just rightAsPattern ->
                                    listFilledOne { matchedRange = left |> Node.range, equality = equality, pattern = rightAsPattern } |> Ok
            in
            case symbol of
                "&&" ->
                    ( left, [ right ] ) |> listFilledAllOkMap equalCaseablesPossiblyInAnd |> Result.map listFilledConcat

                "==" ->
                    equalityWithCaseable Equals

                "/=" ->
                    equalityWithCaseable NotEquals

                _ ->
                    Expression.OperatorApplication symbol fixDirection left right
                        |> Node (expression |> Node.range)
                        |> Err

        nonCaseable ->
            nonCaseable |> Node (expression |> Node.range) |> Err


type Equality
    = NotEquals
    | Equals


firstEqualsCaseable : Node Expression -> Maybe { pattern : Pattern, matchedRange : Range, equality : Equality }
firstEqualsCaseable expression =
    case expression |> Node.value of
        Expression.OperatorApplication "||" _ left right ->
            listFirstJustMap firstEqualsCaseable [ left, right ]

        nonAndOrOr ->
            nonAndOrOr
                |> Node (expression |> Node.range)
                |> equalCaseablesPossiblyInAnd
                |> Result.toMaybe
                |> Maybe.map listFilledHead


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


rangeIsInside : Range -> Range -> Bool
rangeIsInside potentialEnclosingRange =
    \range ->
        Range.combine [ range, potentialEnclosingRange ]
            == potentialEnclosingRange
