module EqualsCaseableTest exposing (all)

import EqualsCaseable exposing (forbid)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "EqualsCaseable"
        [ test "should not report an error on float, int, record, variable, function" <|
            \() ->
                """module A exposing (..)
a =
    [ number == 1
    , number == (2 + 1)
    , record == {}
    , record == { record | field = field }
    , variable == variable
    , field == record.field
    , function == always []
    , function == (+)
    , function == (\\arg -> arg)
    , choice == Variant variable
    , tuple == ( [], variable )
    ]
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectNoErrors
        , test "should not report an error on non-if condition with `forbid InIf`" <|
            \() ->
                """module A exposing (..)
a =
    list == []
"""
                    |> Review.Test.run (forbid EqualsCaseable.InIf)
                    |> Review.Test.expectNoErrors
        , test "should report if == Variant" <|
            \() ->
                """module A exposing (..)
a =
    if choice == Variant then
        "variant"

    else
        "not variant"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "choice == Variant"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case choice of
        Variant ->
            "variant"
        
        _ ->
            "not variant"
"""
                        ]
        , test "should report if == ( Variant [], [ (), ('a'), \"wat\" ] )" <|
            \() ->
                """module A exposing (..)
a =
    if tuple == ( Variant [], [ (), ('a'), "wat" ] ) then
        "specific"

    else
        "not specific"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "tuple == ( Variant [], [ (), ('a'), \"wat\" ] )"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case tuple of
        ( Variant [], [ (), 'a', "wat" ] ) ->
            "specific"
        
        _ ->
            "not specific"
"""
                        ]
        , test "should report if /= []" <|
            \() ->
                """module A exposing (..)
a =
    if list /= [] then
        "filled"

    else
        "empty"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "list /= []"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case list of
        [] ->
            "empty"
        
        _ ->
            "filled"
"""
                        ]
        , test "should report if == [] && == []" <|
            \() ->
                """module A exposing (..)
a =
    if aList == [] && bList == [] then
        "empty"

    else
        "a and or b is filled"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "aList == [] && bList == []"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case ( aList, bList ) of
        ( [], [] ) ->
            "empty"
        
        _ ->
            "a and or b is filled"
"""
                        ]
        , test "should report if /= [] || /= []" <|
            \() ->
                """module A exposing (..)
a =
    if aList /= [] || bList /= [] then
        "a and or b is filled"

    else
        "empty"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "aList /= [] || bList /= []"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case ( aList, bList ) of
        ( [], [] ) ->
            "empty"
        
        _ ->
            "a and or b is filled"
"""
                        ]
        , test "should report if /= [] && /= []" <|
            \() ->
                """module A exposing (..)
a =
    if aList /= [] && bList /= [] then
        "filled"

    else
        "a and b is empty"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                , """Note: Since your condition isn't as simple as `(a == A) && (b == B) && ...` or `(a /= A) || (b /= B) || ...`, fixing isn't automatic. An example:

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
                            , under = "aList /= []"
                            }
                        ]
        , test "should report if == [] || == []" <|
            \() ->
                """module A exposing (..)
a =
    if aList == [] || bList == [] then
        "a and or b empty"

    else
        "filled"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                , """Note: Since your condition isn't as simple as `(a == A) && (b == B) && ...` or `(a /= A) || (b /= B) || ...`, fixing isn't automatic. An example:

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
                            , under = "aList == []"
                            }
                        ]
        , test "should report if == [] && /= []" <|
            \() ->
                """module A exposing (..)
a =
    if aList == [] && bList /= [] then
        "a empty and b filled"

    else
        "a filled and or b empty"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                , """Note: Since your condition isn't as simple as `(a == A) && (b == B) && ...` or `(a /= A) || (b /= B) || ...`, fixing isn't automatic. An example:

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
                            , under = "aList == []"
                            }
                        ]
        , test "should report if == [] || /= []" <|
            \() ->
                """module A exposing (..)
a =
    if aList == [] || bList /= [] then
        "a empty and b filled"

    else
        "a filled and or b empty"
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                , """Note: Since your condition isn't as simple as `(a == A) && (b == B) && ...` or `(a /= A) || (b /= B) || ...`, fixing isn't automatic. An example:

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
                            , under = "aList == []"
                            }
                        ]
        , test "should report if == [] and keep indentation" <|
            \() ->
                """module A exposing (..)
a =
    List.reverse
        (if list == [] then
            "empty"

         else
            "filled"
        )
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "equivalent `case of` exists"
                            , details =
                                [ "You are checking for equality against a value that could be a pattern in an equivalent `case of`!"
                                , "You can replace this check with a `case of` where you use the value you're matching for as a pattern."
                                , "This can aid structuring your code in a way where the compiler knows as much about the current branch as you. Read more in the readme: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/"
                                ]
                            , under = "list == []"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    List.reverse
        (case list of
             [] ->
                 "empty"
             
             _ ->
                 "filled"
        )
"""
                        ]
        , test "should report (/=) []" <|
            \() ->
                """module A exposing (..)
a =
    [ (/=) [] ]
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
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
                            , under = "(/=) []"
                            }
                        ]
        ]
