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
    ]
"""
                    |> Review.Test.run (forbid EqualsCaseable.Everywhere)
                    |> Review.Test.expectNoErrors
        , test "should report if == []" <|
            \() ->
                """module A exposing (..)
a =
    if list == [] then
        "empty"

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
                                ]
                            , under = "list == []"
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
        ]
