# [elm-review-equals-caseable](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/)

Provides the [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule [🔧`EqualsCaseable.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-equals-caseable/1.0.0/EqualsCaseable#forbid) which reports when `==` is used but there's an equivalent `case of` available.

## reported
```elm
a =
    if list == [] then
        "empty"

    else
        "filled"
```

## not reported
```elm
a =
    case list of
        [] ->
            "empty"

        _ :: _ ->
            "filled"
```

## why

Reasons are pretty similar to [`VariablesBetweenCaseOf.AccessInCases.forbid`](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest/#why).
Let me highlight the most important ones:

(Note: The exhaustiveness benefits shown are possible once [`NoWildcard`](https://github.com/jfmengels/elm-review/discussions/91) is implemented.)

### notifies you when adding a new variant

```elm
type Role
    = Admin
    | User

canBanUser =
    \theUserWhoTriesToBan -> theUserWhoTriesToBan.role == Admin
```
It's a new day and time for a new feature: moderators!
```elm
type Role
    = Admin
    | Moderator
    | User
```
> It compiles, so it works! Time for a break.

☕

> Oh crap! It don't work I'm sad elm has failed me I'm going back to typescript.

Oh no, you made them sad.

With the rules enabled:

```elm
type Role
    = Admin
    | User

canBanUser =
    \theUserWhoTriesToBan -> theUserWhoTriesToBan.role == Admin
```
→ `EqualsCaseable.forbid Everywhere` fixes to
```elm
canBanUser =
    \theUserWhoTriesToBan ->
        case theUserWhoTriesToBan.role of
            Admin ->
                True

            _ ->
                False
```
→ `NoWildcard.rule` fixes to
```elm
canBanUser =
    \theUserWhoTriesToBan ->
        case theUserWhoTriesToBan.role of
            Admin ->
                True

            User ->
                False
```
Now if you add moderator, elm will show you where there's more to handle specifically for moderators. Or as a quote by @janiczek on slack
> Now the compiler will tell me all the places I need to make a new decision


### gives the compiler as much information as you have
Here's [a real example](https://github.com/dillonkearns/elm-pages/blob/9d6d30235f7db8d3c2c0b40d78f3cc5a6278562d/src/Pages/Internal/RoutePattern.elm#L418)
```elm
toVariant pattern =
    if ... pattern.ending == Nothing then
        Elm.variant "Index"

    else
        let
            allSegments =
                ...
                    ++ ([ Maybe.map endingToVariantName pattern.ending
                        ]
                            |> List.filterMap identity
                       )
        in
        ...
```
Don't do this to yourself. Let the compiler know as much as you about the path you're in.
```elm
toVariant pattern =
    case pattern.ending of
        Nothing ->
            Elm.variant "Index"

        Just patternEnding ->
            let
                allSegments =
                    ... ++ [ endingToVariantName patternEnding ]
            in
            ...
```

### doesn't make you jump around
Here's [a real example](https://github.com/dillonkearns/elm-pages/blob/9d6d30235f7db8d3c2c0b40d78f3cc5a6278562d/src/Pages/Internal/Platform.elm#L941)
```elm
if fields.method == Form.Get then
    model.key
        |> Maybe.map
            (\key ->
                Browser.Navigation.pushUrl key
                    (appendFormQueryParams fields)
            )
        |> Maybe.withDefault
            Cmd.none

else
    -- wait, ... What is else?
    let
        urlToSubmitTo : Url
        urlToSubmitTo =
            model.url
    in
    fetchRouteData -1
        (UpdateCacheAndUrlNew False model.url Nothing)
        config
        urlToSubmitTo
        (Just fields)
```
be explicit :)
```elm
case fields.method of
    Form.Get ->
        model.key
            |> Maybe.map
                (\key ->
                    Browser.Navigation.pushUrl key
                        (appendFormQueryParams fields)
                )
            |> Maybe.withDefault
                Cmd.none

    Form.Post ->
        let
            urlToSubmitTo : Url
            urlToSubmitTo =
                model.url
        in
        fetchRouteData -1
            (UpdateCacheAndUrlNew False model.url Nothing)
            config
            urlToSubmitTo
            (Just fields)
```


## try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template lue-bird/elm-review-equals-caseable/example
```

## configure

```elm
module ReviewConfig exposing (config)

import EqualsCaseable
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ EqualsCaseable.forbid EqualsCaseable.InIf
    ]
```
