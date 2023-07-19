# [elm-review-equals-caseable](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-equals-caseable/latest/)

Provides the [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule
[`EqualsCaseable.forbid`](EqualsCaseable#forbid) which reports when `==` is used when there is an equivalent `case of` available.

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
