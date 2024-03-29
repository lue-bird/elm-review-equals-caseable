module Util exposing
    ( listAllJustMap
    , ListFilled, listFilledOne
    , listFilledHead
    , listFirstJustMap, listFilledAllJustMap, listFilledConcat, listFilledMap
    , listFilledToList
    )

{-|


## list that can be empty

@docs listAllJustMap


## list that can't be empty

@docs ListFilled, listFilledOne
@docs listFilledHead
@docs listFirstJustMap, listFilledAllJustMap, listFilledConcat, listFilledMap
@docs listFilledToList

-}


type alias ListFilled a =
    ( a, List a )


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
                            Nothing

                        Just elMapped ->
                            soFarList |> (::) elMapped |> Just
        )
        (Just [])


listFirstJustMap : (a -> Maybe b) -> List a -> Maybe b
listFirstJustMap map =
    List.foldl
        (\el soFar ->
            case soFar of
                Nothing ->
                    case el |> map of
                        Nothing ->
                            Nothing

                        Just elMapped ->
                            elMapped |> Just

                Just found ->
                    found |> Just
        )
        Nothing


listFilledAllJustMap : (a -> Maybe b) -> ListFilled a -> Maybe (ListFilled b)
listFilledAllJustMap map =
    \( head, tail ) ->
        case head |> map of
            Nothing ->
                Nothing

            Just headOk ->
                tail
                    |> listAllJustMap map
                    |> Maybe.map (\tailOk -> ( headOk, tailOk ))


listFilledOne : a -> ListFilled a
listFilledOne onlyElement =
    ( onlyElement, [] )


listFilledMap : (a -> b) -> ( a, List a ) -> ( b, List b )
listFilledMap elementChange =
    \( head, tail ) ->
        ( head |> elementChange, tail |> List.map elementChange )


listFilledHead : ( a, List a ) -> a
listFilledHead =
    \( head, _ ) -> head


listFilledTail : ( a, List a ) -> List a
listFilledTail =
    \( _, tail ) -> tail


listFilledConcat : ListFilled (ListFilled a) -> ListFilled a
listFilledConcat =
    \( nestedHead, nestedTail ) ->
        ( nestedHead |> listFilledHead
        , (nestedHead |> listFilledTail) ++ (nestedTail |> List.concatMap listFilledToList)
        )


listFilledToList : ListFilled a -> List a
listFilledToList =
    \listFilled ->
        (listFilled |> listFilledHead) :: (listFilled |> listFilledTail)
