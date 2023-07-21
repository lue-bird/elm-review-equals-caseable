module Util exposing
    ( listAllJustMap
    , ListFilled, listFilledOne
    , listFilledHead, listFilledAll
    , listFirstJustMap, listFilledAllJustMap, listFilledConcat, listFilledMap
    )

{-|


## list that can be empty

@docs listAllJustMap


## list that can't be empty

@docs ListFilled, listFilledOne
@docs listFilledHead, listFilledAll
@docs listFirstJustMap, listFilledAllJustMap, listFilledConcat, listFilledMap

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


listAllOkMap : (a -> Result error b) -> List a -> Result error (List b)
listAllOkMap map =
    List.foldr
        (\el soFar ->
            case soFar of
                Err soFarError ->
                    soFarError |> Err

                Ok soFarList ->
                    case el |> map of
                        Err elError ->
                            elError |> Err

                        Ok elMapped ->
                            soFarList |> (::) elMapped |> Ok
        )
        (Ok [])


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


listFilledAll : (a -> Bool) -> ListFilled a -> Bool
listFilledAll map =
    \( head, tail ) ->
        (head |> map) && (tail |> List.all map)


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
