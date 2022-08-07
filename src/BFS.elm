module BFS exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


bfs : comparable -> (comparable -> Set comparable) -> Dict comparable (Maybe comparable)
bfs start neighborFn =
    let
        go : List comparable -> Dict comparable (Maybe comparable) -> Dict comparable (Maybe comparable)
        go frontier parentMap =
            let
                folder u xs =
                    let
                        folder2 v ( oldf, oldfpm ) =
                            ( v :: oldf, Dict.insert v (Just u) oldfpm )
                    in
                    neighborFn u
                        |> Set.filter (\v -> not (Dict.member v parentMap))
                        |> Set.foldl folder2 xs

                ( newFrontier, newParentMap ) =
                    List.foldl folder ( [], parentMap ) frontier
            in
            case frontier of
                [] ->
                    parentMap

                _ ->
                    go newFrontier newParentMap
    in
    go [ start ] (Dict.singleton start Nothing)


shortestPath : comparable -> comparable -> (comparable -> Set comparable) -> List comparable
shortestPath start end neighborFn =
    constructPathFromParentMap (bfs start neighborFn) end


constructPathFromParentMap : Dict comparable (Maybe comparable) -> comparable -> List comparable
constructPathFromParentMap parentMap end =
    let
        go acc v =
            case Dict.get v parentMap of
                Just (Just u) ->
                    go (u :: acc) u

                _ ->
                    acc
    in
    go [ end ] end
