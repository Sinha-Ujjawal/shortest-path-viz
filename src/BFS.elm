module BFS exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


bfs : comparable -> comparable -> (comparable -> Set comparable) -> Dict comparable (Maybe comparable)
bfs start end neighborFn =
    let
        go : Set comparable -> Dict comparable (Maybe comparable) -> Dict comparable (Maybe comparable)
        go frontier parentMap =
            let
                folder u xs =
                    let
                        folder2 v ( oldf, oldfpm ) =
                            ( Set.insert v oldf, Dict.insert v (Just u) oldfpm )
                    in
                    neighborFn u
                        |> Set.filter (\v -> not (Dict.member v parentMap))
                        |> Set.foldl folder2 xs

                ( newFrontier, newParentMap ) =
                    Set.foldl folder ( Set.empty, parentMap ) frontier
            in
            if Set.isEmpty frontier || Set.member end frontier then
                parentMap

            else
                go newFrontier newParentMap
    in
    go (Set.singleton start) (Dict.singleton start Nothing)


shortestPath : comparable -> comparable -> (comparable -> Set comparable) -> List comparable
shortestPath start end neighborFn =
    constructPathFromParentMap (bfs start end neighborFn) end


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
