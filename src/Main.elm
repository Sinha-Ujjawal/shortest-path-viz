module Main exposing (main)

import BFS
import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import MyCss
import Set exposing (Set)
import Utils


minWidth : Int
minWidth =
    10


maxWidth : Int
maxWidth =
    50


minHeight : Int
minHeight =
    10


maxHeight : Int
maxHeight =
    50


type SybmolType
    = Start
    | End
    | Obstacle


symbolTypeToString : SybmolType -> String
symbolTypeToString cb =
    case cb of
        Start ->
            "🟢"

        End ->
            "🔴"

        Obstacle ->
            "🚧"


type ClickButtonType
    = ST SybmolType
    | Delete


type alias Model =
    { start : Maybe ( Int, Int )
    , end : Maybe ( Int, Int )
    , obstacles : Set ( Int, Int )
    , path : Set ( Int, Int )
    , clickButtonType : ClickButtonType
    , width : Int
    , height : Int
    }


type Msg
    = NoOp
    | SwitchClickButtonType ClickButtonType
    | ApplyClickButtonTypeOnCell ( Int, Int )
    | ComputePath
    | Reset
    | SetWidth Int
    | SetHeight Int


initModel : Int -> Int -> Model
initModel width height =
    { start = Nothing
    , end = Nothing
    , obstacles = Set.empty
    , path = Set.empty
    , clickButtonType = ST Start
    , width = width
    , height = height
    }


emptyCell : List (Html.Attribute msg) -> Html msg
emptyCell attrs =
    MyCss.styledNode attrs []


startCell : List (Html.Attribute msg) -> Html msg
startCell attrs =
    MyCss.styledNode attrs [ Html.text (symbolTypeToString Start) ]


endCell : List (Html.Attribute msg) -> Html msg
endCell attrs =
    MyCss.styledNode attrs [ Html.text (symbolTypeToString End) ]


obstacleCell : List (Html.Attribute msg) -> Html msg
obstacleCell attrs =
    MyCss.styledNode attrs [ Html.text (symbolTypeToString Obstacle) ]


pathCell : List (Html.Attribute msg) -> Html msg
pathCell attrs =
    MyCss.withBackgroundBlack MyCss.styledNode attrs []


drawGrid : Model -> Html Msg
drawGrid model =
    let
        rowIds =
            List.range 1 model.height

        colIds =
            List.range 1 model.width

        renderCell ( rowId, colId ) =
            let
                cell =
                    if Just ( rowId, colId ) == model.start then
                        startCell

                    else if Just ( rowId, colId ) == model.end then
                        endCell

                    else if Set.member ( rowId, colId ) model.obstacles then
                        obstacleCell

                    else if Set.member ( rowId, colId ) model.path then
                        pathCell

                    else
                        emptyCell
            in
            cell [ Events.onClick (ApplyClickButtonTypeOnCell ( rowId, colId )) ]

        elements =
            Utils.cartesianProduct rowIds colIds
                |> List.map renderCell
    in
    MyCss.styledGrid model.width [] elements


drawButton : String -> Msg -> Html Msg
drawButton buttonText msg =
    Html.styled Html.button
        [ Css.margin (Css.px 10) ]
        [ Events.onClick msg ]
        [ Html.text buttonText ]


drawStartButton : Html Msg
drawStartButton =
    drawButton (symbolTypeToString Start) (SwitchClickButtonType (ST Start))


drawEndButton : Html Msg
drawEndButton =
    drawButton (symbolTypeToString End) (SwitchClickButtonType (ST End))


drawObstacleButton : Html Msg
drawObstacleButton =
    drawButton (symbolTypeToString Obstacle) (SwitchClickButtonType (ST Obstacle))


drawDeleteButton : Html Msg
drawDeleteButton =
    drawButton "x" (SwitchClickButtonType Delete)


printCurrentClickButtonTypeMessage : Model -> Html Msg
printCurrentClickButtonTypeMessage model =
    Html.div []
        [ Html.text
            (case model.clickButtonType of
                ST st ->
                    "Click on grid cell to put " ++ symbolTypeToString st

                Delete ->
                    "Click on grid cell to clear the symbol"
            )
        ]


slider : String -> Int -> Int -> Int -> (Int -> msg) -> Html msg
slider label labelValue min max mkEvent =
    Html.div []
        [ Html.div [] [ Html.text (label ++ ": " ++ String.fromInt labelValue) ]
        , Html.input
            [ Events.onInput (\x -> String.toInt x |> Maybe.withDefault 0 |> mkEvent)
            , Attrs.type_ "range"
            , Attrs.value (String.fromInt labelValue)
            , Attrs.min (String.fromInt min)
            , Attrs.max (String.fromInt max)
            ]
            []
        ]


view : Model -> Html Msg
view model =
    Html.styled Html.div
        [ Css.padding (Css.px 10) ]
        []
        [ drawGrid model
        , Html.styled Html.div
            [ Css.padding (Css.px 10) ]
            []
            [ drawStartButton
            , drawEndButton
            , drawObstacleButton
            , drawDeleteButton
            ]
        , printCurrentClickButtonTypeMessage model
        , Html.div [] [ drawButton "Compute Path" ComputePath ]
        , Html.div [] [ drawButton "Reset" Reset ]
        , slider "Width" model.width minWidth maxWidth SetWidth
        , slider "Height" model.height minHeight maxHeight SetHeight
        ]


symbolTypeAtPosition : Model -> ( Int, Int ) -> Maybe SybmolType
symbolTypeAtPosition model ( rowId, colId ) =
    if Just ( rowId, colId ) == model.start then
        Just Start

    else if Just ( rowId, colId ) == model.end then
        Just End

    else if Set.member ( rowId, colId ) model.obstacles then
        Just Obstacle

    else
        Nothing


applyClickButtonTypeOnCell : ( Int, Int ) -> Model -> Model
applyClickButtonTypeOnCell ( rowId, colId ) model =
    let
        modelWithButtonApplied =
            case symbolTypeAtPosition model ( rowId, colId ) of
                Just st ->
                    case model.clickButtonType of
                        Delete ->
                            case st of
                                Start ->
                                    { model | start = Nothing }

                                End ->
                                    { model | end = Nothing }

                                Obstacle ->
                                    { model | obstacles = Set.remove ( rowId, colId ) model.obstacles }

                        _ ->
                            model

                Nothing ->
                    case model.clickButtonType of
                        ST Start ->
                            { model | start = Just ( rowId, colId ) }

                        ST End ->
                            { model | end = Just ( rowId, colId ) }

                        ST Obstacle ->
                            { model | obstacles = Set.insert ( rowId, colId ) model.obstacles }

                        _ ->
                            model
    in
    { modelWithButtonApplied | path = Set.empty }


validCell : Model -> ( Int, Int ) -> Bool
validCell model ( rowId, colId ) =
    rowId >= 1 && rowId <= model.height && colId >= 1 && colId <= model.width


neighbors : ( Int, Int ) -> Set ( Int, Int )
neighbors ( x, y ) =
    Set.fromList [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]


validNeighbors : Model -> ( Int, Int ) -> Set ( Int, Int )
validNeighbors model loc =
    neighbors loc
        |> Set.filter (validCell model)
        |> Set.filter (\pos -> not (Set.member pos model.obstacles))


withPathComputed : Model -> Model
withPathComputed model =
    case ( model.start, model.end ) of
        ( Just s, Just e ) ->
            { model | path = Set.fromList (BFS.shortestPath s e (validNeighbors model)) }

        _ ->
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Reset ->
            initModel model.width model.height

        SwitchClickButtonType buttonType ->
            { model | clickButtonType = buttonType }

        ApplyClickButtonTypeOnCell pos ->
            applyClickButtonTypeOnCell pos model

        ComputePath ->
            withPathComputed model

        SetWidth width ->
            { model | width = width, path = Set.empty }

        SetHeight height ->
            { model | height = height, path = Set.empty }


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> Html.toUnstyled
        , update = update
        , init = initModel minWidth minHeight
        }
