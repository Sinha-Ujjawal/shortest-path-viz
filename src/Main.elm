module Main exposing (main)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import MyCss
import Set exposing (Set)
import Utils


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
    , clickButtonType : ClickButtonType
    , width : Int
    , height : Int
    }


type Msg
    = NoOp
    | SwitchClickButtonType ClickButtonType
    | ApplyClickButtonTypeOnCell Int Int
    | Reset


initModel : Int -> Int -> Model
initModel width height =
    { start = Nothing
    , end = Nothing
    , obstacles = Set.empty
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

                    else
                        emptyCell
            in
            cell [ Events.onClick (ApplyClickButtonTypeOnCell rowId colId) ]

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
        , drawButton "Reset" Reset
        ]


symbolTypeAtPosition : Model -> Int -> Int -> Maybe SybmolType
symbolTypeAtPosition model rowId colId =
    if Just ( rowId, colId ) == model.start then
        Just Start

    else if Just ( rowId, colId ) == model.end then
        Just End

    else if Set.member ( rowId, colId ) model.obstacles then
        Just Obstacle

    else
        Nothing


applyClickButtonTypeOnCell : Int -> Int -> Model -> Model
applyClickButtonTypeOnCell rowId colId model =
    case symbolTypeAtPosition model rowId colId of
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Reset ->
            initModel model.width model.height

        SwitchClickButtonType buttonType ->
            { model | clickButtonType = buttonType }

        ApplyClickButtonTypeOnCell rowId colId ->
            applyClickButtonTypeOnCell rowId colId model


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> Html.toUnstyled
        , update = update
        , init = initModel 10 10
        }
