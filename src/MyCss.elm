module MyCss exposing
    ( StyledElement
    , styledGrid
    , styledNode
    , withBackgroundBackgroundColor
    , withBackgroundBlack
    , withBackgroundBlue
    , withBackgroundGreen
    , withBackgroundRed
    , withBackgroundWhite
    )

import Css
import Html.Styled as Html exposing (Html, styled)


cellSideLengthPixels : Float
cellSideLengthPixels =
    30


type alias StyledElement msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


repeat : Int -> Float -> String
repeat n px =
    "repeat(" ++ String.fromInt n ++ ", " ++ String.fromFloat px ++ "px)"


gridContainer : Int -> List Css.Style
gridContainer width =
    [ Css.property "display" "grid"
    , Css.property "grid-template-columns" (repeat width cellSideLengthPixels)
    , Css.property "grid-template-rows" "auto"
    , Css.property "grid-gap" "10px"
    ]


gridItem : List Css.Style
gridItem =
    [ Css.border3 (Css.px 1.0) Css.solid (Css.rgba 0 0 0 0.8)
    , Css.height (Css.px cellSideLengthPixels)
    , Css.width (Css.px cellSideLengthPixels)
    , Css.justifyContent Css.center
    , Css.alignItems Css.center
    , Css.displayFlex
    ]


styledGrid : Int -> StyledElement msg
styledGrid width =
    styled Html.div (gridContainer width)


styledNode : StyledElement msg
styledNode =
    styled Html.div gridItem


withBackgroundBackgroundColor : Css.Color -> StyledElement msg -> StyledElement msg
withBackgroundBackgroundColor color selm =
    styled selm [ Css.backgroundColor color ]


withBackgroundBlack : StyledElement msg -> StyledElement msg
withBackgroundBlack =
    withBackgroundBackgroundColor (Css.rgb 0 0 0)


withBackgroundWhite : StyledElement msg -> StyledElement msg
withBackgroundWhite =
    withBackgroundBackgroundColor (Css.rgb 255 255 255)


withBackgroundRed : StyledElement msg -> StyledElement msg
withBackgroundRed =
    withBackgroundBackgroundColor (Css.rgb 255 0 0)


withBackgroundGreen : StyledElement msg -> StyledElement msg
withBackgroundGreen =
    withBackgroundBackgroundColor (Css.rgb 0 255 0)


withBackgroundBlue : StyledElement msg -> StyledElement msg
withBackgroundBlue =
    withBackgroundBackgroundColor (Css.rgb 0 0 255)
