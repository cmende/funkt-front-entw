module Series02 exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Series01 exposing (..)


type alias Model =
    Int


initialModel : Model
initialModel =
    0


type Msg
    = Increase
    | Decrease
    | Reset
    | Sum
    | Fac


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increase ->
            model + 1

        Decrease ->
            model - 1

        Reset ->
            0

        Sum ->
            sumN model

        Fac ->
            fac model


buttonStyle : Attribute Msg
buttonStyle =
    style
        [ ( "height", "30px" )
        , ( "width", "50px" )
        ]


textStyle : Attribute Msg
textStyle =
    style
        [ ( "backgroundColor", "black" )
        , ( "color", "white" )
        , ( "text-align", "right" )
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ textStyle ] [ text (toString model) ]
        , button [ buttonStyle, onClick Increase ] [ text "+" ]
        , button [ buttonStyle, onClick Decrease ] [ text "-" ]
        , button [ buttonStyle, onClick Reset ] [ text "0" ]
        , button [ buttonStyle, disabled (model < 0), onClick Sum ] [ text "Sum" ]
        , button [ buttonStyle, disabled (model < 0), onClick Fac ] [ text "Fac" ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
