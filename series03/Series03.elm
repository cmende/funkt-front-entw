module Series03 exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import RNA exposing (..)


type alias Model =
    RNA


initialModel : Model
initialModel =
    Nil


type Msg
    = U
    | A
    | C
    | G
    | Complement
    | Inverse
    | Case


update : Msg -> Model -> Model
update msg model =
    case msg of
        U ->
            extend model RNA.U

        A ->
            extend model RNA.A

        C ->
            extend model RNA.C

        G ->
            extend model RNA.G

        Complement ->
            complement model

        Inverse ->
            inverse model

        Case ->
            inverse model


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



-- TODO Fügen Sie außerdem einen Button hinzu, um die aktuelle Eingabe zu lösen


checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [] [ input [ type_ "checkbox", onClick msg ] [], text title ]


view : Model -> Html Msg
view model =
    div []
        [ div [ textStyle ] [ text (toString model) ]
        , button [ buttonStyle, onClick U ] [ text "U" ]
        , button [ buttonStyle, onClick A ] [ text "A" ]
        , button [ buttonStyle, onClick C ] [ text "C" ]
        , button [ buttonStyle, onClick G ] [ text "G" ]
        , button [ buttonStyle, onClick Complement ] [ text "Komplement" ]
        , button [ buttonStyle, onClick Inverse ] [ text "Invers" ]
        , checkbox Case "Kleinschreibung"
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
