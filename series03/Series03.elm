module Series03 exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (..)
import RNA exposing (..)


type alias Model =
    ( RNA, Bool )


initialModel : Model
initialModel =
    ( Nil, False )


type Msg
    = U
    | A
    | C
    | G
    | Complement
    | Inverse
    | Case
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        U ->
            ( extend (first model) RNA.U, second model )

        A ->
            ( extend (first model) RNA.A, second model )

        C ->
            ( extend (first model) RNA.C, second model )

        G ->
            ( extend (first model) RNA.G, second model )

        Complement ->
            ( complement (first model), second model )

        Inverse ->
            ( inverse (first model), second model )

        Case ->
            ( first model, not (second model) )

        Reset ->
            ( Nil, second model )


buttonStyleBase : Attribute Msg
buttonStyleBase =
    style
        [ ( "height", "30px" )
        , ( "width", "50px" )
        ]


buttonStyleAction : Attribute Msg
buttonStyleAction =
    style
        [ ( "height", "30px" )
        , ( "width", "150px" )
        ]


textStyle : Attribute Msg
textStyle =
    style
        [ ( "backgroundColor", "black" )
        , ( "color", "white" )
        , ( "text-align", "right" )
        ]


checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [] [ input [ type_ "checkbox", onClick msg ] [], text title ]


view : Model -> Html Msg
view model =
    div []
        [ div [ textStyle ] [ text (printRNA (second model) (first model)) ]
        , button [ buttonStyleBase, onClick U ] [ text "U" ]
        , button [ buttonStyleBase, onClick A ] [ text "A" ]
        , button [ buttonStyleBase, onClick C ] [ text "C" ]
        , button [ buttonStyleBase, onClick G ] [ text "G" ]
        , button [ buttonStyleAction, onClick Complement ] [ text "Komplement" ]
        , button [ buttonStyleAction, onClick Inverse ] [ text "Invers" ]
        , button [ buttonStyleAction, onClick Reset ] [ text "Löschen" ]
        , checkbox Case "Kleinschreibung"
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
