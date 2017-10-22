module RNAApp exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (..)
import RNA exposing (..)
import RNA2 exposing (..)


type alias Model =
    { rna : RNA, lowercase : Bool, parseResult : Result String RNA, input : RNA }


initialModel : Model
initialModel =
    { rna = Nil, lowercase = False, parseResult = Err "Keine Eingabe", input = Nil }


type Msg
    = U
    | A
    | C
    | G
    | Complement
    | Inverse
    | Case
    | Reset
    | Parse String
    | Copy


update : Msg -> Model -> Model
update msg model =
    case msg of
        U ->
            { model | rna = extend model.rna RNA.U }

        A ->
            { model | rna = extend model.rna RNA.A }

        C ->
            { model | rna = extend model.rna RNA.C }

        G ->
            { model | rna = extend model.rna RNA.G }

        Complement ->
            { model | rna = complement model.rna }

        Inverse ->
            { model | rna = inverse model.rna }

        Case ->
            { model | lowercase = not model.lowercase }

        Reset ->
            initialModel

        Parse input ->
            case parse input of
                Ok r ->
                    { model | parseResult = parse input, input = r }

                Err _ ->
                    { model | parseResult = parse input }

        Copy ->
            { model | rna = model.input }


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


parseResultStyle : Result String RNA -> Attribute Msg
parseResultStyle r =
    style
        [ ( "backgroundColor"
          , case r of
                Ok _ ->
                    "green"

                Err _ ->
                    "red"
          )
        , ( "color", "white" )
        ]


checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [] [ input [ type_ "checkbox", onClick msg ] [], text title ]


parseResultToBool : Result String RNA -> Bool
parseResultToBool r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


parseResultToString : Result String RNA -> String
parseResultToString r =
    case r of
        Ok _ ->
            "Parsen erfolgreich"

        Err e ->
            e


view : Model -> Html Msg
view model =
    div []
        [ div [ textStyle ] [ text (printRNA model.lowercase model.rna) ]
        , button [ buttonStyleBase, onClick U ] [ text "U" ]
        , button [ buttonStyleBase, onClick A ] [ text "A" ]
        , button [ buttonStyleBase, onClick C ] [ text "C" ]
        , button [ buttonStyleBase, onClick G ] [ text "G" ]
        , button [ buttonStyleAction, onClick Complement ] [ text "Komplement" ]
        , button [ buttonStyleAction, onClick Inverse ] [ text "Invers" ]
        , button [ buttonStyleAction, onClick Reset ] [ text "Löschen" ]
        , checkbox Case "Kleinschreibung"
        , input [ placeholder "Text to parse", onInput Parse ] []
        , button [ buttonStyleAction, disabled (not (parseResultToBool model.parseResult)), onClick Copy ] [ text "Übernehmen" ]
        , div [ parseResultStyle model.parseResult ] [ text (parseResultToString model.parseResult) ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
