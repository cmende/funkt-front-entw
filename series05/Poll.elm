module Poll exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict
import Maybe


type alias Model =
    Dict.Dict String Int


initialModel : Model
initialModel =
    Dict.fromList [ ( msgToString Option1, 0 ), ( msgToString Option2, 0 ), ( msgToString Option3, 0 ) ]


type Msg
    = Option1
    | Option2
    | Option3


msgToString : Msg -> String
msgToString msg =
    case msg of
        Option1 ->
            "Option 1"

        Option2 ->
            "Option 2"

        Option3 ->
            "Option 3"


inc : Maybe Int -> Maybe Int
inc =
    -- Maybe.map (+1)
    Maybe.map ((\x -> x + 1))


update : Msg -> Model -> Model
update msg model =
    Dict.update (msgToString msg) inc model


buttonStyleBase : Attribute Msg
buttonStyleBase =
    style
        [ ( "height", "30px" )
        , ( "width", "100px" )
        ]


optionsToButtons : List (Html Msg)
optionsToButtons =
    List.map (\x -> button [ buttonStyleBase, onClick x ] [ text (msgToString x) ]) [ Option1, Option2, Option3 ]


results : Model -> List (Html Msg)
results m =
    Dict.foldr (\k v b -> div [] [ text (k ++ ": " ++ toString v) ] :: b) [] m


view : Model -> Html Msg
view model =
    div []
        (optionsToButtons
            ++ results model
        )


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
