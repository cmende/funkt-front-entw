module Series08 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Keyboard
import Time


type alias Model =
    Comic


type Msg
    = Next
    | Previous
    | Start
    | Response (Result Http.Error Comic)
    | Key Keyboard.KeyCode


type alias Comic =
    { number : Int, title : String, url : String }


url : Int -> String
url n =
    "https://xkcd.com/" ++ (toString n) ++ "/info.0.json"


init : Int -> ( Model, Cmd Msg )
init n =
    ( Comic n "" "", getXkcdComic n )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            next model

        Previous ->
            previous model

        Start ->
            ( model, getXkcdComic 1 )

        Response (Ok comic) ->
            ( comic, Cmd.none )

        Response (Err err) ->
            ( model, Cmd.none )

        Key code ->
            case code of
                38 ->
                    next model

                40 ->
                    previous model

                _ ->
                    ( model, Cmd.none )


next : Model -> ( Model, Cmd Msg )
next model =
    ( model, getXkcdComic (model.number + 1) )


previous : Model -> ( Model, Cmd Msg )
previous model =
    ( model, getXkcdComic (model.number - 1) )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("#" ++ (toString model.number) ++ " " ++ model.title) ]
        , img [ src model.url ] []
        , br [] []
        , button [ onClick Previous ] [ text "<" ]
        , button [ onClick Start ] [ text "1" ]
        , button [ onClick Next ] [ text ">" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs Key


main : Program Never Model Msg
main =
    program
        { init = init 614
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


getXkcdComic : Int -> Cmd Msg
getXkcdComic n =
    Http.send Response (Http.get (url n) xkcdDecoder)


xkcdDecoder : Decode.Decoder Comic
xkcdDecoder =
    Decode.map3 Comic
        (Decode.field "num" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "img" Decode.string)
