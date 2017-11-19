module Series07 exposing (..)

import Html exposing (..)
import Mouse
import Svg
import Svg.Attributes exposing (..)


type Circle
    = Circle ( Int, Int ) Int


type Eye
    = Eye Circle Circle


type alias Model =
    Eye


type Msg
    = Move Mouse.Position


init : Model
init =
    Eye (Circle ( 50, 50 ) 50) (Circle ( 50, 50 ) 20)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Move pos ->
            updatePupilPos model pos


updatePupilPos : Eye -> Mouse.Position -> Eye
updatePupilPos (Eye (Circle ( x, y ) r1) (Circle _ r2)) pos =
    Eye (Circle ( x, y ) r1) (Circle (calcPos r1 r2 ( x, y ) ( pos.x, pos.y )) r2)



-- eye radius -> pupil radius -> eye center -> mouse pos -> pupil center


calcPos : Int -> Int -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
calcPos r1 r2 ( x1, y1 ) ( x2, y2 ) =
    if x2 < x1 + r1 && y2 < y1 + r1 then
        ( x2, y2 )
    else
        ( x1, y1 )


eyeToSvg : Eye -> List (Svg.Svg Msg)
eyeToSvg (Eye (Circle ( cx1, cy1 ) r1) (Circle ( cx2, cy2 ) r2)) =
    [ Svg.circle [ cx (toString cx1), cy (toString cy1), r (toString r1), fill "white", stroke "black" ] []
    , Svg.circle [ cx (toString cx2), cy (toString cy2), r (toString r2) ] []
    ]


view : Model -> Html Msg
view model =
    Svg.svg [] (eyeToSvg model)


main : Program Never Model Msg
main =
    program
        { init = ( init, Cmd.none )
        , subscriptions = (\_ -> Mouse.moves Move)
        , view = view
        , update = update
        }
