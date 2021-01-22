module Main exposing (..)

import Archimate as Archi
import Browser
import Html exposing (Html, text)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { archi : Archi.ArchimateModel
    }


type Msg
    = Noop


init : () -> ( Model, Cmd Msg )
init _ =
    ( { archi = Archi.empty }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    text "Hello World!"
