module Main exposing (..)

import Archimate as Archi
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Task


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { archi : Maybe Archi.ArchimateModel
    , contents : Maybe String
    }


type Msg
    = ModelRequested
    | ModelSelected File
    | ModelLoaded String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelRequested ->
            ( model
            , Select.file [ "application/xml" ] ModelSelected
            )

        ModelSelected file ->
            ( model
            , Task.perform ModelLoaded (File.toString file)
            )

        ModelLoaded contents ->
            ( { model | contents = Just contents }
            , Cmd.none
            )


subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ModelRequested ] [ text "Load Model" ]
        , case model.contents of
            Nothing ->
                p [] [ text "No model loaded" ]

            Just s ->
                p [] [ text s ]
        ]
