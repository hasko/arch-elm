module Main exposing (..)

import Archimate as Archi
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h2, h3, p, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task
import Xml.Decode


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { archi : Maybe Archi.Model
    , contents : Maybe String
    , notice : Maybe String
    }


type Msg
    = ModelRequested
    | ModelSelected File
    | ModelLoaded String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing Nothing, Cmd.none )


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
            case Xml.Decode.run Archi.decoder contents of
                Ok a ->
                    ( { model | contents = Just contents, archi = Just a, notice = Nothing }
                    , Cmd.none
                    )

                Err s ->
                    ( { model | contents = Just contents, archi = Nothing, notice = Just s }
                    , Cmd.none
                    )


subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ case model.notice of
            Nothing ->
                text " "

            Just s ->
                p [ class "notice" ] [ text s ]
        , button [ onClick ModelRequested ] [ text "Load Model" ]
        , case model.archi of
            Nothing ->
                p [] [ text "No model decoded" ]

            Just a ->
                viewArchimateModel a
        , h3 [] [ text "Raw data" ]
        , case model.contents of
            Nothing ->
                p [] [ text "No model loaded. Please upload your Archimate Exchange file" ]

            Just s ->
                pre [] [ text s ]
        ]


viewArchimateModel : Archi.Model -> Html Msg
viewArchimateModel model =
    div []
        [ h2 [] [ text model.name ]
        , h3 [] [ text "Documentation" ]
        , p []
            [ if String.isEmpty model.documentation then
                text "(Undocumented)"

              else
                text model.documentation
            ]
        , h3 [] [ text "Elements" ]
        , table []
            [ thead [] [ tr [] [ th [] [ text "Name" ], th [] [ text "Type" ], th [] [ text "ID" ] ] ]
            , tbody [] (List.map viewElement model.elements)
            ]
        ]


viewElement : Archi.Element -> Html Msg
viewElement e =
    tr [] [ td [] [ text e.name ], td [] [ text e.type_ ], td [] [ text e.identifier ] ]
