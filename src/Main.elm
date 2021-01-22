module Main exposing (..)

import Archimate as Archi
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, em, h2, h3, p, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Entity exposing (nbsp)
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
    div [ class "container" ]
        [ case model.notice of
            Nothing ->
                text nbsp

            Just s ->
                p [ class "notice" ] [ text s ]
        , button [ onClick ModelRequested ] [ text "Load Model" ]
        , case model.archi of
            Nothing ->
                p [] [ text "No model decoded" ]

            Just a ->
                viewArchimateModel a
        , div []
            [ h3 [] [ text "Raw data" ]
            , case model.contents of
                Nothing ->
                    p [] [ text "No model loaded. Please upload your Archimate Exchange file" ]

                Just s ->
                    pre [ class "smaller" ] [ text s ]
            ]
        ]


viewArchimateModel : Archi.Model -> Html Msg
viewArchimateModel model =
    div []
        [ div []
            [ h2 [] [ text model.name ]
            , div [] (h3 [] [ text "Observations" ] :: observations model)
            , h3 [] [ text "Documentation" ]
            , p []
                [ if String.isEmpty model.documentation then
                    text "(Undocumented)"

                  else
                    text model.documentation
                ]
            ]
        , div []
            [ h3 [] [ text "Elements" ]
            , table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Type" ]
                        , th [ class "lighter" ] [ text "ID" ]
                        ]
                    ]
                , tbody [] (List.map viewElement (List.sortBy (\e -> e.name) model.elements))
                ]
            ]
        , div []
            [ h3 [] [ text "Relationships" ]
            , table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Source" ]
                        , th [] [ text "Target" ]
                        , th [] [ text "Type" ]
                        , th [] [ text "Name" ]
                        , th [ class "lighter" ] [ text "ID" ]
                        ]
                    ]
                , tbody [] (List.map (viewRelationship model) model.relationships)
                ]
            ]
        ]


viewElement : Archi.Element -> Html Msg
viewElement e =
    tr []
        [ td [] [ text e.name ]
        , td [] [ text e.type_ ]
        , td [ class "lighter" ] [ text e.identifier ]
        ]


viewRelationship : Archi.Model -> Archi.Relationship -> Html Msg
viewRelationship model r =
    tr []
        [ td []
            [ case Archi.element model r.source of
                Nothing ->
                    em [] [ text r.source ]

                Just e ->
                    text e.name
            ]
        , td []
            [ case Archi.element model r.target of
                Nothing ->
                    em [] [ text r.target ]

                Just e ->
                    text e.name
            ]
        , td [] [ text r.type_ ]
        , td []
            [ case r.name of
                Nothing ->
                    text nbsp

                Just s ->
                    text s
            ]
        , td [ class "lighter" ] [ text r.identifier ]
        ]


observations : Archi.Model -> List (Html Msg)
observations model =
    []
        |> List.append
            (if Archi.hasAnyExternalElements model Archi.Application then
                []

             else
                [ p [] [ text "The model has no external elements on the Application layer." ] ]
            )
