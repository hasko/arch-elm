module Main exposing (..)

import Archimate as Archi exposing (jsonEncode)
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, em, h2, h3, p, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan)
import Html.Entity exposing (nbsp)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import Ports
import Task
import Xml.Decode


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { archi : Archi.Model
    , notice : Maybe String
    }


type Msg
    = ModelRequested
    | ModelSelected File
    | ModelLoaded String


init : Maybe JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags of
        Nothing ->
            ( Model Archi.empty Nothing, Cmd.none )

        Just jsonValue ->
            case JD.decodeValue Archi.jsonDecoder jsonValue of
                Ok m ->
                    ( Model m (Just "Previous model loaded."), Cmd.none )

                Err s ->
                    ( Model Archi.empty (Just (JD.errorToString s)), Cmd.none )


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
                    ( { model | archi = a, notice = Nothing }
                    , saveModel a
                    )

                Err s ->
                    ( { model | archi = Archi.empty, notice = Just s }
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
        , viewArchimateModel model.archi
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
                , tbody []
                    (if List.isEmpty model.elements then
                        [ tr [] [ td [ colspan 3 ] [ text "(none)" ] ] ]

                     else
                        List.map viewElement (List.sortBy (\e -> e.name) model.elements)
                    )
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
                , tbody []
                    (if List.isEmpty model.relationships then
                        [ tr [] [ td [ colspan 5 ] [ text "(none)" ] ] ]

                     else
                        List.map (viewRelationship model) model.relationships
                    )
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
            [ case Archi.elementById model r.source of
                Nothing ->
                    em [] [ text r.source ]

                Just e ->
                    text e.name
            ]
        , td []
            [ case Archi.elementById model r.target of
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


saveModel : Archi.Model -> Cmd Msg
saveModel model =
    Archi.jsonEncode model
        |> JE.encode 0
        |> Ports.storeModel
