module Main exposing (..)

import Archimate as Archi exposing (jsonEncode)
import Archimate.Analysis as Analysis
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, em, h2, h3, label, option, p, pre, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, disabled, for, id, value)
import Html.Entity exposing (nbsp)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Maybe.Extra exposing (isNothing)
import Ports
import Task
import Xml.Decode


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { archi : Archi.Model
    , notice : Maybe String
    , selectedElementType : Maybe String
    }


type Msg
    = ModelRequested
    | ModelSelected File
    | ModelLoaded String
    | SelectElementType String
    | AddElement


init : Maybe JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags of
        Nothing ->
            ( Model Archi.empty Nothing Nothing, Cmd.none )

        Just jsonValue ->
            case JD.decodeValue Archi.jsonDecoder jsonValue of
                Ok m ->
                    ( Model m (Just "Previous model loaded from browser local storage.") Nothing, Cmd.none )

                Err s ->
                    ( Model Archi.empty (Just (JD.errorToString s)) Nothing, Cmd.none )


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

        SelectElementType t ->
            let
                sel =
                    if String.isEmpty t then
                        Nothing

                    else
                        Just t
            in
            ( { model | selectedElementType = sel }, Cmd.none )

        AddElement ->
            case model.selectedElementType of
                Nothing ->
                    ( { model | notice = Just "Select an element type first" }, Cmd.none )

                Just t ->
                    let
                        a =
                            Archi.addElement "Unnamed Element" t model.archi
                    in
                    ( { model | archi = a }, saveModel a )


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
        , viewArchimateModel model
        ]


viewArchimateModel : Model -> Html Msg
viewArchimateModel model =
    div []
        [ div []
            [ h2 [] [ text model.archi.name ]
            , div [] (h3 [] [ text "Observations" ] :: observations model.archi)
            , h3 [] [ text "Documentation" ]
            , p []
                [ if String.isEmpty model.archi.documentation then
                    text "(Undocumented)"

                  else
                    text model.archi.documentation
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
                    (if List.isEmpty model.archi.elements then
                        [ tr [] [ td [ colspan 3 ] [ text "(none)" ] ] ]

                     else
                        List.map viewElement (List.sortBy (\e -> e.name) model.archi.elements)
                    )
                ]
            , label [ for "element-type-select" ] [ text "Element type: " ]
            , select [ id "element-type-select", onInput SelectElementType ]
                (option [ value "" ] [ text "--Choose a type--" ]
                    :: List.map
                        (\t -> option [ value t ] [ text t ])
                        Archi.elementTypes
                )
            , button [ onClick AddElement, disabled (isNothing model.selectedElementType) ] [ text "Add Element" ]
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
                    (if List.isEmpty model.archi.relationships then
                        [ tr [] [ td [ colspan 5 ] [ text "(none)" ] ] ]

                     else
                        List.map (viewRelationship model.archi) model.archi.relationships
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
    let
        obs =
            Analysis.observations model
    in
    if List.isEmpty obs then
        [ p [] [ text "None. All good." ] ]

    else
        List.map (\o -> p [] [ text o.description ]) obs


saveModel : Archi.Model -> Cmd Msg
saveModel model =
    Archi.jsonEncode model
        |> JE.encode 0
        |> Ports.storeModel
