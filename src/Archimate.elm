module Archimate exposing
    ( Aspect
    , Element
    , Layer(..)
    , Model
    , Relationship
    , addElement
    , decoder
    , elementById
    , elementTypes
    , empty
    , hasAnyExternalElements
    , jsonDecoder
    , jsonEncode
    , relationshipById
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Maybe
import Xml.Decode as D exposing (Decoder)


type alias Model =
    { name : String
    , documentation : String
    , elements : List Element
    , relationships : List Relationship
    , next_id : Int
    }


type alias Element =
    { identifier : String, type_ : String, name : String }


type alias Relationship =
    { identifier : String, source : String, target : String, type_ : String, name : Maybe String }


empty : Model
empty =
    Model "Unnamed Model" "" [] [] 0


decoder : Decoder Model
decoder =
    D.succeed Model
        |> D.optionalPath [ "name" ] (D.single D.string) "Unnamed Model"
        |> D.optionalPath [ "documentation" ] (D.single D.string) ""
        |> D.requiredPath [ "elements", "element" ] (D.list elementDecoder)
        |> D.requiredPath [ "relationships", "relationship" ] (D.list relationshipDecoder)
        |> D.optionalPath [ "xyz_next_id" ] (D.single D.int) 0


elementDecoder : Decoder Element
elementDecoder =
    D.map3 Element
        (D.stringAttr "identifier")
        (D.stringAttr "xsi:type")
        (D.path [ "name" ] (D.single D.string))


relationshipDecoder : Decoder Relationship
relationshipDecoder =
    D.map5 Relationship
        (D.stringAttr "identifier")
        (D.stringAttr "source")
        (D.stringAttr "target")
        (D.stringAttr "xsi:type")
        (D.maybe (D.path [ "name" ] (D.single D.string)))


elementById : Model -> String -> Maybe Element
elementById model id_ =
    model.elements
        |> List.filter (\e -> e.identifier == id_)
        |> List.head


relationshipById : Model -> String -> Maybe Relationship
relationshipById model id_ =
    model.relationships
        |> List.filter (\r -> r.identifier == id_)
        |> List.head


type Layer
    = Strategy
    | Business
    | Application
    | Technology
    | Implementation
    | None


type Aspect
    = PassiveStructure
    | Behavior
    | ActiveStructure
    | Motivation
    | Structure


type Visibility
    = External
    | Internal


metamodelElements : Dict String { layer : Layer, aspect : Aspect, visibility : Visibility }
metamodelElements =
    Dict.empty
        |> Dict.insert "ApplicationComponent" { layer = Application, aspect = ActiveStructure, visibility = Internal }
        |> Dict.insert "ApplicationCollaboration" { layer = Application, aspect = ActiveStructure, visibility = Internal }
        |> Dict.insert "ApplicationInterface" { layer = Application, aspect = ActiveStructure, visibility = External }
        |> Dict.insert "ApplicationFunction" { layer = Application, aspect = Behavior, visibility = Internal }
        |> Dict.insert "ApplicationProcess" { layer = Application, aspect = Behavior, visibility = Internal }
        |> Dict.insert "ApplicationInteraction" { layer = Application, aspect = Behavior, visibility = Internal }
        |> Dict.insert "ApplicationEvent" { layer = Application, aspect = Behavior, visibility = External }
        |> Dict.insert "ApplicationService" { layer = Application, aspect = Behavior, visibility = External }
        |> Dict.insert "DataObject" { layer = Application, aspect = PassiveStructure, visibility = External }


elementTypes : List String
elementTypes =
    Dict.keys metamodelElements


layer : Element -> Maybe Layer
layer e =
    metamodelElements |> Dict.get e.type_ |> Maybe.map .layer


aspect : Element -> Maybe Aspect
aspect e =
    metamodelElements |> Dict.get e.type_ |> Maybe.map .aspect


visibility : Element -> Maybe Visibility
visibility e =
    metamodelElements |> Dict.get e.type_ |> Maybe.map .visibility


hasAnyExternalElements : Model -> Layer -> Bool
hasAnyExternalElements model desiredLayer =
    model.elements
        |> List.any
            (\e ->
                (case layer e of
                    Just actualLayer ->
                        actualLayer == desiredLayer

                    _ ->
                        False
                )
                    && (case visibility e of
                            Just External ->
                                True

                            _ ->
                                False
                       )
            )


jsonEncode : Model -> JE.Value
jsonEncode model =
    JE.object
        [ ( "name", JE.string model.name )
        , ( "doc", JE.string model.documentation )
        , ( "elements", JE.list jsonEncodeElement model.elements )
        , ( "rels", JE.list jsonEncodeRelationship model.relationships )
        , ( "max_id", JE.int model.next_id )
        ]


jsonEncodeElement : Element -> JE.Value
jsonEncodeElement el =
    JE.object
        [ ( "id", JE.string el.identifier )
        , ( "type", JE.string el.type_ )
        , ( "name", JE.string el.name )
        ]


jsonEncodeRelationship : Relationship -> JE.Value
jsonEncodeRelationship rel =
    [ ( "id", JE.string rel.identifier )
    , ( "source", JE.string rel.source )
    , ( "target", JE.string rel.target )
    , ( "type", JE.string rel.type_ )
    ]
        |> List.append
            (case rel.name of
                Nothing ->
                    []

                Just name ->
                    [ ( "name", JE.string name ) ]
            )
        |> JE.object


jsonDecoder : JD.Decoder Model
jsonDecoder =
    JD.map5 Model
        (JD.field "name" JD.string)
        (JD.field "doc" JD.string)
        (JD.field "elements" (JD.list jsonElementDecoder))
        (JD.field "rels" (JD.list jsonRelationshipDecoder))
        (JD.maybe (JD.field "max_id" JD.int)
            |> JD.andThen
                (\m ->
                    case m of
                        Nothing ->
                            JD.succeed 0

                        Just i ->
                            JD.succeed i
                )
        )


jsonElementDecoder : JD.Decoder Element
jsonElementDecoder =
    JD.map3 Element
        (JD.field "id" JD.string)
        (JD.field "type" JD.string)
        (JD.field "name" JD.string)


jsonRelationshipDecoder : JD.Decoder Relationship
jsonRelationshipDecoder =
    JD.map5 Relationship
        (JD.field "id" JD.string)
        (JD.field "source" JD.string)
        (JD.field "target" JD.string)
        (JD.field "type" JD.string)
        (JD.maybe (JD.field "name" JD.string))


addElement : String -> String -> Model -> Model
addElement name type_ model =
    let
        new_id =
            newIdentifier model
    in
    { model
        | elements = Element (String.fromInt (newIdentifier model)) type_ name :: model.elements
        , next_id = new_id + 1
    }


newIdentifier : Model -> Int
newIdentifier model =
    if
        List.any (\e -> e.identifier == String.fromInt model.next_id) model.elements
            || List.any (\r -> r.identifier == String.fromInt model.next_id) model.relationships
    then
        newIdentifier { model | next_id = model.next_id + 1 }

    else
        model.next_id
