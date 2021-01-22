module Archimate exposing (Aspect, Element, Layer(..), Model, Relationship, decoder, element, hasAnyExternalElements, rel)

import Dict exposing (Dict)
import Maybe
import Xml.Decode as D exposing (Decoder)


type alias Model =
    { name : String
    , documentation : String
    , elements : List Element
    , relationships : List Relationship
    }


type alias Element =
    { identifier : String, type_ : String, name : String }


type alias Relationship =
    { identifier : String, source : String, target : String, type_ : String, name : Maybe String }


decoder : Decoder Model
decoder =
    D.succeed Model
        |> D.optionalPath [ "name" ] (D.single D.string) "Unnamed Model"
        |> D.optionalPath [ "documentation" ] (D.single D.string) ""
        |> D.requiredPath [ "elements", "element" ] (D.list elementDecoder)
        |> D.requiredPath [ "relationships", "relationship" ] (D.list relationshipDecoder)


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


element : Model -> String -> Maybe Element
element model id_ =
    model.elements
        |> List.filter (\e -> e.identifier == id_)
        |> List.head


rel : Model -> String -> Maybe Relationship
rel model id_ =
    model.relationships
        |> List.filter (\e -> e.identifier == id_)
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
