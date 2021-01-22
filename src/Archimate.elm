module Archimate exposing (Element, Model, Relationship, decoder)

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
    { identifier : String, source : String, target : String, type_ : String, name : String }


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
        (D.path [ "name" ] (D.single D.string))
