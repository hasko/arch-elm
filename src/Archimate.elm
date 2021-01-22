module Archimate exposing (Element, Model, decoder)

import Xml.Decode as D exposing (Decoder)


type alias Model =
    { name : String
    , documentation : String
    , elements : List Element
    , relationships : List String
    }


type alias Element =
    { identifier : String, type_ : String, name : String }


decoder : Decoder Model
decoder =
    D.succeed Model
        |> D.optionalPath [ "name" ] (D.single D.string) "Unnamed Model"
        |> D.optionalPath [ "documentation" ] (D.single D.string) ""
        |> D.requiredPath [ "elements", "element" ] (D.list elementDecoder)
        |> D.requiredPath [ "relationships" ] (D.list (D.succeed ""))


elementDecoder : Decoder Element
elementDecoder =
    D.map3 Element
        (D.stringAttr "identifier")
        (D.stringAttr "xsi:type")
        (D.path [ "name" ] (D.single D.string))
