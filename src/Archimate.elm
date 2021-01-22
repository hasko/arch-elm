module Archimate exposing (Model, decoder)

import Xml.Decode as D exposing (Decoder)


type alias Model =
    { name : String
    , documentation : String
    , elements : List String
    , relationships : List String
    }


decoder : Decoder Model
decoder =
    D.succeed Model
        |> D.optionalPath [ "name" ] (D.single D.string) "Unnamed Model"
        |> D.optionalPath [ "documentation" ] (D.single D.string) ""
        |> D.requiredPath [ "elements" ] (D.list (D.succeed ""))
        |> D.requiredPath [ "relationships" ] (D.list (D.succeed ""))
