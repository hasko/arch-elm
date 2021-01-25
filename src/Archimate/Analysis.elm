module Archimate.Analysis exposing (observations)

import Archimate as Archi exposing (Model)


type alias Observation =
    { description : String }


observations : Model -> List Observation
observations model =
    []
        |> (if List.isEmpty model.elements then
                (::) (Observation "The model is empty. Add an element to begin.")

            else
                identity
           )
