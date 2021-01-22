module Archimate exposing (..)


type alias ArchimateModel =
    { entities : List String, rels : List String }


empty : ArchimateModel
empty =
    { entities = [], rels = [] }
