namespace CorrelationDrawing.SemanticTypes
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus
open CorrelationDrawing.Types



type GeometryType = Point = 0  | Line = 1      | Polyline = 2     | Polygon = 3 | DnS = 4       | Undefined = 5
type SemanticType = Metric = 0 | Angular = 1   | Hierarchical = 2 | Dummy = 3   | Undefined = 4

  type SemanticId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module SemanticId = 
    let invalid = {id = ""}
    let newId unit : SemanticId  = 
      {id = System.Guid.NewGuid().ToString()}


  [<DomainType>]
  type Semantic = {
     [<NonIncremental;PrimaryKey>]
     id                : SemanticId

     [<NonIncremental>]
     timestamp         : string

     state             : State
     label             : TextInput
     style             : Style
     semanticType      : SemanticType
     geometryType      : GeometryType
     level             : NodeLevel
   }

   type SemanticsSortingOption = Label = 0 | Level = 1 | GeometryType = 2 | SemanticType = 3 | SemanticId = 4 | Timestamp = 5

   [<DomainType>]
   type SemanticApp = {
     semantics           : hmap<SemanticId, Semantic>
     semanticsList       : plist<Semantic>
     selectedSemantic    : SemanticId
     sortBy              : SemanticsSortingOption
     creatingNew         : bool
   }