namespace CorrelationDrawing.AnnotationTypes
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.SemanticTypes
open CorrelationDrawing.Types
open UIPlus.KeyboardTypes


type AnnotationId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module AnnotationId = 
  let invalid = {id = ""}
  let newId unit : AnnotationId  = 
    {id = System.Guid.NewGuid().ToString()}

[<DomainType>]
type AnnotationPoint = {
  [<NonIncremental>]
  point     : V3d

  selected  : bool
}

[<DomainType>]
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                    : AnnotationId
    
    [<NonIncremental>]
    geometry              : GeometryType

    [<NonIncremental>]
    projection            : Projection

    [<NonIncremental>]
    semanticType          : SemanticType

    [<NonIncremental>]
    elevation             : V3d -> float

    selected              : bool
    hovered               : bool

    semanticId            : SemanticId
    points                : plist<AnnotationPoint>
    segments              : plist<plist<V3d>> //list<Segment>
    visible               : bool
    text                  : string
    overrideStyle         : option<Style>
    //overrideLevel         : option<int>
}

[<DomainType>]
type AnnotationApp = {
  annotations         : hmap<AnnotationId, Annotation>
  selectedAnnotation  : option<AnnotationId>
  keyboard            : Keyboard<AnnotationApp>
}



