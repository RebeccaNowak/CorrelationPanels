namespace CorrelationDrawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives


/// BEGIN GUI

[<DomainType>]
type TextInput = {
    text      : string
    disabled  : bool
    bgColor   : C4b
    size      : option<int>
 } 

[<DomainType>]
type DropdownList<'a> = {
   valueList          : plist<'a>
   selected           : option<'a>
   color              : C4b
   searchable         : bool
   //changeFunction     : (option<'a> -> 'msg) @Thomas proper way?
   //labelFunction      : ('a -> IMod<string>)
   //getIsSelected      : ('a -> IMod<bool>) 
 } 

/// END GUI


// BEGIN GENERAL
type Rangef = {
  min     : float
  max     : float
} with 
    member this.range   = this.max - this.min
    member this.mapRange (other : Rangef) = 
      fun (x : float) ->
        x *  (other.range / this.range)
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rangef =
  let init : Rangef = {
      min     = 0.0
      max     = 0.0
    }

  let calcRange (r : Rangef) =
    match r.max with 
      | a when a = Operators.infinity -> r.min * 1.01 //TODO HACK
      | _ -> r.max - r.min

/// CORRELATION PANELS

type Projection   = Linear = 0 | Viewpoint = 1 | Sky = 2
type GeometryType = Point = 0 | Line = 1 | Polyline = 2 | Polygon = 3 | DnS = 4 | Undefined = 5
type SemanticType = Metric = 0 | Angular = 1 | Hierarchical = 2 | Dummy = 3 | Undefined = 4


[<DomainType>]
type Style = {
    color     : ColorInput
    thickness : NumericInput
 } 


[<DomainType>]
type RenderingParameters = {
    fillMode : FillMode
    cullMode : CullMode
}   
    


type SemanticState = New | Edit | Display
type SemanticId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")

module SemanticId = 
  let invalid = {id = ""}





[<DomainType>]
type Semantic = {
   [<NonIncremental;PrimaryKey>]
   id                : SemanticId

   [<NonIncremental>]
   timestamp         : string

   state             : SemanticState
   label             : TextInput
   size              : double
   style             : Style
   semanticType      : SemanticType
   level             : int
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

[<DomainType>]
type AnnotationPoint = {
  [<NonIncremental>]
  point     : V3d

  selected  : bool
}

[<DomainType>]
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                    : string
    
    [<NonIncremental>]
    geometry              : GeometryType

    [<NonIncremental>]
    projection            : Projection

    [<NonIncremental>]
    semanticType          : SemanticType

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
  annotations         : plist<Annotation>
  selectedAnnotation  : option<string>
}


type BorderStyle = Annotation | Border
type BorderType  = PositiveInfinity | NegativeInfinity | Normal

[<DomainType>]
type Border = {
    anno        : Annotation
    point       : V3d
    color       : C4b
    weight      : double
    styleType   : BorderStyle

    [<NonIncremental>]
    borderType  : BorderType
}

type LogNodeType             = Hierarchical | HierarchicalLeaf | Metric | Angular | PosInfinity | NegInfinity | Infinity | Empty
type CorrelationPlotViewType = LineView | LogView | CorrelationView
type LogNodeBoxType          = SimpleBox | TwoColorBox | FancyBox
type XAxisFunction           = Average | Minimum | Maximum

[<DomainType>]
type LogNodeStyle = {
    label     : string
    color     : C4b
    range     : Rangef
}

type LNStyleListId = {
    id        : string
}

type LogNodeId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")

module LogNodeId = 
  let invalid = {id = ""}




[<DomainType>]
type LogNode = {
    [<NonIncremental>]
    id            : LogNodeId

    label         : string
    isSelected    : bool
    hasDefaultX   : bool
                  
    //[<NonIncremental>]
    nodeType      : LogNodeType

    level         : int //TODO think about this; performance vs interaction
    lBoundary     : Border
    uBoundary     : Border
    children      : plist<LogNode>
                  
   // elevation     : float //TODO should be a function if at all
   // range         : Rangef
    logYPos       : float
    logXPos       : float
    pos           : V3d
    size          : V3d
} with 
    member this.range = 
            {Rangef.init with min = this.lBoundary.point.Length
                              max = this.uBoundary.point.Length}
              

[<DomainType>]
type LogNodeStyleTemplate = { //TODO make dynamic
    [<NonIncremental>]
    id                 : LNStyleListId
    [<NonIncremental>]
    label              : string
    [<NonIncremental>]
    defaultRange       : Rangef
    [<NonIncremental>]
    metricToSvgSize    : float
    [<NonIncremental>]
    defaultGranularity : float //TODO must be positive and > 0; maybe use uint
    [<NonIncremental>]
    styleTemplate      : list<LogNodeStyle>
    
}

[<DomainType>]
type LogNodeStyleApp = {
    [<NonIncremental>]
    templates        : list<LogNodeStyleTemplate>
    selectedTemplate : LNStyleListId
}



type LogId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")

module LogId = 
  let invalid = {id = ""}
  let newId unit : LogId  = //TODO hack: how can I do make this a function without an argument?
    {id = System.Guid.NewGuid().ToString()}

//let a = LogId.newId()
//let b = LogId.newId()


[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id          : LogId

    isSelected  : bool
    label       : string
    annoPoints  : list<(V3d * Annotation)>
    nodes       : plist<LogNode>
    range       : Rangef
    camera      : CameraControllerState

    semanticApp : SemanticApp

    xAxis       : SemanticId
}

[<DomainType>]
type Correlation = {
  fromLog       : LogId
  toLog         : LogId
  fromLogNode   : LogNodeId
  fromBorder    : Border
  toLogNode     : LogNodeId
  toBorder      : Border
}

[<DomainType>]
type CorrelationPlotApp = {
   logs                : plist<GeologicalLog>
   correlations        : plist<Correlation>
   editCorrelations    : bool
   selectedPoints      : list<(V3d * Annotation)>
   annotations         : plist<Annotation>
   selectedLog         : option<LogId>
   secondaryLvl        : int
   creatingNew         : bool
   viewType            : CorrelationPlotViewType
   logNodeStyleApp     : LogNodeStyleApp
   xAxis               : SemanticId
   semanticApp         : SemanticApp
}

//type AnnotationParameters = {Point:V3d;semanticId:string}

[<DomainType>]
type CorrelationDrawingModel = {
    isDrawing         : bool //maybe change to state selection
    hoverPosition     : option<Trafo3d>
    working           : option<Annotation>
    projection        : Projection 
    geometry          : GeometryType
    exportPath        : string
}

//[<DomainType>]
//type CorrelationAppModel = {
//    rendering        : RenderingParameters
//    drawing          : CorrelationDrawingModel 
//}

[<DomainType>]
type Pages = 
    {
        [<NonIncremental>]
        past          : Option<Pages>

        [<NonIncremental>]
        future        : Option<Pages>

        camera        : CameraControllerState
        cullMode      : CullMode
        fill          : bool
        rendering     : RenderingParameters
        dockConfig    : DockConfig

        drawingApp    : CorrelationDrawingModel
        annotationApp : AnnotationApp
        semanticApp   : SemanticApp
        corrPlotApp   : CorrelationPlotApp
    }