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



///// IDs /////////////
type SemanticId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module SemanticId = 
  let invalid = {id = ""}
  let newId unit : SemanticId  = 
    {id = System.Guid.NewGuid().ToString()}

type BorderId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module BorderId = 
  let invalid = {id = ""}
  let newId unit : BorderId  = 
    {id = System.Guid.NewGuid().ToString()}

type LogId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module LogId = 
  let invalid = {id = ""}
  let newId unit : LogId  = 
    {id = System.Guid.NewGuid().ToString()}

type LogNodeId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")

module LogNodeId = 
  let invalid = {id = ""}
  let newId unit : LogNodeId  = 
    {id = System.Guid.NewGuid().ToString()}
/////

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


type BorderType  = PositiveInfinity | NegativeInfinity | Normal | Invalid




[<DomainType>]
type Border = {
    [<NonIncremental>]
    id          : BorderId

    nodeId      : LogNodeId
    logId       : LogId
    isSelected  : bool
    correlation : Option<BorderId>
    anno        : Annotation
    point       : V3d
    color       : C4b
    weight      : double
    svgPosition : V2d

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
    lBorder       : Border
    uBorder       : Border
    children      : plist<LogNode>
                  
   // elevation     : float //TODO should be a function if at all
   // range         : Rangef
    //svgPos.Y       : float
    //svgPos.X       : float
    svgPos        : V2d
    size          : V2d
} with 
    member this.range = 
            {Rangef.init with min = this.lBorder.point.Length
                              max = this.uBorder.point.Length}
              

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

[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id          : LogId

    [<NonIncremental>]
    index       : int

    isSelected  : bool
    label       : string
    annoPoints  : list<(V3d * Annotation)>
    nodes       : plist<LogNode>
    range       : Rangef
    camera      : CameraControllerState

    semanticApp : SemanticApp
    xAxis       : SemanticId
    svgYOffset  : float
}

[<DomainType>]
type Correlation = {
  fromBorder    : Border
  toBorder      : Border
}

[<DomainType>]
type CorrelationPlot = {
   logs                : plist<GeologicalLog>
   correlations        : plist<Correlation>
   selectedBorder      : Option<Border>
   //aardvark dies: selectedBorder      : Option<(Border * V2d)>

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

[<DomainType>]
type CorrelationPlotApp = {
   correlationPlot     : CorrelationPlot
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