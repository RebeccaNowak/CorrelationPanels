namespace CorrelationDrawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.SceneGraph
open Aardvark.Base
open System


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// BEGIN GENERAL



  //let a = init 22.0
  //let b = degrees a


type Rangef = {
  min     : float
  max     : float
} with 
    member this.range   = this.max - this.min
    member this.mapRange (other : Rangef) = 
      fun (x : float) ->
        x *  (other.range / this.range)
    member this.outer (other : Rangef) : Rangef =
      {
        min = min this.min other.min
        max = max this.max other.max
      }
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rangef =
  let init : Rangef = {
      min     = 0.0
      max     = 0.0
    }

  let calcRangeNoInf (r : Rangef) =
    match r.max with 
      | a when a = Operators.infinity -> r.min * 1.01 //TODO HACK
      | _ -> r.max - r.min



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//FLAGS

type AppFlags =
  | None                  = 0b0000000000
  | ShowDebugView         = 0b0000000001
  

type SgFlags = 
  | None                  = 0b0000000000
  | TestTerrain           = 0b0000000001
  //| Correlations          = 0b0000000010
  //| TestTerrain           = 0b0000000100
  

type SvgFlags = 
  | None                  = 0b0000000000
  | BorderColour          = 0b0000000001
  | RadialDiagrams        = 0b0000000010 
  | Histograms            = 0b0000000100 
    //| EditLogNames          = 0b0000001000
  | LogLabels             = 0b0000100000 
  | XAxis                 = 0b0001000000 
  | YAxis                 = 0b0010000000 
  | EditCorrelations      = 0b0100000000 //TODO stretchLogs?




type Projection   = Linear = 0 | Viewpoint = 1 | Sky = 2
type GeometryType = Point = 0  | Line = 1      | Polyline = 2     | Polygon = 3 | DnS = 4       | Undefined = 5
type SemanticType = Metric = 0 | Angular = 1   | Hierarchical = 2 | Dummy = 3   | Undefined = 4
type Orientation  = Horizontal | Vertical


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
    
type State = New | Edit | Display

type SvgZoom = {
  zoomFactor : float
  //TODO min max
} with 
    static member (+) (a,b) : SvgZoom =
      let newZoom = (a.zoomFactor + b.zoomFactor)
      let checkedZoom =
        match newZoom with
          | a when a <= 0.1 -> 0.1
          | b when b >= 10.0 -> 10.0
          | _ -> newZoom
      {zoomFactor = checkedZoom}

    static member (+) (a : SvgZoom, b : float) : SvgZoom =
      let newZoom = (a.zoomFactor + b)
      let checkedZoom =
        match newZoom with
          | a when a <= 0.1 -> 0.1
          | b when b >= 10.0 -> 10.0
          | _ -> newZoom
      {zoomFactor = checkedZoom}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SvgZoom =
  let defaultZoom = {zoomFactor = 1.0}

  let init d : SvgZoom = 
    let z =
      match d with
        | a when a <= 0.1 -> 0.1
        | b when b >= 10.0 -> 10.0
        | _ -> d
    {zoomFactor = z}

  let add (z : SvgZoom) (d : float) : SvgZoom =
    init (z.zoomFactor + d)

type FontSize = {
  fontSize : int 
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FontSize =
  let defaultSize = {fontSize = 12}
  let min = 10
  let max = 30

  let init d : FontSize = 
    let s =
      match d with
        | a when a <= min -> min
        | b when b >= max -> max
        | _ -> d
    {fontSize = s}

  let add (s : FontSize) (d : int) : FontSize =
    init (s.fontSize + d)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


[<DomainType>]
type Semantic = {
   [<NonIncremental;PrimaryKey>]
   id                : SemanticId

   [<NonIncremental>]
   timestamp         : string

   state             : State
   label             : TextInput
   size              : double
   style             : Style
   semanticType      : SemanticType
   geometryType      : GeometryType
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


 ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
type LogAxisSection = {
    label     : string
    color     : C4b
    range     : Rangef
}

type LogAxisConfigId = {
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
    nativePos     : V2d
    svgSize       : V2d
    nativeSize    : V2d
} with 
    member this.range = 
            {Rangef.init with min = this.lBorder.point.Length
                              max = this.uBorder.point.Length}
              

[<DomainType>]
type LogAxisConfig = { //TODO make dynamic
    [<NonIncremental>]
    id                 : LogAxisConfigId
    [<NonIncremental>]
    label              : string
    [<NonIncremental>]
    defaultRange       : Rangef
    //[<NonIncremental>]
    //metricToSvgSize    : float //TODO put into svgOptions, needs to be able to handle negative numbers!
    [<NonIncremental>]
    defaultGranularity : float //TODO must be positive and > 0; maybe use uint
    [<NonIncremental>]
    styleTemplate      : list<LogAxisSection>
    
}

[<DomainType>]
type LogAxisApp = {
    [<NonIncremental>]
    templates        : list<LogAxisConfig>
    selectedTemplate : LogAxisConfigId
}



[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id          : LogId

    index       : int

    state        : State
    isVisible    : bool

    isSelected   : bool
    label        : TextInput
    annoPoints   : list<(V3d * Annotation)>
    nodes        : plist<LogNode>
    nativeYRange : Rangef
    svgMaxX      : float
    camera       : CameraControllerState

    semanticApp  : SemanticApp
    xAxis        : SemanticId
    yOffset      : float
}

[<DomainType>]
type Correlation = {
  fromBorder    : Border
  toBorder      : Border
}

[<DomainType>]
type SvgOptions = {
  logPadding       : float
  logHeight        : float
  logMaxWidth      : float
  cpWidth          : float
  secLevelWidth    : float
  xAxisScaleFactor : float
  yAxisScaleFactor : float
  xAxisPadding     : float
  yAxisPadding     : float
  yAxisStep        : float
  axisWeight       : float

  offset           : V2d //TODO might want to put into svgOptions
  zoom             : SvgZoom //TODO might want to put into svgOptions
  fontSize         : FontSize

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
   //creatingNew         : bool
   viewType            : CorrelationPlotViewType

   svgFlags            : SvgFlags
   svgOptions          : SvgOptions


   logAxisApp          : LogAxisApp
   xAxis               : SemanticId
   semanticApp         : SemanticApp
   currrentYMapping    : Option<float>
   yRange              : Rangef
}

[<DomainType>]
type CorrelationPlotApp = {
   correlationPlot     : CorrelationPlot
   semanticApp         : SemanticApp
   zooming             : bool
   dragging            : bool
   lastMousePos        : V2i
   
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<DomainType>]
type CorrelationDrawingModel = {
    isDrawing         : bool //maybe change to state selection
    hoverPosition     : option<Trafo3d>
    working           : option<Annotation>
    projection        : Projection 
    //geometry          : GeometryType
    exportPath        : string
   // flags             : SgFlags
}

//[<DomainType>]
//type CorrelationAppModel = {
//    rendering        : RenderingParameters
//    drawing          : CorrelationDrawingModel 
//}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
type SaveType = Annotations | Semantics
type SaveIndex =
  {
    ind : int
  } with
    member this.next : SaveIndex =
      {ind = this.ind + 1}
    member this.filename (t : SaveType) : string =
      match t with
        | SaveType.Annotations ->
          sprintf "%03i_annotations.save" this.ind
        | SaveType.Semantics ->
          sprintf "%03i_semantics.save" this.ind

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SaveIndex =
  let init : SaveIndex =
    {ind = 0}
  let findSavedIndices () =
      System.IO.Directory.GetFiles( "./", "*.save")
        |> Array.toList
        |> List.filter (fun (str : string) ->
                          let (b, _) = System.Int32.TryParse str.[2..4] //TODO hardcoded
                          b
                       )
        |> List.map (fun str -> str.[2..4])
        |> List.map int
        |> List.distinct //TODO only if sem & anno
        |> List.map (fun i -> {ind = i})

  let nextAvaible () =
    let si = findSavedIndices ()
    let max = 
      si |> List.map (fun i -> i.ind)
         |> List.max
    {ind = max}.next
      

[<DomainType>]
type Pages = 
    {
        [<NonIncremental>]
        past          : Option<Pages>

        saveIndices   : list<SaveIndex>
        

        [<NonIncremental>]
        future        : Option<Pages>

        appFlags      : AppFlags
        sgFlags       : SgFlags

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