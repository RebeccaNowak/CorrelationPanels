namespace CorrelationDrawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open SimpleTypes
open Svgplus
open UIPlus
open UIPlus.KeyboardTypes
open Svgplus.RectangleStackTypes
open Svgplus.RectangleType
open Svgplus.CameraType

//[<DomainType>]
//type BorderedRectangle = {
//  leftUpper         : V2d 
//  size              : Size2D
//  fill              : C4b
//  borderColors      : BorderColors
//  bWeight           : SvgWeight
//  selected          : bool
//  dottedBorder      : bool
//}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// BEGIN GUI

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

type AnnotationId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module AnnotationId = 
  let invalid = {id = ""}
  let newId unit : AnnotationId  = 
    {id = System.Guid.NewGuid().ToString()}

type BorderId = {
  id        : string 
} with
  member this.isValid = (this.id <> "")
module BorderId = 
  let invalid = {id = ""}
  let newId unit : BorderId  = 
    {id = System.Guid.NewGuid().ToString()}

//type LogId = {
//  id        : string 
//} with
//  member this.isValid = (this.id <> "")
//module LogId = 
//  let invalid = {id = ""}
//  let newId unit : LogId  = 
//    {id = System.Guid.NewGuid().ToString()}

type LogNodeId = {
  id            : string 
  rectangleId   : RectangleId
} with
  member this.isValid = (this.id <> "")

module LogNodeId = 
  let invalid = 
    {
      id = ""
      rectangleId = RectangleId.invalid
    }
  let newId unit : LogNodeId  = 
    {
      id = System.Guid.NewGuid().ToString()
      rectangleId = RectangleId.newId unit
    }
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




////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type NodeLevel = {
  level : int
} with 
    member this.weight =
      ((8.0 - (float this.level)) * 0.3)

module NodeLevel =
  let LEVEL_MAX = 8
  let INVALID = {level = -1}

  let init integer : NodeLevel = 
    match integer with
      | i when i < 0         -> {level = i}
      | i when i > LEVEL_MAX -> {level = LEVEL_MAX}
      | i                    -> {level = i}

  let isInvalid nodeLevel =
    nodeLevel = INVALID
  
  let availableLevels =
    alist {
      for i in 0..LEVEL_MAX do
        yield {level = i}
    }


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


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
type BorderType  = PositiveInfinity | NegativeInfinity | Normal | Invalid

[<DomainType>]
type Border = {
    [<NonIncremental>]
    id            : BorderId

    nodeId        : LogNodeId
    logId         : RectangleStackId
    isSelected    : bool
    correlation   : Option<BorderId>
    annotationId  : AnnotationId
    point         : V3d
    color         : C4b
    weight        : double
    svgPosition   : V2d

    [<NonIncremental>]
    borderType  : BorderType
}


type LogNodeType             = Hierarchical | HierarchicalLeaf | Metric | Angular | PosInfinity | NegInfinity | Infinity | Empty
type CorrelationPlotViewType = LineView | LogView | CorrelationView
type LogNodeBoxType          = SimpleBox | TwoColorBox | FancyBox
type XAxisFunction           = Average | Minimum | Maximum





[<DomainType>]
type LogNode = {
    [<NonIncremental>]
    id            : LogNodeId
    [<NonIncremental>]
    rectangleId   : RectangleId
    
    logId         : RectangleStackId

    //[<NonIncremental>]
    nodeType           : LogNodeType

    level              : NodeLevel //TODO think about this; performance vs interaction
    lBorder            : option<Border>
    uBorder            : option<Border>
    annotation         : option<AnnotationId>

    children           : plist<LogNode>
    
    mainBody           : Rectangle
    //roseDiagram        : RoseDiagram
    //buttonNorth        : Svgplus.Button
    //buttonSouth        : Svgplus.Button

}
      




[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id              : Svgplus.RectangleStackTypes.RectangleStackId

    state           : State
    //xToSvg          : float -> float
    //yToSvg          : float
    defaultWidth    : float
    nodes           : plist<LogNode>
    annoPoints      : hmap<AnnotationId, V3d>
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

  //offset           : V2d //TODO might want to put into svgOptions
  //zoom             : SvgZoom //TODO might want to put into svgOptions
  //fontSize         : FontSize

}


[<DomainType>]
type CorrelationPlot = {
   diagram          : Svgplus.DA.Diagram
   colourMapApp        : ColourMap

   svgCamera           : SvgCamera
   keyboard            : UIPlus.KeyboardTypes.Keyboard<CorrelationPlot>

   logs                : hmap<Svgplus.RectangleStackTypes.RectangleStackId, GeologicalLog>
   correlations        : plist<Correlation>
   selectedBorder      : Option<Border>
   //aardvark dies: selectedBorder      : Option<(Border * V2d)>
   
   editCorrelations    : bool
   selectedPoints      : hmap<AnnotationId, V3d>
   selectedNode        : option<LogNodeId>
   selectedLog         : option<RectangleStackId>
   secondaryLvl        : NodeLevel
   //creatingNew         : bool
   viewType            : CorrelationPlotViewType

   svgFlags            : SvgFlags
   svgOptions          : SvgOptions

   //logAxisApp          : LogAxisApp
   xAxis               : SemanticId
   semanticApp         : SemanticApp
   currrentYMapping    : Option<float>
   yRange              : Rangef

   xToSvg              : float -> float
   yToSvg              : float
   defaultWidth        : float
   elevationZeroHeight : float
}

[<DomainType>]
type CorrelationPlotModel = {
   correlationPlot     : CorrelationPlot
   semanticApp         : SemanticApp

   //zooming             : bool
   //dragging            : bool
   //lastMousePos        : V2d
   
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<DomainType>]
type CorrelationDrawingModel = {
    keyboard          : Keyboard<CorrelationDrawingModel>
    hoverPosition     : option<Trafo3d>
    working           : option<Annotation>
    projection        : Projection 
    //geometry          : GeometryType
    exportPath        : string
   // flags             : SgFlags
} with
  member self.isDrawing =
    self.keyboard.ctrlPressed


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
    match si with
      | [] -> init
      | si ->
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

        keyboard      : Keyboard<Pages>
        
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
        corrPlot      : CorrelationPlotModel
    }