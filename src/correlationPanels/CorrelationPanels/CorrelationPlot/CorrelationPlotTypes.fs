namespace CorrelationDrawing.CorrelationPlotTypes
open Aardvark.Base
open Aardvark.Base.Incremental

open SimpleTypes

open CorrelationDrawing.Types
open CorrelationDrawing.AnnotationTypes
open CorrelationDrawing.SemanticTypes
open CorrelationDrawing.LogTypes
open CorrelationDrawing.CorrelationTypes
open CorrelationDrawing.LogNodeTypes
open CorrelationDrawing

open Svgplus.CameraType

open Svgplus.DiagramItemType
open UIPlus

type Action = 
  | Clear
  | SvgCameraMessage        of Svgplus.SvgCamera.Action
  | KeyboardMessage         of Keyboard.Action
  // | ToggleSelectLog        of option<RectangleStackId>
  | SelectLog              of DiagramItemId
  //| NewLog                 
//   | TogglePoint            of (V3d * AnnotationId)
  | FinishLog              
//   | SaveLog                of RectangleStackId              
  | DeleteLog              of DiagramItemId
  | LogMessage             of (DiagramItemId * GeologicalLog.Action)
  //| ChangeView             of CorrelationPlotViewType
  //| ChangeXAxis            of (AnnotationApp * SemanticId)
  //| LogAxisAppMessage      of LogAxisApp.Action
  | ToggleEditCorrelations
  | SetSecondaryLevel      of NodeLevel
  //| ToggleFlag             of SvgFlags
  | DiagramMessage         of Svgplus.Diagram.Action
  | MouseMove              of V2d
  | GrainSizeTypeMessage   of ColourMap.Action
  //| KeyDown                of key : Keys
  //| KeyUp                  of key : Keys    

[<DomainType>]
type CorrelationPlot = {
   diagram          : Svgplus.DA.Diagram
   colourMapApp        : ColourMap

   svgCamera           : SvgCamera
   keyboard            : KeyboardTypes.Keyboard<CorrelationPlot>

   logs                : hmap<DiagramItemId, GeologicalLog>

   [<NonIncremental>]
   logsTable           : TableTypes.Table<GeologicalLog, MGeologicalLog, 
                                            MDiagramItem, GeologicalLog.Action, Action>

   correlations        : plist<Correlation>
   selectedBorder      : Option<Border>
   //aardvark dies: selectedBorder      : Option<(Border * V2d)>
   
   editCorrelations    : bool
   selectedPoints      : hmap<AnnotationId, V3d>
   selectedNode        : option<LogNodeId>
   selectedLog         : option<DiagramItemId>
   secondaryLvl        : NodeLevel
   //creatingNew         : bool
   //viewType            : CorrelationPlotViewType

   //svgFlags            : SvgFlags
   //svgOptions          : SvgOptions

   //logAxisApp          : LogAxisApp
   xAxis               : SemanticId
   currrentYMapping    : Option<float>
   yRange              : Rangef

   xToSvg              : float -> float
   yToSvg              : float
   defaultWidth        : float
   elevationZeroHeight : float
}