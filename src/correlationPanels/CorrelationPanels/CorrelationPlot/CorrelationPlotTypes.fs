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


open Svgplus.CameraType
open Svgplus.DiagramItemType
open UIPlus

[<DomainType>]
type CorrelationPlot = {
   diagram          : Svgplus.DA.Diagram
   colourMapApp        : ColourMap

   svgCamera           : SvgCamera
   keyboard            : UIPlus.KeyboardTypes.Keyboard<CorrelationPlot>

   logs                : hmap<DiagramItemId, GeologicalLog>
   //logsTable           : UIPlus.TableTypes.Table<GeologicalLog, MGeologicalLog, DiagramItem, GeologicalLog.Action>

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