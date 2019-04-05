namespace CorrelationDrawing.LogTypes
open Aardvark.Base
open Aardvark.Base.Incremental

open CorrelationDrawing.Types
open CorrelationDrawing.AnnotationTypes
open CorrelationDrawing.LogNodeTypes

open UIPlus.KeyboardTypes

open Svgplus.RectangleStackTypes
open Svgplus.RectangleType
open Svgplus.CameraType
open Svgplus.DiagramItemType


  type LogId = {
    id            : string 
  } with
    member this.isValid = (this.id <> "")

  module LogId = 
    let invalid = 
      {
        id = ""
      }
    let newId unit : LogId  = 
      {
        id = System.Guid.NewGuid().ToString()
      }

  type LogDiagramReferences = {
    itemId         : DiagramItemId
    mainLog        : RectangleStackId
    secondaryLog   : option<RectangleStackId>
  }




  [<DomainType>]
  type GeologicalLog = {
      [<NonIncremental;PrimaryKey>]
      id              : Svgplus.RectangleStackTypes.RectangleStackId
      [<NonIncremental>]
      diagramRef      : LogDiagramReferences
      state           : State
      //xToSvg          : float -> float
      //yToSvg          : float
      defaultWidth    : float
      nodes           : plist<LogNode>
      annoPoints      : hmap<AnnotationId, V3d>
  }

