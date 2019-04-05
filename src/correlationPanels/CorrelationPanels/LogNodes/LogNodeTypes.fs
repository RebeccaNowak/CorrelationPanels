namespace CorrelationDrawing.LogNodeTypes
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing
open CorrelationDrawing.AnnotationTypes
open UIPlus.KeyboardTypes
open Svgplus.RectangleStackTypes
open Svgplus.RectangleType
open Svgplus.CameraType
open Svgplus.DiagramItemType

open CorrelationDrawing.Types




  type LogNodeId = {
    id            : string 
  } with
    member this.isValid = (this.id <> "")

  module LogNodeId = 
    let invalid = 
      {
        id = ""
      }
    let newId unit : LogNodeId  = 
      {
        id = System.Guid.NewGuid().ToString()
      }


  type BorderId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module BorderId = 
    let invalid = {id = ""}
    let newId unit : BorderId  = 
      {id = System.Guid.NewGuid().ToString()}


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


