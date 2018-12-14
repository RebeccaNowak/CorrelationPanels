﻿namespace CorrelationDrawing.LogNodes

open SimpleTypes

open CorrelationDrawing
open Svgplus
open Aardvark.Base
open Aardvark.UI

  type Action =
  //| ChangeXAxis         of (AnnotationApp * SemanticId * float)
  | MouseOver           of LogNodeId
  | ToggleSelectNode    of LogNodeId
  | BorderMessage       of Border.Action
  | DrawCorrelation     of BorderId
  | RectangleMessage    of Svgplus.Rectangle.Action
  | ColorPickerMessage  of ColorPicker.Action
  //| RoseDiagramMessage  of Svgplus.RoseDiagram.Action
  //| CorrelationButton   of Svgplus.Button.Action


  module Lens = 
    let svgWidth =
      { new Lens<CorrelationDrawing.LogNode, float>() with
        override x.Get(r)   = r.mainBody.dim.width
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.width.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.width.Update(r.mainBody, f)}
      }

    let svgHeight =
      { new Lens<CorrelationDrawing.LogNode, float>() with
        override x.Get(r)   = r.mainBody.dim.height
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.height.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.height.Update(r.mainBody, f)}
      }

    let svgX =
      { new Lens<CorrelationDrawing.LogNode, float>() with
        override x.Get(r)   = r.mainBody.pos.X
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.posX.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.posX.Update(r.mainBody, f)}
      }

    let svgY =
      { new Lens<CorrelationDrawing.LogNode, float>() with
        override x.Get(r)   = r.mainBody.pos.Y
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.posY.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.posY.Update(r.mainBody, f)}
      }

    let colour =
      { new Lens<CorrelationDrawing.LogNode, C4b>() with
        override x.Get(r)   = r.mainBody.colour.c
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.colour.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.colour.Update(r.mainBody, f)}
      }
    
    let hasAverageWidth =
      { new Lens<CorrelationDrawing.LogNode, bool>() with
        override x.Get(r)   = r.mainBody.dottedBorder
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.dottedBorder.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.dottedBorder.Update(r.mainBody, f)}
      }

    let isSelected =
      { new Lens<CorrelationDrawing.LogNode, bool>() with
        override x.Get(r)   = r.mainBody.isToggled
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.isToggled.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.isToggled.Update(r.mainBody, f)}
      }

    let draw =
      { new Lens<CorrelationDrawing.LogNode, bool>() with
        override x.Get(r)   = r.mainBody.draw
        override x.Set(r,v) = 
          { r with mainBody = Rectangle.Lens.draw.Set(r.mainBody,v)}
        override x.Update(r,f) = 
          {r with mainBody = Rectangle.Lens.draw.Update(r.mainBody, f)}
      }



  //  let _svgX      : Lens<CorrelationDrawing.LogNode,float>  = 
  //                    CorrelationDrawing.LogNode
  //  let _svgY      : Lens<CorrelationDrawing.LogNode,float>  = 
  //    CorrelationDrawing.LogNode.Lens.style |. Style.Lens.thickness |. NumericInput.Lens.value
  //  let _labelText : Lens<CorrelationDrawing.LogNode,string> = CorrelationDrawing.LogNodes.Lens.label |. TextInput.Lens.text


  module Init =
    open Aardvark.Base
    open Aardvark.UI

    let empty  : LogNode = 
 
      {
        id           = LogNodeId.invalid
        logId        = LogId.invalid
      //  isSelected   = false
     //   hasDefaultX  = false
        nodeType     = LogNodeType.Empty
        label        = "log node"
        level        = NodeLevel.INVALID
        lBorder      = None
        uBorder      = None
        annotation   = None
        children     = plist.Empty
        //svgPos.Y      = 0.0
        //svgPos.X      = 0.0
        //nativePos    = V2d.OO
        //nativeSize   = Size2D.init
       // svgPos       = V2d.OO
      //  svgSize      = Size2D.init

        mainBody     = Svgplus.Rectangle.init 
        roseDiagram  = Svgplus.RoseDiagram.init
        buttonNorth  = Svgplus.Button.init
        buttonSouth  = Svgplus.Button.init
      }


    let topLevel 
      (logId    : LogId)
      ((up, ua) : (V3d * AnnotationId)) 
      ((lp, la) : (V3d * AnnotationId)) 
      (children : plist<LogNode>)
      (level    : NodeLevel) : LogNode = 
      let nodeId = LogNodeId.newId()
      let lBorder = Border.initial la lp nodeId logId
      let uBorder = Border.initial ua up nodeId logId
      let nodeType = 
        match (lp = Border.negInf),
              (up = Border.posInf) with
          | true, false  -> LogNodeType.NegInfinity
          | false, true  -> LogNodeType.PosInfinity
          | false, false -> LogNodeType.Hierarchical
          | true, true   -> LogNodeType.Infinity

      { 
        empty with 
          id          = nodeId
          logId       = logId
          nodeType    = nodeType
          label       = "log node"
          level       = level
          lBorder     = Some lBorder
          uBorder     = Some uBorder
          children    = children
      }

    let topLevelWithId
      (nodeId   : LogNodeId)
      (logId    : LogId)
      ((up, ua)  : (V3d * AnnotationId)) 
      ((lp, la) : (V3d * AnnotationId)) 
      (children : plist<LogNode>)
      (level    : NodeLevel) : LogNode = 
      let lBorder = Border.initial la lp nodeId logId
      let uBorder = Border.initial ua up nodeId logId
      let n = topLevel logId (up, ua) (lp, la) children level
      {
        n with id       = nodeId
               logId    = logId
               lBorder  = Some lBorder
               uBorder  = Some uBorder
      }

    // TODO add level
    let hierarchicalLeaf 
      (logId    : LogId)
      (anno     : AnnotationId) 
      (lp       : V3d ) 
      (up       : V3d )  =
      let nodeId = LogNodeId.newId ()
      {empty with
        id        = nodeId
        logId     = logId
        nodeType  = LogNodeType.HierarchicalLeaf
        lBorder   = Some (Border.initial anno lp nodeId logId)
        uBorder   = Some (Border.initial anno up nodeId logId)
      }

    let metric (logId : LogId)  (anno : Annotation) (level : NodeLevel) =
      let nodeId = LogNodeId.newId ()
      let lowestPoint = Annotation.lowestPoint anno
      let highestPoint = Annotation.highestPoint anno
      {empty with
        id         = nodeId
        logId      = logId
        nodeType   = LogNodeType.Metric
        lBorder   = Some (Border.initial anno.id lowestPoint.point nodeId logId)
        uBorder   = Some (Border.initial anno.id highestPoint.point nodeId logId)
        annotation = Some anno.id
        level = level
      }

    let angular (logId : LogId) (anno : Annotation) (level : NodeLevel) =
      let nodeId = LogNodeId.newId ()
      let lowestPoint = Annotation.lowestPoint anno
      let highestPoint = Annotation.highestPoint anno
      {empty with
        id         = nodeId
        logId      = logId
        nodeType   = LogNodeType.Angular
        lBorder   = Some (Border.initial anno.id lowestPoint.point nodeId logId)
        uBorder   = Some (Border.initial anno.id highestPoint.point nodeId logId)
        annotation = Some anno.id
        level = level
      }

        //////////////////////////////////////////
    let fromSemanticType (a : Annotation) (semApp : SemanticApp) 
                         (logId : LogId) 
                         (lp : V3d) (up : V3d)
                         (level : NodeLevel) =   
      let semType = Annotation.getType semApp a
      let (lvl : NodeLevel) = {level = level.level + 1}
      match semType with
        | SemanticType.Hierarchical -> hierarchicalLeaf logId a.id lp up
        | SemanticType.Angular -> angular logId a lvl
        | SemanticType.Metric -> metric logId a lvl
        | SemanticType.Undefined -> empty //TODO something useful
        | _ -> empty //TODO something useful
            


