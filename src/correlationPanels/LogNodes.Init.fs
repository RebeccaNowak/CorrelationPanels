namespace CorrelationDrawing.LogNodes

open SimpleTypes

open CorrelationDrawing
      type Action =
      | ChangeXAxis       of (AnnotationApp * SemanticId * float)
      | MouseOver         of LogNodeId
      | ToggleSelectNode  of LogNodeId
      | BorderMessage     of Border.Action
      | DrawCorrelation   of BorderId

  module Init =
    open Aardvark.Base
    open Aardvark.UI

    let empty  : LogNode = {
      id           = LogNodeId.invalid
      logId        = LogId.invalid
      isSelected   = false
      hasDefaultX  = false
      nodeType     = LogNodeType.Empty
      label        = "log node"
      level        = NodeLevel.INVALID
      lBorder      = None
      uBorder      = None
      annotation   = None
      children     = plist.Empty
      //svgPos.Y      = 0.0
      //svgPos.X      = 0.0
      nativePos    = V2d.OO
      nativeSize   = Size2D.init
      svgPos       = V2d.OO
      svgSize      = Size2D.init

      mainBody     = None
      roseDiagram  = None
      buttonNorth  = None
      buttonSouth  = None

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

    let metric (logId : LogId)  (annoId : AnnotationId)  =
      let nodeId = LogNodeId.newId ()
      
      {empty with
        id         = nodeId
        logId      = logId
        nodeType   = LogNodeType.Metric
        annotation = Some annoId
      }


    let angular (logId : LogId) (annoId : AnnotationId) =
      let nodeId = LogNodeId.newId ()
      {empty with
        id         = nodeId
        logId      = logId
        nodeType   = LogNodeType.Angular
        annotation = Some annoId
      }

        //////////////////////////////////////////
    let fromSemanticType (a : Annotation) (semApp : SemanticApp) 
                         (logId : LogId) 
                         (lp : V3d) (up : V3d) =   
      let semType = Annotation.getType semApp a
      match semType with
        | SemanticType.Hierarchical -> hierarchicalLeaf logId a.id lp up
        | SemanticType.Angular -> angular logId a.id
        | SemanticType.Metric -> metric logId a.id
        | SemanticType.Undefined -> empty //TODO something useful
        | _ -> empty //TODO something useful
            


