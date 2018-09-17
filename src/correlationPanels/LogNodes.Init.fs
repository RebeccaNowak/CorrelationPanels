namespace CorrelationDrawing.LogNodes

open CorrelationDrawing
      type Action =
      | ChangeXAxis       of (SemanticId * float)
      | MouseOver         of LogNodeId
      | ToggleSelectNode  of LogNodeId
      | BorderMessage     of Border.Action
      | DrawCorrelation   of BorderId

  module Init =
    open Aardvark.Base
    open Aardvark.UI

    let empty  : LogNode = {
      id           = LogNodeId.invalid
      isSelected   = false
      hasDefaultX  = false
      nodeType     = LogNodeType.Empty
      label        = "log node"
      level        = -1
      lBorder      = Border.initialEmpty
      uBorder      = Border.initialEmpty
      children     = plist.Empty
      //svgPos.Y      = 0.0
      //svgPos.X      = 0.0
      nativePos    = V2d.OO
      nativeSize   = V2d.OO
      svgPos       = V2d.OO
      svgSize         = V2d.OO
    }


    let topLevel 
      (logId    : LogId)
      ((up, ua) : (V3d * Annotation)) 
      ((lp, la) : (V3d * Annotation)) 
      (children : plist<LogNode>)
      (level    : int) : LogNode = 
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
          nodeType    = nodeType
          label       = "log node"
          level       = level
          lBorder     = lBorder
          uBorder     = uBorder
          children    = children
      }

    let topLevelWithId
      (nodeId   : LogNodeId)
      (logId    : LogId)
      ((up, ua)  : (V3d * Annotation)) 
      ((lp, la) : (V3d * Annotation)) 
      (children : plist<LogNode>)
      (level    : int) : LogNode = 
      let lBorder = Border.initial la lp nodeId logId
      let uBorder = Border.initial ua up nodeId logId
      let n = topLevel logId (up, ua) (lp, la) children level
      {
        n with id       = nodeId
               lBorder  = lBorder
               uBorder  = uBorder
      }


    // TODO add level
    let hierarchicalLeaf 
      (logId    : LogId)
      (anno     : Annotation) 
      (lp       : V3d ) 
      (up       : V3d )  =
      let nodeId = LogNodeId.newId()
      {empty with
        id        = nodeId
        nodeType  = LogNodeType.HierarchicalLeaf
        lBorder   = Border.initial anno lp nodeId logId
        uBorder   = Border.initial anno up nodeId logId}

    let metric (logId : LogId)  (anno : Annotation)  =
      let nodeId = LogNodeId.newId()
      {empty with
        id         = nodeId
        nodeType   = LogNodeType.Metric
        lBorder    = Border.initial anno (Annotation.lowestPoint anno).point nodeId logId
        uBorder    = Border.initial anno (Annotation.highestPoint anno).point nodeId logId
      }


    let angular (logId : LogId) (anno : Annotation) =
      let nodeId = LogNodeId.newId()
      {empty with
        id         = nodeId
        nodeType   = LogNodeType.Angular
        lBorder    = Border.initial anno (Annotation.lowestPoint anno).point nodeId logId
        uBorder    = Border.initial anno (Annotation.highestPoint anno).point nodeId logId}

        //////////////////////////////////////////
    let fromSemanticType (a : Annotation) (semApp : SemanticApp) 
                         (logId : LogId) (lp : V3d) (up : V3d) =   
      match (Annotation.getType semApp a) with
        | SemanticType.Hierarchical -> hierarchicalLeaf logId a lp up
        | SemanticType.Angular -> angular logId a
        | SemanticType.Metric -> metric logId a
        | SemanticType.Undefined -> empty //TODO something useful
        | _ -> empty //TODO something useful
            


