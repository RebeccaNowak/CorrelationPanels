﻿namespace CorrelationDrawing

  module GeologicalLog =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open Aardvark.Application
    open SimpleTypes
    open Svgplus.RectangleStackTypes
    open Svgplus
    open Svgplus.RectangleType
    open Svgplus.DiagramItemType
    open UIPlus
   
    open CorrelationDrawing.LogTypes
    open CorrelationDrawing.Types
    open CorrelationDrawing.SemanticTypes
    open CorrelationDrawing.LogNodeTypes
    open CorrelationDrawing.AnnotationTypes

    
    type Action =
      | SetState                  of State
      | ToggleState                  
      | LogNodeMessage            of (LogNodeId * LogNodes.Action)
      | SelectLogNode             of LogNodeId
      | TextInputMessage          of (DiagramItemId * TextInput.Action)
      | MoveUp                    of RectangleStackId
      | MoveDown                  of RectangleStackId

    let headings =
      ["name";"move"]





    //[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    //module Action =
    // let unpack fromAction toAction f def =
    //    match fromAction, toAction with
          
         

    let findNode (model : GeologicalLog) (f : LogNode -> bool) =
      let nodeList = 
        model.nodes
          |> PList.map (LogNodes.Recursive.filterAndCollect f)
          |> DS.PList.flattenLists
      let node = List.tryHead nodeList
      node

    let findNodeFromRectangleId (model : GeologicalLog)
                                (rid   : RectangleId) =
       findNode model (fun (n : LogNode) -> n.rectangleId = rid)


    module Helpers = 
      let getMinLevel ( model : MGeologicalLog) = 
        adaptive {
          let! lst = model.nodes.Content

          let min =
            lst |> PList.toList
                |> List.map (fun (n : MLogNode) -> Mod.force n.level)
                |> List.min 

          return min
        }

      let getMinLevelNodes (model : MGeologicalLog) = //TODO this should be unnecessary as root nodes are min lvl
        alist {
          let! minLvl  = getMinLevel model
          for n in model.nodes do
            let! lvl = n.level
            if lvl = minLvl then yield n
        }

      let mapMinLevelNodes (model : MGeologicalLog) (f : MLogNode -> alist<'b>) = //TODO this should be unnecessary as root nodes are min lvl
        alist {
          let! minLvl  = getMinLevel model
          for n in model.nodes do
            let! lvl = n.level
            if lvl = minLvl then 
              yield! f n
        }

    module Generate = 
      let generateNonLevelNodes (logId : RectangleStackId) 
                                (annos : plist<Annotation>) 
                                ((lp, la) : (V3d * Annotation))
                                ((up, ua)  : (V3d * Annotation))
                                (level : NodeLevel)
                                (semApp : SemanticApp) =         
        let f (a : Annotation) =
          let inSel = a.id = la.id || a.id = ua.id
          match inSel with
            | true -> []
            | false -> [LogNodes.Init.fromSemanticType a semApp logId lp up level]

        annos 
          |> PList.map f
          |> DS.PList.flattenLists

      let getMinimumLevel (points : hmap<AnnotationId, V3d>) annoApp semApp =
        let lvl =
          points
            |> HMap.keys
            |> HSet.toList
            |> List.map (fun id -> AnnotationApp.getLevel' annoApp semApp id)
            |> List.min 
        lvl

      let filterAllButLevel points (level : NodeLevel) annoApp semApp =
        let filtered = 
          HMap.filter 
            (fun id p -> ( AnnotationApp.getLevel' annoApp semApp id) = level)
            points
        filtered
          |> DS.HMap.toSwappedPairList
          |> List.map (fun ((p : V3d) ,id) -> 
                        (p, AnnotationApp.findAnnotation annoApp id))
          |> DS.PairList.filterNone
          |> List.sortBy (fun (p, k) -> (k.elevation p))

       

      let rec generateLevel (logId          : RectangleStackId)
                            (selectedPoints : hmap<AnnotationId, V3d>) 
                            (annoApp        : AnnotationApp)
                            (semApp         : SemanticApp) 
                            (lowerBorder    : Border) //TODO could pass point and anno
                            (upperBorder    : Border) =
        ///// FUNCTION //////////
        let generateOneLogNodeAndItsChildren p1 a1 p2 a2 restAnnos restSelPoints currentLevel =
          let ((lp,la),(up,ua)) = Annotation.sortByElevation (p1, a1) (p2, a2)
          let nodeId = LogNodeId.newId()
          let nodeChildren = 
            let childrenAnnos = 
              restAnnos // only take Annotations with elevations within the current node borders
                |> HMap.filter (fun id a -> Annotation.isElevationBetween lp up a)
            let childrenSelectedPoints = 
              restSelPoints // only take selected points with elevations within the current node borders
                |> HMap.filter (fun a p -> Annotation.isElevationBetween' a1.elevation p lp up)
            let ncs = 
              match childrenSelectedPoints.IsEmptyOrNull () with
                | true    -> 
                  let children = generateNonLevelNodes logId (DS.HMap.toPList childrenAnnos) (lp, la) (up, ua) currentLevel semApp
                  children |> PList.ofList
                | false   -> 
                  match childrenAnnos.IsEmptyOrNull () with
                    | true  -> PList.empty
                    | false -> 
                      let lBorder = Border.initial la.id lp nodeId logId
                      let uBorder = Border.initial ua.id up nodeId logId
                      generateLevel logId childrenSelectedPoints annoApp semApp lBorder uBorder 
            ncs
          let thisLevelHierarchicalNodesWithChildren =
            LogNodes.Init.topLevelWithId nodeId logId
                  (up, ua.id) (lp, la.id) nodeChildren currentLevel 
          thisLevelHierarchicalNodesWithChildren

        /// START ///
        let thisLevel = 
          match selectedPoints.IsEmpty with
            | true -> plist.Empty
            | false ->
              let currentLevel = getMinimumLevel selectedPoints annoApp semApp
                //selectedPoints
                //  |> HMap.keys
                //  |> HSet.toList
                //  // |> DS.HMap.keys
                //  |> List.map (fun id -> AnnotationApp.getLevel' annoApp semApp id)
                //  |> List.min

              let onlyCurrentLevel = filterAllButLevel selectedPoints currentLevel annoApp semApp
                //let filtered = 
                //  HMap.filter 
                //    (fun id p -> ( AnnotationApp.getLevel' annoApp semApp id) = currentLevel)
                //    selectedPoints
                //filtered
                //  |> DS.HMap.toSwappedPairList
                //  |> List.map (fun ((p : V3d) ,id) -> 
                //                (p, AnnotationApp.findAnnotation annoApp id))
                //  |> DS.PairList.filterNone
                //  |> List.sortBy (fun (p, k) -> (k.elevation p))
        
              let listWithBorders = 
                List.concat 
                      [
                        [(lowerBorder.point, AnnotationApp.annotationOrDefault annoApp lowerBorder.annotationId)]
                        onlyCurrentLevel
                        [(upperBorder.point, AnnotationApp.annotationOrDefault annoApp upperBorder.annotationId)]
                      ]
                |> List.sortBy (fun (p, a) -> (a.elevation p))
              
              let pairwiseWithBorders =
                listWithBorders
                  |> List.pairwise

              let level a =
                (Annotation.getLevel semApp a)
              let restAnnos =
                annoApp.annotations
                  |> HMap.filter (fun k a -> (level a) <> currentLevel)

              let restSelPoints =
                selectedPoints 
                  |> HMap.filter (fun id p -> AnnotationApp.getLevel id annoApp semApp <> currentLevel)
       
              let nodesInCurrentLevel =
                seq {
                  for ((p1, a1), (p2, a2)) in pairwiseWithBorders do
                    let thisLevelHierarchicalNodesWithChildren =
                      generateOneLogNodeAndItsChildren p1 a1 p2 a2 restAnnos restSelPoints currentLevel
                    //let ((lp,la),(up,ua)) = Annotation.sortByElevation (p1, a1) (p2, a2)
                    //let nodeId = LogNodeId.newId()
                    //let nodeChildren = 
                    //  let childrenAnnos = 
                    //    restAnnos // only take Annotations with elevations within the current node borders
                    //      |> HMap.filter (fun id a -> Annotation.isElevationBetween lp up a)
                    //  let childrenSelectedPoints = 
                    //    restSelPoints // only take selected points with elevations within the current node borders
                    //      |> HMap.filter (fun a p -> Annotation.isElevationBetween' a1.elevation p lp up)
                    //  let ncs = 
                    //    match childrenSelectedPoints.IsEmptyOrNull () with
                    //      | true    -> 
                    //        let children = generateNonLevelNodes logId (DS.HMap.toPList childrenAnnos) (lp, la) (up, ua) currentLevel semApp
                    //        children |> PList.ofList
                    //      | false   -> 
                    //        match childrenAnnos.IsEmptyOrNull () with
                    //          | true  -> PList.empty
                    //          | false -> 
                    //            let lBorder = Border.initial la.id lp nodeId logId
                    //            let uBorder = Border.initial ua.id up nodeId logId
                    //            generateLevel logId childrenSelectedPoints annoApp semApp lBorder uBorder 
                    //  ncs
                    //let thisLevelHierarchicalNodesWithChildren =
                    //  LogNodes.Init.topLevelWithId nodeId logId
                    //        (up, ua.id) (lp, la.id) nodeChildren currentLevel        
                    yield thisLevelHierarchicalNodesWithChildren
                }

              nodesInCurrentLevel
                |> Seq.sortByDescending (fun x -> LogNodes.Helper.elevation x annoApp)
                |> PList.ofSeq
        thisLevel
        

    let printDebug nodes = 
      nodes 
        |> PList.map (fun (x : LogNode) -> 
                        LogNodes.Recursive.filterAndCollect (fun x -> 
                          x.nodeType = LogNodeType.Hierarchical) x)
        |> List.concat
        |> List.map LogNodes.Debug.print

    ///////////////////////////////////////////////////// GENERATE ///////////////////////////////////////////////////////////////////////
    let initial      (selectedPoints  : hmap<AnnotationId, V3d>) 
                     (semApp          : SemanticApp)
                     (annoApp         : AnnotationApp)
                     (xToSvg          : float -> float)
                     (yToSvg          : float)
                     (defaultWidth    : float)
                     (colourMap       : ColourMap) 
                     (elevationZeroHeight : float) = 
      let id = RectangleStackId.newId()
      let wInfNodes = (Generate.generateLevel //TODO make more compact by removing debug stuff
                        id
                        selectedPoints 
                        annoApp
                        semApp 
                        (Border.initNegInf id)
                        (Border.initPosInf id)
                      )
      let nodes = 
        LogNodes.Helper.replaceInfinity' wInfNodes annoApp

      let nodeToRectangle (n : LogNode) =
        let metricVal = LogNodes.Recursive.calcMetricValue n annoApp //TODO!!!!

        let (dotted, width, colour, overwriteColour) = 
          match metricVal  with
            | Some d -> 
              let width = xToSvg d
              let c = 
                let opt = ColourMap.svgValueToColourPicker colourMap width
                match opt with
                  | Some c -> c
                  | None -> {ColorPicker.init with c = C4b.Black}
              (false,width, c, None)
            | None   -> 
              let white : ColorInput = {c = C4b.White} 
              (true, defaultWidth, white, Some white.c)
        
        let dataRange = (LogNodes.Helper.elevationRange n annoApp)

        let dataHeight = dataRange.range
        let yAxisUpperBorder = sprintf "%.2f" (dataRange.max - elevationZeroHeight)
        
        let height = dataHeight * yToSvg
        let _height = //DEBUGGING
          match height with
            | height when height > 1000.0 ->
              1000.0
            | _ -> height
        let lowerBorderCol =
          let optCol = LogNodes.Helper.lowerBorderColour n annoApp semApp
          match optCol with
            | Some c -> c
            | None   -> C4b.Black
        let upperBorderCol =
          let optCol = LogNodes.Helper.upperBorderColour n annoApp semApp
          match optCol with
            | Some c -> c
            | None   -> C4b.Black
        let rectangle =
          {
            Rectangle.init n.rectangleId with 
              dim = {width = width; height = height}
              draw = true
              dottedBorder = dotted
              colour = colour
              overwriteColour = overwriteColour
              lowerBorderColour = lowerBorderCol
              upperBorderColour = upperBorderCol
              svgYAxisLabel = Svgplus.Text.init' yAxisUpperBorder
          }

        (rectangle, dataHeight)

      //let rectangles = //////////////////////// LOWEST LEVEL NODES!!!
      //  nodes
      //   |> PList.toList
      //   |> List.map (fun n -> nodeToRectangle n)
      //////////////// NOES TO LOG //////////////////////////////////
      let nodesToLog (nodes : list<LogNode>) (rmap : hmap<RectangleId, Rectangle>) =
        let _rids =
          nodes |> List.map (fun n -> n.rectangleId)
        let _rmap = 
          rmap |> HMap.filter (fun rid _ -> _rids |> List.contains rid)

        let order =
          nodes 
            |> List.map (fun n -> n.rectangleId)
         
        let zipped = List.zip order nodes
        let __nodes = zipped 
                      |> List.map (fun (r,n) -> {n with rectangleId = r})
                      |> PList.ofList

        let lowest =
          LogNodes.Helper.tryLowestBorder' __nodes annoApp
        let upmost =
          LogNodes.Helper.tryHighestBorder' __nodes annoApp
        let _dataRange =
          Option.map2 (fun l u -> {min = l; max = u}) lowest upmost

        let dataRange =
          match _dataRange with
          | None -> Rangef.init
          | Some r -> r

        let log = 
          RectangleStack.init id rmap (PList.ofList order) (fun x -> x * yToSvg) dataRange
        log
        //////////////// \NODES TO LOG //////////////////////////////////

      let rectangles = 
        nodes
          |> LogNodes.Recursive.collectAll
          |> List.filter (fun n -> n.nodeType = LogNodeType.Hierarchical)
          |> List.map (fun n -> nodeToRectangle n)
      let rmap = 
        rectangles 
          |> List.map (fun (r,h) -> (r.id, r))
          |> HMap.ofList          

      let primaryLog =
        let _nodes =
          nodes 
            |> LogNodes.Recursive.treeCutLowestLevel
            |> List.sortByDescending (fun n -> LogNodes.Helper.elevation n annoApp)
        (nodesToLog _nodes rmap)

      let secondaryLog =
        let _nodes =
          nodes 
            |> PList.toList
            |> List.sortByDescending (fun n -> LogNodes.Helper.elevation n annoApp)
        let stack = {(nodesToLog _nodes rmap) with id = RectangleStackId.newId ()}
        let _stack = RectangleStack.update stack (RectangleStack.FixWidthTo 50.0)
        let __stack = RectangleStack.update _stack (RectangleStack.SetDrawLabels true)
        RectangleStack.update __stack (RectangleStack.SetDrawButtons false)
        //TODO hardcoded width of secondary level

      let logList = [(primaryLog.id, primaryLog); (secondaryLog.id, secondaryLog)]
      let orderList = [secondaryLog.id;primaryLog.id]
      let item = (DiagramItem.init (HMap.ofList logList) (PList.ofList orderList))
      
      let ref : LogDiagramReferences =
        {
          itemId        = item.id
          mainLog       = primaryLog.id
          secondaryLog  = Some secondaryLog.id
        }

      let log =
        {
          id             = id
          diagramRef     = ref
          state          = State.Display

          //xToSvg         =  xToSvg      
          //yToSvg         =  yToSvg      
          defaultWidth   =  defaultWidth

          nodes          = nodes
          annoPoints     = selectedPoints    
      
        }

      (item, log)

    /////////////////////////////////////////// UPDATE /////////////////////////////////////////
    let update (model : GeologicalLog) (action : Action) =
      match action with
        | MoveDown id -> model
        | MoveUp id   -> model
        | TextInputMessage m -> model
        | LogNodeMessage (id, m) -> 
            {model with 
              nodes = 
                model.nodes 
                  |> LogNodes.Recursive.applyAll  
                      (fun n -> 
                        match n.id = id with
                          | true -> (LogNodes.Update.update m n) 
                          | false -> n
                      )
            }
        | SelectLogNode n ->
            {model with nodes = 
                          model.nodes 
                            |> LogNodes.Recursive.applyAll 
                                (LogNodes.Update.update (LogNodes.ToggleSelectNode n))
            }
        | SetState state      -> {model with state = state}
        | ToggleState         -> 
          let _state =
            match model.state with
              | State.New | State.Edit -> State.Display
              | State.Display -> State.Edit
          {model with state = _state}
    ///////////////////////////////////////// SVG VIEW ///////////////////////////////////////////

    module View = 
      let moveUpDown (model : MGeologicalLog) =
        div [clazz "ui small vertical buttons"]
          [
            UIPlus.Buttons.iconButtonNoTooltip 
                "angle up icon" 
                (fun _ -> MoveUp model.id)  
                (style "padding: 0px 2px 0px 2px")
            div[style "padding: 1px 0px 1px 0px"][]
            UIPlus.Buttons.iconButtonNoTooltip 
                "angle down icon" 
                (fun _ -> MoveDown model.id) 
                (style "padding: 0px 2px 0px 2px")
          ]

      let isSelected (model : MGeologicalLog) =
        model.state |> Mod.map (fun s -> s = State.Edit)

      let onSelect = Action.ToggleState

      let displayView (stack : MDiagramItem) (model : MGeologicalLog) =
        [
          label [clazz "ui horizontal label"]
              [Incremental.text (stack.header.label.textInput.text)]
          moveUpDown model
        ]

      let editView (stack : MDiagramItem) (model : MGeologicalLog) = 
        let labelEditNode textInput = 
          (TextInput.view'' 
            "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
            textInput)
        [
          (labelEditNode stack.header.label.textInput) 
            |> UI.map (fun m -> Action.TextInputMessage (model.diagramRef.itemId, m))
          moveUpDown model
        ]






             


        
        
////////////////////////////////////////////////////////////////////////

      //open UIPlus

      //let viewList (model       : MGeologicalLog) 
      //             (semApp      : MSemanticApp)
      //             (mapper      : Action -> 'msg)
      //             (stack       : MDiagramItem) =

      //  let labelEditNode textInput = 
      //    (TextInput.view'' 
      //      "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
      //      textInput)


        
      //  let viewNew  : list<DomNode<'msg>> =
      //      [
      //        [
      //          (labelEditNode stack.header.label.textInput) 
      //            |> UI.map (fun m -> Action.TextInputMessage (model.diagramRef.itemId, m))
      //            |> UI.map mapper
      //            |> Tables.intoTd
      //        ] |> Tables.intoTr //|> Table.intoActiveTr rowOnClick      
      //      ]

      //  //let viewEdit  : list<DomNode<'msg>> =     
      //  //  let nodesRow =
      //  //    let viewFunction = 
      //  //      //(LogNodes.Debug.view semApp annoApp LogNodeMessage
      //  //      (LogNodes.Debug.view 
      //  //          semApp  
      //  //          LogNodeMessage
      //  //      ) >> AList.single
                
      //  //    let domNodes = 
      //  //      (LogNodes.Debug.viewAll' model.nodes (viewFunction))  
                        
      //  //    let mapped = domNodes |> (UI.map mapper)
      //  //    [mapped]
              

      //  //  [
      //  //    [
      //  //      (labelEditNode stack.header.label.textInput) 
      //  //        |> UI.map (fun m -> Action.TextInputMessage (model.diagramRef.itemId, m))
      //  //        |> UI.map mapper
      //  //        |> Table.intoTd
      //  //      moveUpDown |> Table.intoTd |> UI.map mapper

      //  //    ] |> Table.intoTr//|> Table.intoActiveTr rowOnClick  
      //  //    //[
      //  //    //  (div [] nodesRow) |> Table.intoTd 
      //  //    //] |> Table.intoActiveTr rowOnClick
      //  //  ]          

      //  //let viewDisplay  : list<DomNode<'msg>>  =
      //  //  [
      //  //    [
      //  //      label [clazz "ui horizontal label"]
      //  //            [Incremental.text (stack.header.label.textInput.text)] ///|> UI.map mapper
      //  //        |> Tables.intoTd
      //  //      moveUpDown |> Tables.intoTd |> UI.map mapper
      //  //    ] //@ [nodeViews] 
      //  //    |> Table.intoTrOnClick rowOnClick  
      //  //    |> Tables.intoTr
            
      //  //  ] 

      //  //model.state 
      //  //  |> Mod.map (fun state -> 
      //  //                match state with
      //  //                  | State.Display  -> viewDisplay 
      //  //                  | State.Edit     -> viewNew 
      //  //                  | State.New      -> viewNew
      //  //             ) 

///////////////////////////////////////////////////////////////////////////////////////////////////////////

      


      let dummy (model       : MGeologicalLog)  =
        let attributes = 
          amap {
             yield style "border: 2px solid black"
          }
          |> AttributeMap.ofAMap

        Incremental.div attributes AList.empty  


    
    let logToRow actionMapping =
      (TableRow.init View.isSelected 
                     update
                     View.displayView
                     View.editView
                     Action.ToggleState
                     (fun model -> Alignment.CENTRE))
                     actionMapping


    let init = 
      {
        id              = RectangleStackId.invalid
        diagramRef      = {
                            itemId  = DiagramItemId.invalid
                            mainLog      = RectangleStackId.invalid
                            secondaryLog = None
                          }
        state           = State.Display
        //xToSvg          = (fun x -> x)
        //yToSvg          = 10.0
        defaultWidth    = 50.0
        nodes           = PList.empty
        annoPoints      = HMap.empty
      
      }

    let threads (model : GeologicalLog) =
      ThreadPool.empty

    let app : App<GeologicalLog, MGeologicalLog,Action> =
      {
          unpersist = Unpersist.instance
          threads = threads
          initial = init
          update = update
          view = View.dummy
      }

