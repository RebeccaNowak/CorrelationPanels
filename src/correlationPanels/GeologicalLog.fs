﻿namespace CorrelationDrawing

  module Log =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Rendering.Text
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.SceneGraph
    open Aardvark.Application
    open SimpleTypes
    open Svgplus.RS
    open Svgplus
    

    type Action =
      | SetState                  of State
      //| ChangeXAxis               of (AnnotationApp * SemanticId * float * SvgOptions)
      | LogNodeMessage            of (LogNodeId * LogNodes.Action)
      | SelectLogNode             of LogNodeId
      | UpdateYOffset             of float
      | TextInputMessage          of TextInput.Action
      //| SetVisibility             of bool
      //| MoveUp                    of LogId
      //| MoveDown                  of LogId

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
      let generateNonLevelNodes (logId : LogId) 
                                (annos : plist<Annotation>) 
                                (lp, la) (up, ua) 
                                (level : NodeLevel)
                                (semApp : SemanticApp) =                   
        annos 
          |> PList.map (fun a ->
              LogNodes.Init.fromSemanticType a semApp logId lp up level)

      let rec generateLevel (logId          : LogId)
                            (selectedPoints : hmap<AnnotationId, V3d>) 
                            (annoApp        : AnnotationApp)
                            (semApp         : SemanticApp) 
                            (lowerBorder    : Border) //TODO could pass point and anno
                            (upperBorder    : Border) =
      
        match selectedPoints.IsEmpty with
          | true -> plist.Empty
          | false ->
            let currentLevel =
              selectedPoints
                |> DS.HMap.keys
                |> List.map (fun id -> AnnotationApp.getLevel' annoApp semApp id)
                |> List.min

            let onlyCurrentLevel = 
              let filtered = 
                HMap.filter 
                  (fun id p -> ( AnnotationApp.getLevel' annoApp semApp id) = currentLevel)
                  selectedPoints
              filtered
                |> DS.HMap.toSwappedPairList
                |> List.map (fun ((p : V3d) ,id) -> 
                              (p, AnnotationApp.findAnnotation annoApp id))
                |> DS.PairList.filterNone
                |> List.sortBy (fun (p, k) -> (p.Length))
        
            let listWithBorders = 
              List.concat 
                    [
                      [(lowerBorder.point, AnnotationApp.annotationOrDefault annoApp lowerBorder.annotationId)]
                      onlyCurrentLevel
                      [(upperBorder.point, AnnotationApp.annotationOrDefault annoApp upperBorder.annotationId)]
                    ]
              |> List.sortBy (fun (p, a) -> (p.Length))
              
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
                  let ((lp,la),(up,ua)) = Annotation.sortByElevation (p1, a1) (p2, a2)
                  let nodeId = LogNodeId.newId()
                  let nodeChildren = 
                    let childrenAnnos = 
                      restAnnos // only take Annotations with elevations within the current node borders
                        |> HMap.filter (fun id a -> Annotation.isElevationBetween lp up a)
                    let childrenSelectedPoints = 
                      restSelPoints // only take selected points with elevations within the current node borders
                        |> HMap.filter (fun a p -> V3d.isElevationBetween p lp up)

                    match childrenSelectedPoints.IsEmptyOrNull () with
                      | true    -> generateNonLevelNodes logId (DS.HMap.toPList childrenAnnos) (lp, la) (up, ua) currentLevel semApp
                      | false   -> 
                        match childrenAnnos.IsEmptyOrNull () with
                          | true  -> PList.empty
                          | false -> 
                            let lBorder = Border.initial la.id lp nodeId logId
                            let uBorder = Border.initial ua.id up nodeId logId
                            generateLevel logId childrenSelectedPoints annoApp semApp lBorder uBorder 
                  yield LogNodes.Init.topLevelWithId nodeId logId
                          (up, ua.id) (lp, la.id) nodeChildren currentLevel        
              }

            nodesInCurrentLevel
              |> Seq.sortByDescending (fun x -> LogNodes.Helper.elevation x annoApp)
              |> PList.ofSeq
        

    let printDebug nodes = 
      nodes 
        |> PList.map (fun (x : LogNode) -> LogNodes.Recursive.filterAndCollect x (fun x -> x.nodeType = LogNodeType.Hierarchical))
        |> List.concat
        |> List.map LogNodes.Debug.print

    ///////////////////////////////////////////////////// GENERATE ///////////////////////////////////////////////////////////////////////
    let initial      (selectedPoints  : hmap<AnnotationId, V3d>) 
                     (semApp          : SemanticApp)
                     (annoApp         : AnnotationApp) = 

      let id = LogId.newId()
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

      let nodeToRectangle (n : LogNode) : Rectangle =
        let id = RectangleId.newId ()
        let width = 
          match LogNodes.Helper.calcMetricValue n annoApp with
            | Some d -> d
            | None   -> 50.0
        
        let height = (LogNodes.Helper.elevationRange n).range * 10.0
        
        {
          Rectangle.init id with 
            dim = {width = width; height = height}
            draw = true
        }

      let rectangles = 
        seq {
          for n in PList.toSeq nodes do
            yield nodeToRectangle n
        } |> List.ofSeq
        //nodes
        //  |> PList.map (fun n -> nodeToRectangle n)
      
      let rmap = 
        rectangles 
          |> List.map (fun r -> (r.id, r))
          |> HMap.ofList
          
      let order =
        rectangles 
          |> PList.ofList
          |> PList.map (fun r -> r.id)

      // let allNodes = LogNodes.Recursive.collectAll
      // WIP

      let stack = 
        RectangleStack.init (RectangleStackId.newId ()) rmap order 

      let log =
        {
          id           = id
          stackId      = stack.id
          state        = State.Display

          nodes        = nodes
          annoPoints   = selectedPoints    
      
        }

      (stack, log)
    ///////////////////////////////////////// SVG VIEW ///////////////////////////////////////////

    module View = 


////////////////////////////////////////////////////////////////////////

      open UIPlus

      let listView (model       : MGeologicalLog) 
                   (semApp      : MSemanticApp)
                   (annoApp     : MAnnotationApp)
                   (rowOnClick  : 'msg) 
                   (mapper      : Action -> 'msg)   =

        //let labelEditNode textInput = 
        //  (TextInput.view'' 
        //    "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
        //    textInput)

        //let moveUpDown =
        //  div [clazz "ui small vertical buttons"]
        //    [
        //      UIPlus.Buttons.iconButton' "angle up icon" "move up" 
        //                             (fun _ -> MoveUp model.id)  
        //                             (style "padding: 0px 2px 0px 2px")
        //      div[style "padding: 1px 0px 1px 0px"][]
        //      UIPlus.Buttons.iconButton' "angle down icon" "move down" 
        //                             (fun _ -> MoveDown model.id) 
        //                             (style "padding: 0px 2px 0px 2px")
        //    ]
        
        //let viewNew (model : MGeologicalLog) : list<DomNode<'msg>> =
        //    [
        //      [
        //        (labelEditNode model.label) 
        //          |> UI.map Action.TextInputMessage 
        //          |> UI.map mapper
        //          |> Table.intoTd
        //      ] |> Table.intoActiveTr rowOnClick      
        //    ]

        //let viewEdit (model : MGeologicalLog)  : list<DomNode<'msg>> =     
        //  let nodesRow =
        //    let viewFunction = 
        //      (LogNodes.Debug.view 
        //        semApp annoApp 
        //        LogNodeMessage
        //      ) >> AList.single
                
        //    let domNodes = 
        //      (LogNodes.Debug.viewAll' model.nodes (viewFunction))  
                        
        //    let mapped = domNodes |> (UI.map mapper)
        //    [mapped]
              

        //  [
        //    [
        //      (labelEditNode model.label) 
        //        |> UI.map Action.TextInputMessage 
        //        |> UI.map mapper
        //        |> Table.intoTd
        //      moveUpDown |> Table.intoTd |> UI.map mapper

        //    ] |> Table.intoActiveTr rowOnClick
        //    [
        //      (div [] nodesRow) |> Table.intoTd 
        //      //(div [] []) |> Table.intoTd |> UI.map mapper

        //    ] |> Table.intoActiveTr rowOnClick
        //  ]          

        //let viewDisplay (model : MGeologicalLog) : list<DomNode<'msg>>  =
        //  [
        //    [
        //      label [clazz "ui horizontal label"]
        //            [Incremental.text (model.label.text)] |> UI.map mapper
        //        |> Table.intoTd
        //      moveUpDown |> Table.intoTd |> UI.map mapper
        //    ] //@ [nodeViews] 
        //    |> Table.intoTrOnClick rowOnClick
            
        //  ] 

        model.state 
          |> Mod.map (fun state -> 
                        match state with
                          | State.Display  -> [] //viewDisplay model 
                          | State.Edit     -> [] //viewEdit model
                          | State.New      -> []//viewNew model 
                     ) 

///////////////////////////////////////////////////////////////////////////////////////////////////////////

      


      let dummy (model       : MGeologicalLog)  =
        let attributes = 
          amap {
             yield style "border: 2px solid black"
          }
          |> AttributeMap.ofAMap

        Incremental.div attributes AList.empty  



    
    let getLogConnectionSg //TODO connections wrong
          (model : MGeologicalLog) 
          (semanticApp : MSemanticApp) 
          (isSelected : bool) //TODO performance: IMod<bool>
          (camera : MCameraControllerState) =
            
      match isSelected with
        | false -> Sg.empty
        | true  ->
          let color = Mod.constant C4b.Yellow
          let width = Mod.constant 3.0 
          let lines =
            adaptive {
              let! aps = model.annoPoints.Content
              let points =  
                DS.HMap.toPList aps
                  |> PList.toList
                
              let head = points |> List.tryHead
              return match head with
                      | Some h ->  points
                                      |> List.sortBy (fun v -> v.Length) //TODO dirty hack. use click order
                                      |> List.pairwise
                                      |> List.map (fun (a,b) -> new Line3d(a, b))
                                      |> List.toArray
                      | None -> [||]
            }
        
          lines
              |> Sg.lines color
              |> Sg.noEvents
              |> Sg.effect [
                  toEffect DefaultSurfaces.trafo
                  toEffect DefaultSurfaces.vertexColor
                  toEffect DefaultSurfaces.thickLine                                
                  ] 
              |> Sg.noEvents
              |> Sg.uniform "LineWidth" width
              |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
              |> Sg.depthTest (Mod.constant DepthTestMode.None)


    let update (model : GeologicalLog) (action : Action) =
      match action with
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

    let init = 
      {
        id           = LogId.invalid
        stackId      = Svgplus.RS.RectangleStackId.invalid
        state        = State.Display

        nodes        = PList.empty
        annoPoints   = HMap.empty
      
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

