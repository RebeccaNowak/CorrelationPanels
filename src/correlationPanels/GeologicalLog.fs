namespace CorrelationDrawing

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
    open UIPlus
    open Aardvark.Base.IL.Serializer
    
    type Action =
      | SetState                  of State
      | ToggleState                  
      | LogNodeMessage            of (LogNodeId * LogNodes.Action)
      | SelectLogNode             of LogNodeId
      | TextInputMessage          of (RectangleStackId * TextInput.Action)
      | MoveUp                    of RectangleStackId
      | MoveDown                  of RectangleStackId

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
                                (lp, la) (up, ua) 
                                (level : NodeLevel)
                                (semApp : SemanticApp) =                   
        annos 
          |> PList.map (fun a ->
              LogNodes.Init.fromSemanticType a semApp logId lp up level)

      let rec generateLevel (logId          : RectangleStackId)
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
                     (annoApp         : AnnotationApp)
                     (xToSvg          : float)
                     (yToSvg          : float)
                     (defaultWidth    : float)
                     (colourMap       : ColourMap) : (RectangleStack * GeologicalLog) = 

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
        let metricVal = LogNodes.Recursive.calcMetricValue n annoApp


          
        let id = RectangleId.newId ()
        let (dotted, width) = 
          match metricVal  with
            | Some d -> 
              (false, d * xToSvg)
            | None   -> (true, defaultWidth)
        
        let colour = 
          let opt = ColourMap.valueToColourPicker' colourMap width
          match opt with
            | Some c -> c
            | None -> {ColorPicker.init with c = C4b.White}

        let height = (LogNodes.Helper.elevationRange n).range * yToSvg
        let rectangle =
          {
            Rectangle.init id with 
              dim = {width = width; height = height}
              draw = true
              dottedBorder = dotted
              colour = colour
          }

        rectangle

      let rectangles = 
        nodes
         |> PList.toList
         |> List.map (fun n -> nodeToRectangle n)
      
      let rmap = 
        rectangles 
          |> List.map (fun r -> (r.id, r))
          |> HMap.ofList
          
      let order =
        rectangles 
          |> List.map (fun r -> r.id)

      // let allNodes = LogNodes.Recursive.collectAll
      // WIP
      let zipped = List.zip order (PList.toList nodes)
      let _nodes = zipped 
                    |> List.map (fun (r,n) -> {n with rectangleId = r})
                    |> PList.ofList

      let stack = 
        RectangleStack.init id rmap (PList.ofList order)

      let log =
        {
          id             = id
          state          = State.Display

          xToSvg         =  xToSvg      
          yToSvg         =  yToSvg      
          defaultWidth   =  defaultWidth

          nodes          = _nodes
          annoPoints     = selectedPoints    
      
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
                   (mapper      : Action -> 'msg)
                   (stack       : MRectangleStack) =

        let labelEditNode textInput = 
          (TextInput.view'' 
            "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
            textInput)

        let moveUpDown =
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
        
        let viewNew  : list<DomNode<'msg>> =
            [
              [
                (labelEditNode stack.header.label) 
                  |> UI.map (fun m -> Action.TextInputMessage (stack.id, m))
                  |> UI.map mapper
                  |> Table.intoTd
              ] |> Table.intoTr // |> Table.intoActiveTr rowOnClick      
            ]

        let viewEdit  : list<DomNode<'msg>> =     
          let nodesRow =
            let viewFunction = 
              (LogNodes.Debug.view 
                semApp annoApp 
                LogNodeMessage
              ) >> AList.single
                
            let domNodes = 
              (LogNodes.Debug.viewAll' model.nodes (viewFunction))  
                        
            let mapped = domNodes |> (UI.map mapper)
            [mapped]
              

          [
            [
              (labelEditNode stack.header.label) 
                |> UI.map (fun m -> Action.TextInputMessage (stack.id, m))
                |> UI.map mapper
                |> Table.intoTd
              moveUpDown |> Table.intoTd |> UI.map mapper

            ] |> Table.intoTr//|> Table.intoActiveTr rowOnClick  
            //[
            //  (div [] nodesRow) |> Table.intoTd 
            //] |> Table.intoActiveTr rowOnClick
          ]          

        let viewDisplay  : list<DomNode<'msg>>  =
          [
            [
              label [clazz "ui horizontal label"]
                    [Incremental.text (stack.header.label.text)] ///|> UI.map mapper
                |> Table.intoTd
              moveUpDown |> Table.intoTd |> UI.map mapper
            ] //@ [nodeViews] 
           // |> Table.intoTrOnClick rowOnClick  
            |> Table.intoTr
            
          ] 

        model.state 
          |> Mod.map (fun state -> 
                        match state with
                          | State.Display  -> viewDisplay 
                          | State.Edit     -> viewEdit 
                          | State.New      -> viewNew 
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


    let init = 
      {
        id              = RectangleStackId.invalid
        state           = State.Display
        xToSvg          = 10.0
        yToSvg          = 10.0
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

