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

    type Action =
      | SetState                  of State
      | CameraMessage             of ArcBallController.Message    
      | ChangeXAxis               of (AnnotationApp * SemanticId * float * SvgOptions)
      | LogNodeMessage            of (LogNodeId * LogNodes.Action)
      | SelectLogNode             of LogNodeId
      | UpdateYOffset             of float
      | TextInputMessage          of TextInput.Action
      | SetVisibility             of bool
      | MoveUp                    of LogId
      | MoveDown                  of LogId

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
                                (semApp : SemanticApp) =                   
        annos 
          |> PList.map (fun a ->
              LogNodes.Init.fromSemanticType a semApp logId lp up)

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
                |> HMap.keys
                |> List.map (fun id -> AnnotationApp.getLevel' annoApp semApp id)
                |> List.min

            let onlyCurrentLevel = 
              let filtered = 
                HMap.filter 
                  (fun id p -> ( AnnotationApp.getLevel' annoApp semApp id) = currentLevel)
                  selectedPoints
              filtered
                |> HMap.toSwappedPairList
                |> List.map (fun ((p : V3d) ,id) -> 
                              (p, AnnotationApp.findAnnotation annoApp id))
                |> PairList.filterNone
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
                      | true    -> generateNonLevelNodes logId (HMap.toPList childrenAnnos) (lp, la) (up, ua) semApp
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
        
    let initial = {
      id            = LogId.invalid
      index         = 0
      state         = State.New
      isVisible       = true
      isSelected    = false
      label         = {TextInput.init with text = "log"}
      nodes         = PList.empty
      annoPoints    = hmap<AnnotationId, V3d>.Empty
      nativeYRange  = Rangef.init
      svgMaxX       = 0.0
      camera        = 
        {ArcBallController.initial with 
          view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
      semanticApp   = SemanticApp.initial
      xAxis         = SemanticId.invalid
      yOffset    = 0.0
    }

    let printDebug nodes = 
      nodes 
        |> PList.map (fun (x : LogNode) -> LogNodes.Recursive.filterAndCollect x (fun x -> x.nodeType = LogNodeType.Hierarchical))
        |> List.concat
        |> List.map LogNodes.Debug.print

    ///////////////////////////////////////////////////// GENERATE ///////////////////////////////////////////////////////////////////////
    let generate     (index     : int)
                     (lst       : hmap<AnnotationId, V3d>) 
                     (semApp    : SemanticApp)
                     (annoApp   : AnnotationApp)
                     (xAxis     : SemanticId) 
                     (yOffset   : float)
                     (logHeight : float)
                     (optMapper : option<float>) 
                     (opts       : SvgOptions) = 

      let id = LogId.newId()
      let nodes = (Generate.generateLevel //TODO make more compact by removing debug stuff
                        id
                        lst 
                        annoApp
                        semApp 
                        (Border.initNegInf id)
                        (Border.initPosInf id)
                   )
      let nodes = 
        LogNodes.Helper.replaceInfinity' nodes annoApp
      
      let (nodes, yMapper) =
        (LogNodes.Svg.Calc.yPosAndSize logHeight 0.0 optMapper nodes)
      
      let (nodes, svgMaxX) =
        LogNodes.Svg.Calc.xPosAndSize xAxis opts.xAxisScaleFactor nodes annoApp opts

      let yRange : option<Rangef> = AnnotationPoint.tryCalcRange (HMap.toPList annoApp.annotations)
      let yRange = 
        match yRange with
          | None ->
            printf "could not calculate log range" //TODO proper debug output
            Rangef.init
          | Some r -> r

      let xRange = (LogNodes.Recursive.xRange nodes)


      let newLog = 
        {
          id           = id
          index        = index
          state        = State.New
          isVisible    = true
          isSelected   = false
          label        = {TextInput.init with text =  "log"}
          nodes        = nodes
          annoPoints   = lst
          nativeYRange = yRange
          svgMaxX      = xRange //WIP 
          camera       = //TODO for 3D view
            {ArcBallController.initial with 
              view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
          semanticApp  = semApp
          xAxis        = xAxis
          yOffset      = yOffset  
        }
      (newLog, yMapper)

    let findNode (model : GeologicalLog) (nodeId : LogNodeId) =
      let filter (n : LogNode) = 
        (n.id = nodeId)
      let nodeIds = model.nodes
                          |> PList.toList
                          |> List.map (fun n -> (LogNodes.Recursive.filterAndCollect n filter))
      match nodeIds with
        | []       -> None
        | lst ->
            lst
              |> List.reduce (fun l1 l2 -> l1@l2)
              |> List.tryHead


    let findNode' (model : GeologicalLog) (borderId : BorderId) = //TODO make more generic!!
      let filter (n : LogNode) = 
        match n.lBorder, n.uBorder with
          | Some lb, Some ub ->
            (lb.id = borderId) || (ub.id = borderId)
          | _,_ -> false

      let nodeIds = model.nodes
                          |> PList.toList
                          |> List.map (fun n -> (LogNodes.Recursive.filterAndCollect n filter))
      match nodeIds with
        | []       -> None
        | lst ->
            lst
              |> List.reduce (fun l1 l2 -> l1@l2)
              |> List.tryHead



    ///////////////////////////////////////// SVG VIEW ///////////////////////////////////////////

    module View = 
      open Svg

      let createHeader (model   : MGeologicalLog) =
        () //TODO



      let createRoseDiagrams (model   : MGeologicalLog) 
                             (annoApp : MAnnotationApp) 
                             (flags   : IMod<SvgFlags>)
                             (svgOptions   : MSvgOptions)
                             (secondaryLvl : IMod<NodeLevel>) =
        let logNodeTo16Bins (node : MLogNode) (nrBins : int) =
          let aNodes = LogNodes.Recursive.angularChildren node
          adaptive {
            let! isEmpty = AList.isEmpty aNodes
            match isEmpty with
              | true -> return None
              | false -> 
                let angles =
                  aNodes
                    |> AList.map (fun n -> LogNodes.Helper.calcAngularValue' n annoApp)
                    |> AList.bindIMod
                    |> AList.filterNone
                let! isEmpty = AList.isEmpty angles
                match isEmpty with 
                  | true -> return None
                  | false ->
                    let max = angles |> AList.maxBy (fun (a : Math.Angle) -> a.radians)
                    let binNrs =
                      angles
                        |> AList.map (fun a -> Svgplus.Base.mapToBin a)
                    let content = binNrs.Content
                    let! binNrs = content
                    let countPerBin =
                      binNrs
                        |> PList.toList
                        |> List.sort
                        |> List.countBy (fun bin -> bin)
                    let (maxBin, max) = 
                      countPerBin 
                        |> List.maxBy (fun (bin, count) -> count)
                    return Some (countPerBin, max)
          }
          
        let calcRoseDiagramPosition (sLvlNode : MLogNode) 
                                    (radius : float) 
                                    (svgOpt : MSvgOptions) =
          let posX = 
            adaptive {
              let! maxX = model.svgMaxX
              let! offset = svgOpt.offset
              let! secLvl = svgOpt.secLevelWidth
              return maxX + offset.X + secLvl + radius * 3.0
            }
          let posY =
            Mod.map (fun (size : Size2D) -> size.Y * 0.5 + radius) sLvlNode.svgSize
          Mod.map2 (fun (x : float) (y : float) -> V2d(x,y)) posX posY


        let secondaryLvlNodes = 
          alist {
            let! flags = flags
            let flag = Flags.isSet SvgFlags.RadialDiagrams flags
            if flag then 
              let! lvl = secondaryLvl
              let lvlFilter =
                fun (n : MLogNode) -> (Mod.map (fun a -> a = lvl) n.level)
              for n in model.nodes do
                yield! LogNodes.Recursive.collectAndFilterAll' n lvlFilter
          }      

        alist {
          let! isEmpty = (AList.isEmpty secondaryLvlNodes)
          let radius = 15.0; //TODO put into svgOptions
          if (not isEmpty) then
            for n in secondaryLvlNodes do
              let! pos = calcRoseDiagramPosition n radius svgOptions
              let tmp = logNodeTo16Bins n 16
              let! tmp = tmp
              if tmp.IsSome then
                let (countPerBin, nrCircles) = tmp.Value
                failwith "TODO rose diagram!!!"
                //let roseDiagram =
                //  Svgplus.Base.drawRoseDiagram pos radius 5.0 
                //                           C4b.Black nrCircles 0.5 countPerBin
                //yield roseDiagram
          
        }



      let svgView (model        : MGeologicalLog) 
                  (annoApp      : MAnnotationApp)
                  (flags        : IMod<SvgFlags>)
                  (svgOptions   : MSvgOptions)
                  (secondaryLvl : IMod<NodeLevel>)
                  (styleFun     : float -> IMod<LogAxisSection>) 
                   =

        let nodeViewFunction (n : MLogNode) = 
          let mapper (a : DomNode<LogNodes.Action>) =
            a |> UI.map (fun m -> LogNodeMessage (n.id, m))
          let domNodes = 
            (LogNodes.Svg.view n secondaryLvl flags svgOptions styleFun annoApp) 
          domNodes |> AList.map (fun d -> mapper d)

        let nodeViews = 
          Helpers.mapMinLevelNodes model nodeViewFunction

        AList.append nodeViews (createRoseDiagrams model annoApp flags svgOptions secondaryLvl)

////////////////////////////////////////////////////////////////////////

      open UI
      let listView (model : MGeologicalLog) 
                   (semApp : MSemanticApp)
                   (rowOnClick : 'msg) 
                   (mapper : Action -> 'msg)   =
        let labelEditNode textInput = 
          (TextInput.view'' 
            "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
            textInput)

        let moveUpDown =
          div [clazz "ui small vertical buttons"]
            [
              UI.Buttons.iconButton' "angle up icon" "move up" 
                                     (fun _ -> MoveUp model.id)  
                                     (style "padding: 0px 2px 0px 2px")
              div[style "padding: 1px 0px 1px 0px"][]
              UI.Buttons.iconButton' "angle down icon" "move down" 
                                     (fun _ -> MoveDown model.id) 
                                     (style "padding: 0px 2px 0px 2px")
            ]
        
        let viewNew (model : MGeologicalLog) : list<DomNode<'msg>> =
            [
              [
                (labelEditNode model.label) 
                  |> UI.map Action.TextInputMessage 
                  |> UI.map mapper
                  |> Table.intoTd
                //moveUpDown |> Table.intoTd
              ] |> Table.intoActiveTr rowOnClick      
            ]

        let viewEdit (model : MGeologicalLog)  : list<DomNode<'msg>> =
          let nodes = 
            alist {
              for n in model.nodes do
                let lst = LogNodes.Debug.view n semApp
                yield! lst
            }
          [
            [
              (labelEditNode model.label) |> UI.map Action.TextInputMessage |> UI.map mapper
                |> Table.intoTd
              moveUpDown |> Table.intoTd  |> UI.map mapper

            ] |> Table.intoActiveTr rowOnClick
            [
              Incremental.div (AttributeMap.ofList []) nodes
            ] |> Table.intoActiveTr rowOnClick
          ]          

        let viewDisplay (model : MGeologicalLog)  : list<DomNode<'msg>>  =
          [
            [
              label [clazz "ui horizontal label"]
                    [Incremental.text (model.label.text)] |> UI.map mapper
                |> Table.intoTd
              moveUpDown |> Table.intoTd |> UI.map mapper
            ] |> Table.intoTrOnClick rowOnClick
          ] 

        model.state 
          |> Mod.map (fun state -> 
                        match state with
                          | State.Display  -> viewDisplay model // [td [] [text "foobar"]|> UI.map TextInputMessage] WORKS
                          | State.Edit     -> viewEdit model
                          | State.New      -> viewNew model //TODO probably not necessary
                     ) 

///////////////////////////////////////////////////////////////////////////////////////////////////////////




      let debug (model       : MGeologicalLog)  =
        let minLvlNodes = Helpers.getMinLevelNodes model

        let nodeViews =
          alist {
            for n in minLvlNodes do
              yield 
                  Incremental.ul ([clazz  "ui inverted list"] |> AttributeMap.ofList) 
                                 (LogNodes.Debug.view n model.semanticApp)
                     
          }

        let attributes = 
          amap {
            let! sel = model.isSelected
            match sel with
              | true  -> yield style "border: 2px solid orange"
              | false -> yield style "border: 2px solid black"
          }
          |> AttributeMap.ofAMap

        Incremental.div attributes nodeViews  



    
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
                HMap.toPList aps
                  |> PList.toList
                
              let head = points |> List.tryHead
              return match head with
                      | Some h ->  points
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
        | ChangeXAxis (annoApp, id, xAxisScaleFacor, opts) ->
            let (nodes, svgMaxX) = LogNodes.Svg.Calc.xPosAndSize id xAxisScaleFacor model.nodes annoApp opts
            {
              model with nodes    = nodes
                         svgMaxX  = svgMaxX
            } 
        | CameraMessage m -> 
            {model with camera = ArcBallController.update model.camera m}
        | LogNodeMessage (id, m) -> 
            {model with nodes = model.nodes |> PList.map  (LogNodes.Update.update m)}
        | SelectLogNode n ->
            {model with nodes = model.nodes |> PList.map  (LogNodes.Update.update (LogNodes.ToggleSelectNode n))}
        | UpdateYOffset offset ->
            {model with yOffset = offset}
        | TextInputMessage m -> 
            {model with label = TextInput.update model.label m}
        | SetVisibility b -> {model with isVisible = b}
        | MoveUp id   -> {model with index = model.index - 1}
        | MoveDown id -> {model with index = model.index + 1}
        | SetState state -> {model with state = state}

    let threads (model : GeologicalLog) =
      ThreadPool.empty

    let app : App<GeologicalLog, MGeologicalLog,Action> =
      {
          unpersist = Unpersist.instance
          threads = threads
          initial = initial
          update = update
          view = View.debug
      }

