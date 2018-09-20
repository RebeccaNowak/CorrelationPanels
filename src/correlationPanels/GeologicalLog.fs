﻿namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module GeologicalLog =

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

    type Action =
      | SetState                  of State
      | CameraMessage             of ArcBallController.Message    
      | ChangeXAxis               of (SemanticId * float)
      | LogNodeMessage            of (LogNodeId * LogNodes.Action)
      | SelectLogNode             of LogNodeId
      | UpdateYOffset             of float
      | TextInputMessage          of TextInput.Action
      | SetVisibility             of bool
      | MoveUp                    of LogId
      | MoveDown                  of LogId

      
      

    module Helpers = 
      let getMinLevel ( model : MGeologicalLog) = 
        model.nodes |> AList.toList
                    |> List.map (fun (n : MLogNode) -> Mod.force n.level)
                    |> List.min 






    module Generate = 
      let generateNonLevelNodes (logId : LogId) (annos : plist<Annotation>) (lp, la) (up, ua) (semApp : SemanticApp) =                   
        annos 
          |> PList.map (fun a ->
              LogNodes.Init.fromSemanticType a semApp logId lp up)

      let rec generateLevel (logId          : LogId)
                            (selectedPoints : List<V3d * Annotation>) 
                            (annos          : plist<Annotation>) 
                            (semApp         : SemanticApp) 
                            (lowerBorder    : Border) //TODO could pass point and anno
                            (upperBorder    : Border) =
      
        match selectedPoints with
          | [] -> plist.Empty
          | lst ->
            let currentLevel =
              lst
                |> List.map (fun (p,a) -> Annotation.getLevel semApp a)
                |> List.min

            let onlyCurrentLevel = 
              lst 
                |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) = currentLevel)
                |> List.sortBy (fun (p, a) -> (p.Length))
        
            let listWithBorders = 
              List.concat 
                    [
                      [(lowerBorder.point, lowerBorder.anno)]
                      onlyCurrentLevel;
                      [(upperBorder.point, upperBorder.anno)]
                    ]
              |> List.sortBy (fun (p, a) -> (p.Length))

        
            let pairwiseWithBorders =
              listWithBorders
                |> List.pairwise

            let restAnnos =
              annos
                |> PList.filter (fun a -> (Annotation.getLevel semApp a) <> currentLevel)

            let restSelPoints =
              lst 
                |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) <> currentLevel)
       
            let nodesInCurrentLevel =
              seq {
                for ((p1, a1), (p2, a2)) in pairwiseWithBorders do
                  let ((lp,la),(up,ua)) = Annotation.sortByElevation (p1, a1) (p2, a2)
                  let nodeId = LogNodeId.newId()
                  let nodeChildren = 
                    let childrenAnnos = 
                      restAnnos // only take Annotations with elevations within the current node borders
                        |> PList.filter (Annotation.isElevationBetween lp.Length up.Length)
                    let childrenSelectedPoints = 
                      restSelPoints // only take selected points with elevations within the current node borders
                        |> List.filter (
                          fun (p, a) -> 
                            (lp.Length < p.Length && (up.Length > p.Length)))

                    match childrenSelectedPoints with
                      | []    -> generateNonLevelNodes logId childrenAnnos (lp, la) (up, ua) semApp
                      | cLst  -> 
                        match childrenAnnos with
                          | lst when lst.IsEmpty() -> PList.empty
                          | _ -> 
                            let lBorder = Border.initial la lp nodeId logId
                            let uBorder = Border.initial ua up nodeId logId
                            generateLevel logId cLst childrenAnnos semApp lBorder uBorder
                  yield LogNodes.Init.topLevelWithId nodeId logId
                          (up, ua) (lp, la) nodeChildren currentLevel        
              }

            nodesInCurrentLevel
              |> Seq.sortByDescending (fun x -> LogNodes.Helper.elevation x)
              |> PList.ofSeq
        


    let initial = {
      id            = LogId.invalid
      index         = 0
      state         = State.New
      isVisible       = true
      isSelected    = false
      label         = {TextInput.init with text = "log"}
      nodes         = PList.empty
      annoPoints    = []
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
                     (lst       : List<V3d * Annotation>) 
                     (annos     : plist<Annotation>) 
                     (semApp    : SemanticApp)
                     (xAxis     : SemanticId) 
                     (yOffset   : float)
                     (logHeight : float)
                     (xAxisScaleFactor : float)
                     (optMapper : option<float>) = 

      let id = LogId.newId()
      let nodes = (Generate.generateLevel //TODO make more compact by removing debug stuff
                        id
                        lst 
                        annos
                        semApp 
                        (Border.initNegInf id)
                        (Border.initPosInf id)
                   )
      let nodes = 
        nodes 
          |> LogNodes.Helper.replaceInfinity'
      //let nodes = 
      //  nodes |> PList.filter LogNode.isInfinityTypeLeaf
      
      let (nodes, yMapper) =
        nodes |> (LogNodes.Svg.yPosAndSize logHeight optMapper)
      
      let (nodes, svgMaxX) =
        nodes
           |> LogNodes.Svg.xPosAndSize xAxis xAxisScaleFactor

      let yRange : option<Rangef> = AnnotationPoint.tryCalcRange annos
      let yRange = 
        match yRange with
          | None ->
            printf "could not calculate log range" //TODO proper debug output
            Rangef.init
          | Some r -> r

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
          svgMaxX      = svgMaxX 
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
        (n.lBorder.id = borderId) || (n.uBorder.id = borderId)

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

      let svgView (model        : MGeologicalLog) 
                 // (viewType     : IMod<CorrelationPlotViewType>) 
                  (flags        : IMod<SvgFlags>)
                  (svgOptions   : MSvgOptions)
                  (secondaryLvl : IMod<int>)
                  (styleFun     : float -> IMod<LogAxisSection>) 
                   =
        let logNodeTo16Bins (node : MLogNode) (nrBins : int) =
          let aNodes = LogNodes.Recursive.angularChildren node
          adaptive {
            let! isEmpty = AList.isEmpty aNodes
            match isEmpty with
              | true -> return None
              | false -> 
                let angles =
                  aNodes
                    |> AList.map (fun n -> LogNodes.Helper.calcAngularValue' n)
                    |> AList.filterNone
                let! isEmpty = AList.isEmpty angles
                match isEmpty with 
                  | true -> return None
                  | false ->
                    let max = angles |> AList.maxBy (fun a -> a.radians)
                    let binNrs =
                      angles
                        |> AList.map (fun a -> Svg.Base.mapToBin a)
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


        let roseDiagrams = 
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
            if (not isEmpty) then
              for n in secondaryLvlNodes do
                let! pos = n.svgPos
                let! size = n.svgSize
                let shift = new V2d (size.X, size.Y * 0.5)
                let tmp = logNodeTo16Bins n 16
                let! tmp = tmp
                if tmp.IsSome then
                  let (countPerBin, nrCircles) = tmp.Value
                  let roseDiagram =
                    Svg.Base.drawRoseDiagram (pos + shift) 15.0 5.0 C4b.Black nrCircles 0.5 countPerBin
                  yield roseDiagram
          }

        let minLvl = Helpers.getMinLevel model
        let minLvlNodes = //TODO refactor
          model.nodes
            |> AList.filter (fun n -> Mod.force n.level = minLvl)
            |> AList.toSeq // TODO check if OK
            |> Seq.sortByDescending (fun n -> Mod.force (LogNodes.Helper.elevation' n))

        let nodeViews =
          alist {
            for n in minLvlNodes do
              let mapper (a : DomNode<LogNodes.Action>) =
                a |> UI.map (fun m -> LogNodeMessage (n.id, m))
              let! v = (LogNodes.Svg.view n secondaryLvl flags svgOptions styleFun) 
              for it in v do
                yield (mapper it)
          }
        AList.append nodeViews roseDiagrams

      open UI
      let listView (model : MGeologicalLog) 
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

        let indexNode =
            Incremental.text (Mod.map(fun x -> sprintf("%i") x) model.index) 
          

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
          [
            [
              (labelEditNode model.label) |> UI.map Action.TextInputMessage |> UI.map mapper
                |> Table.intoTd
              moveUpDown |> Table.intoTd  |> UI.map mapper

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


      let debug (model       : MGeologicalLog) =
        let minLvl = Helpers.getMinLevel model
        let minLvlNodes =
          model.nodes
            |> AList.filter (fun n -> Mod.force n.level = minLvl)
            |> AList.toSeq
            |> Seq.sortByDescending (fun n -> Mod.force (LogNodes.Helper.elevation' n))
     

        let nodeViews =
          alist {
            for n in minLvlNodes do
              yield 
                  Incremental.ul ([clazz  "ui inverted list"] |> AttributeMap.ofList) 
                            (alist {
                              let! v = (LogNodes.Debug.view n model.semanticApp)
                              for it in v do
                                yield it
                            })                      
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
              let! aps = model.annoPoints
              let points =  
                aps
                  |> List.map (fun (p, a) -> p)
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
        | ChangeXAxis (id, xAxisScaleFacor) ->
            let (nodes, svgMaxX) = LogNodes.Svg.xPosAndSize id xAxisScaleFacor model.nodes
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

