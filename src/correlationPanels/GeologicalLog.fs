namespace CorrelationDrawing

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
      | CameraMessage             of ArcBallController.Message    
      | ChangeXAxis               of (SemanticId * float)
      | LogNodeMessage            of (LogNodeId * LogNode.Action)
      | SelectLogNode             of LogNodeId
      | UpdateYOffset             of float
      

    module Helpers = 
      let getMinLevel ( model : MGeologicalLog) = 
        model.nodes |> AList.toList
                    |> List.map (fun (n : MLogNode) -> Mod.force n.level)
                    |> List.min 


    module Calculate = 
      let svgXPosAndSize (id : SemanticId) (xAxisScaleFactor : float) (nodes : plist<LogNode>)  : (plist<LogNode> * float) =
        let updNodes = 
          nodes 
            |> PList.map (fun n -> LogNode.update (LogNode.ChangeXAxis (id, xAxisScaleFactor)) n) 

        // nodes without grainsize/Annotations of x-Axis semantic type get avg
        let size = updNodes
                    |> PList.toList
                    |> List.filter (fun n -> n.svgSize.X <> 0.0) 
                    |> List.map (fun (n : LogNode) -> n.svgSize.X)
        if size.Length = 0 then printfn "%s" "calc svg x position/size failed" //TODO if list empty //TODO bug/refactor
        //let avg = size |> List.averageOrZero
        let avg = 
           match (size |> List.averageOrZero) with
            | n when n = 0.0 -> 10.0 //TODO hardcoded size if no grain size annotations in log
            | n -> n

        let updNodes =
          updNodes |> PList.map (fun n -> LogNode.defaultIfZero n avg)
        (updNodes, avg)



      let svgYPosAndSize (logHeight : float) (optMapper : option<float>) (plst : plist<LogNode>) =

        let rec calcRecYPosSize (height : float) (startAt : float) (plst : plist<LogNode>) =
          let lst = PList.toList plst
          match lst with 
            | [] -> PList.empty
            | lst ->
                let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRangeNoInf x.range)))
                let factor = height / accHeight
                let result = 
                  lst
                    |> List.map (fun (x : LogNode) -> 
                                  {x with svgSize = V2d.OO 
                                                  + V2d.OI * (Rangef.calcRangeNoInf x.range) * factor
                                  }
                                )
                    |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                                      {b with svgPos   = V2d(0.0, a.svgPos.Y + a.svgSize.Y)}
                                  ) {LogNode.initialEmpty with svgPos = V2d(0.0, startAt)}
                    |> List.tail  
                    |> List.map (fun (x : LogNode) ->
                                  {x with children = calcRecYPosSize 
                                                      x.svgSize.Y
                                                      x.svgPos.Y
                                                      x.children
                                  }
                  )
                    |> PList.ofList
                result




        let lst = PList.toList plst
        match lst with 
          | [] -> (PList.empty, 1.0)
          | lst ->

          let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRangeNoInf x.range)))
          let factor = 
            match optMapper with
              | None -> logHeight / accHeight
              | Some m -> m
          let result = 
            lst
              |> List.map (fun (n : LogNode) ->
                            {n with svgSize = V2d.OO 
                                               + V2d.OI
                                               * (Rangef.calcRangeNoInf n.range) 
                                               * factor
                                    nativeSize = V2d.OO 
                                                  + V2d.OI 
                                                  * (Rangef.calcRangeNoInf n.range) 
                                                  
                            }
                          )
              |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                                {b with svgPos    = V2d(b.svgPos.X, a.svgPos.Y + a.svgSize.Y)
                                        nativePos = V2d(b.nativePos.X, a.nativePos.Y + a.nativeSize.Y)
                                }
                            ) (LogNode.initialEmpty) 
              |> List.tail  
              |> List.map (fun (x : LogNode) ->
                            {x with children = calcRecYPosSize 
                                                x.svgSize.Y
                                                x.svgPos.Y
                                                x.children
                            }
                          )
              |> PList.ofList
          (result, factor)


    module Generate = 
      let generateNonLevelNodes (logId : LogId) (annos : plist<Annotation>) (lp, la) (up, ua) (semApp : SemanticApp) =                   
        annos 
          |> PList.map 
            (fun a ->     
              let lnId : LogNodeId =  {id = System.Guid.NewGuid().ToString()}
              match (Annotation.getType semApp a) with
               | SemanticType.Hierarchical -> LogNode.initialHierarchicalLeaf logId a lp up
               | SemanticType.Angular -> LogNode.intialAngular logId a
               | SemanticType.Metric -> LogNode.intialMetric logId a
               | SemanticType.Undefined -> LogNode.initialEmpty //TODO something useful
               | _ -> LogNode.initialEmpty //TODO something useful
            )

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
                  yield LogNode.initialTLWithId nodeId logId
                          (up, ua) (lp, la) nodeChildren currentLevel        
              }

            nodesInCurrentLevel
              |> Seq.sortByDescending (fun x -> LogNode.elevation x)
              |> PList.ofSeq
        


    let initial = {
      id            = LogId.invalid
      index         = 0
      isSelected    = false
      label         = "log"
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
        |> PList.map (fun (x : LogNode) -> LogNode.filterAndCollect x (fun x -> x.nodeType = LogNodeType.Hierarchical))
        |> List.concat
        |> List.map LogNode.Debug.print

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
          |> LogNode.replaceInfinity'
      //let nodes = 
      //  nodes |> PList.filter LogNode.isInfinityTypeLeaf
      
      let (nodes, yMapper) =
        nodes |> (Calculate.svgYPosAndSize logHeight optMapper)
      
      let (nodes, svgMaxX) =
        nodes
           |> Calculate.svgXPosAndSize xAxis xAxisScaleFactor

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
          isSelected   = false
          label        = "log"
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
                          |> List.map (fun n -> (LogNode.filterAndCollect n filter))
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
                          |> List.map (fun n -> (LogNode.filterAndCollect n filter))
      match nodeIds with
        | []       -> None
        | lst ->
            lst
              |> List.reduce (fun l1 l2 -> l1@l2)
              |> List.tryHead

    let svgView (model        : MGeologicalLog) 
               // (viewType     : IMod<CorrelationPlotViewType>) 
                (flags        : IMod<SvgFlags>)
                (svgOptions   : SvgOptions)
                (secondaryLvl : IMod<int>)
                (styleFun     : float -> IMod<LogAxisSection>) 
                 =
                
      let minLvl = Helpers.getMinLevel model
      let minLvlNodes =
        model.nodes
          |> AList.filter (fun n -> Mod.force n.level = minLvl)
          |> AList.toSeq // TODO check if OK
          |> Seq.sortByDescending (fun n -> Mod.force (LogNode.elevation' n))

      let nodeViews =
        alist {
          for n in minLvlNodes do
            let mapper (a : DomNode<LogNode.Action>) =
              a |> UI.map (fun m -> LogNodeMessage (n.id, m))
            let! v = (LogNode.Svg.view n secondaryLvl flags svgOptions styleFun) 
                      //|> AList.map (UI.map LogNodeMessage)
            for it in v do
              yield (mapper it)
        }
      nodeViews  

    let view (model       : MGeologicalLog) =
      let minLvl = Helpers.getMinLevel model
      let minLvlNodes =
        model.nodes
          |> AList.filter (fun n -> Mod.force n.level = minLvl)
          |> AList.toSeq
          |> Seq.sortByDescending (fun n -> Mod.force (LogNode.elevation' n))
     

      let nodeViews =
        alist {
          for n in minLvlNodes do
            yield 
                Incremental.ul ([clazz  "ui inverted list"] |> AttributeMap.ofList) 
                          (alist {
                            let! v = (LogNode.Debug.debugView n model.semanticApp)
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
          let width = Mod.constant 1.0 
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
            let (nodes, svgMaxX) = Calculate.svgXPosAndSize id xAxisScaleFacor model.nodes
            {
              model with nodes    = nodes
                         svgMaxX  = svgMaxX
            } 
        | CameraMessage m -> 
            {model with camera = ArcBallController.update model.camera m}
        | LogNodeMessage (id, m) -> 
            {model with nodes = model.nodes |> PList.map  (LogNode.update m)}
        | SelectLogNode n ->
            {model with nodes = model.nodes |> PList.map  (LogNode.update (LogNode.ToggleSelectNode n))}
        | UpdateYOffset offset ->
            {model with yOffset = offset}

    let threads (model : GeologicalLog) =
      ThreadPool.empty

    let app : App<GeologicalLog, MGeologicalLog,Action> =
      {
          unpersist = Unpersist.instance
          threads = threads
          initial = initial
          update = update
          view = view
      }

