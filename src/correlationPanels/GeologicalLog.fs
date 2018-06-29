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
      | ChangeXAxis               of SemanticId
      | LogNodeMessage            of (LogNodeId * LogNode.Action)
      | SelectLogNode             of LogNodeId


    module Helpers = 
      let calcXPosition (id : SemanticId) (nodes : plist<LogNode>) = 
        let nodes = nodes |> PList.map (fun n -> LogNode.update (LogNode.ChangeXAxis id) n) 
        let avg = nodes
                    |> PList.toList
                    |> List.filter (fun n -> n.size.X <> 0.0) 
                    |> List.map (fun (n : LogNode) -> n.size.X)
                    |> List.averageOrZero
        nodes |> PList.map (fun n -> LogNode.defaultIfZero n avg)

      let getMinLevel ( model : MGeologicalLog) = 
        model.nodes |> AList.toList
                    |> List.map (fun (n : MLogNode) -> Mod.force n.level)
                    |> List.min 

    let update (model : GeologicalLog) (action : Action) =
      match action with
        | ChangeXAxis id ->
            {model with nodes = Helpers.calcXPosition id model.nodes} 
        | CameraMessage m -> 
            {model with camera = ArcBallController.update model.camera m}
        | LogNodeMessage (id, m) -> 
            {model with nodes = model.nodes |> PList.map  (LogNode.update m)}
        | SelectLogNode n ->
            {model with nodes = model.nodes |> PList.map  (LogNode.update (LogNode.ToggleSelectNode n))}


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
        
    let rec calcLogPosChildren (height : float) (startAt : float) (plst : plist<LogNode>) =
      let lst = PList.toList plst
      match lst with 
        | [] -> PList.empty
        | lst ->
            let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
            let factor = height / accHeight
            let result = 
              lst
                |> List.map (fun (x : LogNode) -> 
                              {x with size = V2d.OO 
                                              + V2d.OI * (Rangef.calcRange x.range) * factor
                              }
                            )
                |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                                  {b with svgPos   = V2d(0.0, a.svgPos.Y + a.size.Y)}
                              ) {LogNode.initialEmpty with svgPos = V2d(0.0, startAt)}
                |> List.tail  
                |> List.map (fun (x : LogNode) ->
                              {x with children = calcLogPosChildren 
                                                  x.size.Y
                                                  x.svgPos.Y
                                                  x.children
                              }
              )
                |> PList.ofList
            result


    let calcsvgPosY (logHeight : float)  (plst : plist<LogNode>) =
      let lst = PList.toList plst
      match lst with 
        | [] -> PList.empty
        | lst ->

        let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
        let factor = logHeight / accHeight
        let result = 
          lst
            |> List.map (fun (x : LogNode) ->
                          {x with size = V2d.OO 
                                          + V2d.OI * (Rangef.calcRange x.range) * factor
                          }
                        )
            |> List.scan (fun (x : LogNode) (y : LogNode) -> 
                              {y with svgPos = V2d(y.svgPos.X, x.svgPos.Y + x.size.Y)}
                          ) (LogNode.initialEmpty) 
            |> List.tail  
            |> List.map (fun (x : LogNode) ->
                          {x with children = calcLogPosChildren 
                                              x.size.Y
                                              x.svgPos.Y
                                              x.children
                          }
                        )
            |> PList.ofList
        result

    let initial = {
      id            = LogId.invalid
      index         = 0
      isSelected    = false
      label         = "log"
      nodes         = PList.empty
      annoPoints    = []
      range         = Rangef.init
      camera        = 
        {ArcBallController.initial with 
          view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
      semanticApp   = SemanticApp.initial
      xAxis         = SemanticId.invalid
    }

    let generate     (index     : int)
                     (lst       : List<V3d * Annotation>) 
                     (annos     : plist<Annotation>) 
                     (semApp    : SemanticApp)
                     (xAxis     : SemanticId) 
                     (logHeight : float) : GeologicalLog = 
      let id = LogId.newId()
      let nodes0 = (generateLevel //TODO make more compact by removing debug stuff
                        id
                        lst 
                        annos
                        semApp 
                        (Border.initNegInf id)
                        (Border.initPosInf id)
                   )
      let nodes1 = 
        nodes0 |> PList.map LogNode.replaceInfinity
      let nodes2 = 
        nodes1 |> PList.filter LogNode.isInfinityType

      let nodes3 =
        nodes2 |> calcsvgPosY logHeight
               |> Helpers.calcXPosition xAxis

      nodes3 |> PList.map (fun (x : LogNode) -> LogNode.filterAndCollect x (fun x -> x.nodeType = LogNodeType.Hierarchical))
             |> List.concat
             |> List.map LogNode.Debug.print
      {
        id          = id
        index       =  index
        isSelected  = false
        label       = "log"
        nodes       = nodes3
        annoPoints  = lst
        range       = Rangef.init
        camera      = //TODO for 3D view
          {ArcBallController.initial with 
            view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
        semanticApp = semApp
        xAxis = xAxis
    }

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
                (viewType     : IMod<CorrelationPlotViewType>) 
                (secondaryLvl : IMod<int>)
                (styleFun     : float -> IMod<LogNodeStyle>) =
                
      let minLvl = Helpers.getMinLevel model
      let minLvlNodes =
        model.nodes
          |> AList.filter (fun n -> Mod.force n.level = minLvl)
          |> AList.toSeq // TODO check if OK
          |> Seq.sortByDescending (fun n -> Mod.force (LogNode.elevation' n))

      let nodeViews =
        alist {
          for n in minLvlNodes do
            let callback (args : list<string>) = 
              printf "%s" "foobar"
              SelectLogNode n.id
              // WIP!!!! 
            let mapper (a : DomNode<LogNode.Action>) =
              a |> UI.map (fun m -> LogNodeMessage (n.id, m))
            let! v = (LogNode.Svg.view n secondaryLvl viewType styleFun) 
                      //|> AList.map (UI.map LogNodeMessage)
            for it in v do
              //|> UI.map LogNodeMessage //
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

