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
      | LogNodeMessage            of LogNode.Action
      | SelectLogNode             of LogNodeId


     //let mapNodes (nodes : plist<LogNode>) (f : LogNode -> LogNode) =
     // nodes |> PList.map (fun n -> LogNode.apply n f)

    let calcXPosition (id : SemanticId) (nodes : plist<LogNode>) = 
      let nodes = nodes |> PList.map (fun n -> LogNode.update (LogNode.ChangeXAxis id) n) 
      let avg = nodes
                  |> PList.toList
                  |> List.filter (fun n -> n.size.X <> 0.0) 
                  |> List.map (fun (n : LogNode) -> n.size.X)
                  |> List.averageOrZero
      nodes |> PList.map (fun n -> LogNode.defaultIfZero n avg)
      //match lst.IsEmpty() with
      //  | true  -> PList.empty
      //  | false ->  
      
      //    let updNodes = lst
      //                    |> PList.map (fun n -> 
      //                      LogNode.update (LogNode.ChangeXAxis id) n)
      //    let onlyNonZero =
      //          updNodes
      //              |> PList.toList
      //              |> List.filter (fun n -> n.size.X = 0.0)
      //    let avg = match onlyNonZero with
      //                | []       -> 0.0
      //                | li       ->
      //                    li |> List.averageBy (fun n -> n.size.X)
      //    updNodes 
      //      |> PList.map (fun n ->  LogNode.defaultIfZero n avg)




    //let rec updateNode (n : LogNode) (m : LogNode.Action) : LogNode =
    //  match PList.count n.children with
    //                | 0     -> LogNode.update n m
    //                | other -> 
    //                    let c = n.children |> PList.map (fun (n : LogNode) -> updateNode n m)
    //                    LogNode.update {n with children = c} m


    let update (model : GeologicalLog) (action : Action) =
      match action with
        | ChangeXAxis id ->
            {model with nodes = calcXPosition id model.nodes} 
        | CameraMessage m -> 
            {model with camera = ArcBallController.update model.camera m}
        | LogNodeMessage m -> 
            {model with nodes = model.nodes |> PList.map  (LogNode.update m)}
        | SelectLogNode n ->
            {model with nodes = model.nodes |> PList.map  (LogNode.update (LogNode.ToggleSelectNode n))}

            
            
              


    let getSecond (_, s) = s

    //let copyAnnoWith (v : V3d) (aToAdd : (V3d * Annotation)) = 
    //  let a = 
    //    (fun (p,a) -> 
    //      (v, {a with points = PList.ofList [{AnnotationPoint.initial with point    = v
    //                                                                       selected = false
    //                                        }]
    //          }
    //      )
    //    ) aToAdd
    //  a

    let getMinLevel ( model : MGeologicalLog) =
      model.nodes |> AList.toList
        |> List.map (fun (n : MLogNode) -> Mod.force n.level)
        |> List.min 

    let generateNonLevelNodes (annos : plist<Annotation>) (lp, la) (up, ua) (semApp : SemanticApp) =                   
      annos 
        |> PList.map 
          (fun a -> 
            let lBorder = Border.initial a lp // TODO think of a better way for leafs
            let uBorder = Border.initial a up         
            let lnId : LogNodeId =  {id = System.Guid.NewGuid().ToString()}
            match (Annotation.getType semApp a) with
             | SemanticType.Hierarchical -> {LogNode.initialHierarchical lnId a lBorder uBorder with
                                              nodeType = LogNodeType.HierarchicalLeaf}
             | SemanticType.Angular -> (LogNode.intialAngular lnId a)
             | SemanticType.Metric -> (LogNode.intialMetric lnId a)
             | SemanticType.Undefined -> (LogNode.initialEmpty lnId) //TODO something useful
             | _ -> (LogNode.initialEmpty lnId) //TODO something useful
          )
       // |> PList.filter (fun n -> n.nodeType <> LogNodeType.Empty) //TODO why is this necessary

      
    let rec generateLevel (selectedPoints : List<V3d * Annotation>) 
                          (annos : plist<Annotation>) 
                          (semApp : SemanticApp) 
                          (lowerBorder : Border) 
                          (upperBorder : Border) =
      
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

          //let lBorder = {lowerBorder with anno = lowerBorder.anno} // do nothing !?
          //let uBorder = {upperBorder with anno = upperBorder.anno}
        
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
                    | []    -> generateNonLevelNodes childrenAnnos (lp, la) (up, ua) semApp
                    | cLst  -> 
                      match childrenAnnos with
                        | lst when lst.IsEmpty() -> PList.empty
                        | _ -> 
                          let lBorder = Border.initial la lp
                          let uBorder = Border.initial ua up
                          generateLevel cLst childrenAnnos semApp lBorder uBorder
                yield LogNode.initialTopLevel 
                        {id = System.Guid.NewGuid().ToString()} 
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
                              {x with size = V3d.OOI 
                                              + V3d.OIO * (Rangef.calcRange x.range) * factor
                              }
                            )
                |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                                  {b with logYPos = a.logYPos + a.size.Y
                                          logXPos = 0.0}
                              ) {LogNode.initialEmpty {id = System.Guid.NewGuid().ToString()} with logYPos = startAt}
                |> List.tail  
                |> List.map (fun (x : LogNode) ->
                              {x with children = calcLogPosChildren 
                                                  x.size.Y
                                                  x.logYPos
                                                  x.children
                              }
              )
                |> PList.ofList
            result


    let calcLogYPos (logHeight : float)  (plst : plist<LogNode>) =
      let lst = PList.toList plst
      match lst with 
        | [] -> PList.empty
        | lst ->

        let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
        let factor = logHeight / accHeight
        let result = 
          lst
            |> List.map (fun (x : LogNode) ->
                          {x with size = V3d.OOI 
                                          + V3d.OIO * (Rangef.calcRange x.range) * factor
                          }
                        )
            |> List.scan (fun (x : LogNode) (y : LogNode) -> 
                              {y with logYPos = x.logYPos + x.size.Y}
                          ) (LogNode.initialEmpty {id = System.Guid.NewGuid().ToString()}) 
            |> List.tail  
            |> List.map (fun (x : LogNode) ->
                          {x with children = calcLogPosChildren 
                                              x.size.Y
                                              x.logYPos
                                              x.children
                          }
                        )
            |> PList.ofList
        result

    let initial = {
      id            = LogId.invalid
      isSelected    = false
      label         = "log"
      nodes         = PList.empty
      annoPoints    = []
      range         = Rangef.init
      camera        = 
        {ArcBallController.initial with 
          view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
      semanticApp   = SemanticApp.initial
      xAxis = SemanticId.invalid
    }

    let generate     (id        : string)
                     (lst       : List<V3d * Annotation>) 
                     (annos     : plist<Annotation>) 
                     (semApp    : SemanticApp)
                     (xAxis     : SemanticId) 
                     (logHeight : float) : GeologicalLog = {
      id          = LogId.newId()
      isSelected    = false
      label       = "log"

      nodes       = (generateLevel 
                      lst 
                      annos
                      semApp 
                      Border.initNegInf
                      Border.initPosInf
                    ) |> PList.map LogNode.replaceInfinity
                      |> PList.filter LogNode.isInfinityType
                      |> calcLogYPos logHeight
                      |> calcXPosition xAxis

      annoPoints  = lst
      range       = Rangef.init
      camera      = 
        {ArcBallController.initial with 
          view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
      semanticApp = semApp
      xAxis = xAxis
    }


    let svgView (model        : MGeologicalLog) 
                (viewType     : CorrelationPlotViewType) 
                (secondaryLvl : IMod<int>)
                (styleFun     : float -> IMod<LogNodeStyle>) =
      let minLvl = getMinLevel model
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

            let! v = (LogNode.View.Svg.view n secondaryLvl viewType styleFun) 
                      //|> AList.map (UI.map LogNodeMessage)
            for it in v do
              yield it |> UI.map LogNodeMessage          
        }
      nodeViews  

    let view (model       : MGeologicalLog) =
            // (semanticApp : MSemanticApp) =
             //(isSelected  : bool) 
            // (xAxis       : SemanticId) 
           //  (styleFun    : float -> IMod<LogNodeStyle>) =
      let minLvl = getMinLevel model
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
                            let! v = (LogNode.View.Debug.debugView n model.semanticApp)
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

    //let start = App.start app
      

      //          yield (Incremental.tr 
  //            (AttributeMap.ofList []) //[st; onClick (fun str -> ToggleSelectLog (Some log.id))])
  //              children
  //            )

















  //      match isSelected with // TODO performance carry in Mod
  //        | _  -> clazz "ui celled striped inverted table"
  //        | false -> clazz "ui celled striped yellow table"
          //| true  -> style (sprintf "%s;%s;%s" UtilitiesGUI.tinyPadding (bgColorStr C4b.Yellow) "color:black")
          //| false -> style UtilitiesGUI.tinyPadding
               
      


  //  /// HELPER FUNCTIONS
  //
  //  let addNode (model : GeologicalLog) (anno : Annotation) (semanticApp : SemanticApp) =
  //    let glvl = Annotation.getLevel semanticApp
  //
  //    let newAnnoLst = model.annotations.Append(anno) |> PList.toList
  //
  //    // filter Annotations: only hierarchical
  //    let hier = Annotation.onlyHierarchicalAnnotations semanticApp newAnnoLst
  //    
  ////    let foo =
  ////      hier
  ////        |> Annotation.splitByLevel semanticApp
  //        //|> List.map (fun lst -> List.sortBy (fun (a : Annotation) -> Annotation.elevation a) lst)
  //    
  ////    let annosToNodes (lst : List<Annotation>) =
  ////       seq {
  ////          yield LogNode.initial Annotation.initialDummy lst.Head
  ////
  ////          for i in 1 .. (hier.Length - 1) do
  ////            yield LogNode.initial (hier.Item (i-1)) (hier.Item (i))
  ////        }
  //
  //    
  ////    let newNodes = 
  ////      hier
  ////        |> Annotation.onlyLvli semanticApp 0
  ////        |> annosToNodes
  //    
  ////    let split =
  ////      hier
  ////        |> Annotation.splitAtLvli semanticApp 0
  //
  //    
  //
  ////    let foo =
  ////      hier
  ////        |> List.sortBy (fun x -> Annotation.elevation x)
  //
  ////    let bar =
  ////      seq {
  ////        yield LogNode.initial Annotation.initialDummy hier.Head
  ////
  ////        for i in 1 .. (hier.Length - 1) do
  ////          if (glvl (hier.Item (i-1))) = (glvl (hier.Item (i))) then
  ////            yield (LogNode.initial (hier.Item (i-1)) (hier.Item (i)))
  ////          else if (glvl (hier.Item (i-1))) < (glvl (hier.Item (i))) then
  ////            // new Layer
  ////            yield (LogNode.initial (hier.Item (i-1)) (hier.Item (i)))
  ////
  ////      }
  //
  //          
  //      
  //
  //
  //
  ////    let newNode = LogNode.initial anno anno /////// 
  ////    let onlyHNodes = 
  ////      (model.nodes.Append newNode)
  ////        |> PList.toList
  ////        |> (LogNode.onlyHierarchicalNodes semanticApp)
  //
  //            
  //
  ////    let newNodes = 
  ////      onlyHNodes
  ////          |> List.sortBy (fun (x : LogNode) -> x.elevation)
  ////          |> PList.ofList
  ////          |> PList.mapiInt
  ////          |> PList.map (fun (x, i) -> {x with index = i}) 
  ////          |> PList.map LogNode.recalcRangeAndSize
  //        
  //    let nodesWithPositions = 
  ////      let lst = 
  ////        onlyHNodes
  ////          //|> PList.toList
  ////          |> List.sortBy (fun x -> x.elevation)
  //      newNodes  
  //        |> Seq.toList
  //        |> (LogNode.calcPos' 10.0)
  //        |> PList.ofList
  //    
  //    {model with nodes = nodesWithPositions}  
  //
  //    
  //
  
  //
  //
  //
  //
  //
  //  let inline (==>) a b = Attributes.attribute a b
  //
  //  let createAnnoBox (node : MLogNode) (colour : IMod<C4b>) =
  //    let foo = C4b(0.0, 0.0, 0.0, 0.5)
  //    let box =
  //        let c = colour |> Mod.map ( fun col -> C4b(col.R, col.G, col.B, foo.A)) 
  //        let b = Mod.map2 (fun pos size -> (Box3d.FromCenterAndSize(pos, size))) node.pos node.size
  //        let lbl = Mod.map2 (fun (x : V3d) (y : float) -> sprintf "%f / %f" x.Y y) node.size node.logYPos
  //        let lblSg = (UtilitiesRendering.makeLblSg' lbl node.pos)
  //
  //        (Sg.box c b)                   
  //          |> Sg.shader {
  //              do! DefaultSurfaces.trafo
  //              do! DefaultSurfaces.vertexColor
  //              do! DefaultSurfaces.simpleLighting
  //          } 
  //
  //      |> Sg.andAlso lblSg  
  //          //|> Sg.noEvents
  //          //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
  //          //|> Sg.billboard
  //      
  //
  //    box
  //
  //
  //
  //  let sg (model       : MGeologicalLog)
  //         (annos       : alist<MAnnotation>) 
  //         (semanticApp : MSemanticApp) 
  //         (camView     : IMod<CameraView>) = //(pos : IMod<V3d>) =
  //    if (Mod.force annos.Content).Count < 2 then
  //      Sg.empty
  //    else  
  //      alist {
  //        for node in model.nodes do
  //          yield createAnnoBox node (Annotation.getColor' node.lBoundary semanticApp) 
  //      }
  //      |> AList.toASet
  //      |> Sg.set
  //
  //
  //    
  //
  //
  //
  //
  //
  //
  //
  //  let view (runtime : IRuntime) (model : MGeologicalLog) =
  //
  //    let domNode =
  //      let attributes =
  //        AttributeMap.ofList [
  //              attribute "style" "width:100%; height: 100%;border: 1px solid black"
  //              clazz "svgRoot"; 
  //        ]
  //
  //      ArcBallController.controlledControl 
  //        model.camera
  //        CameraMessage 
  //        (Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0))
  //        (AttributeMap.ofList [
  //          attribute "style" "width:100%; height: 100%;border: 1px solid black"
  //          clazz "svgRoot"; 
  //        ])
  //        (
  //          //sg model.camera.view (Mod.constant V3d.OOO)
  //          renderLblTextureQuad runtime model.camera.view
  //        )
  //
  //    body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
  //      div [] [
  //        domNode
  //      ]
  //    ]
  //    
  //    ////////////
  //
  //    //            let heightList = 
  ////              annos 
  ////                |> AList.map (fun x -> Annotation.getRange x)
  ////                |> AList.toList
  ////                
  ////
  ////            let annoKeys =
  ////              annos 
  ////                |> AList.map (fun x -> x.id)
  ////                |> AList.toList
  ////            
  ////            let f (i : int) (value : IMod<float>) (valLst : List<IMod<float>>) =
  ////              value 
  ////                |> Mod.map (fun (x : float) ->
  ////                              match x with
  ////                                | h when i < 1 -> x
  ////                                | h when i >= 1 -> x + (Mod.force (valLst.Item (i - 1)))
  ////                                | _ -> 0.0)
  ////
  ////            let accHeightList = (heightList 
  ////                                  |> List.mapi (fun i x -> f i x heightList))
  ////
  ////
  ////            let heightMap = 
  ////              List.zip annoKeys accHeightList
  ////              |> AMap.ofList
  ////
  ////            //let minMax = Annotation.getMinMaxElevation annos
  ////
  ////
  ////            let annoSet = ASet.ofAList annos
  ////         
  //////
  ////            let sgSet = 
  ////              aset {
  ////                for anno in annoSet do
  ////                  let pos = AMap.find anno.id heightMap
  ////                  let tmp = (Annotation.getRange anno) |> Mod.map 
  ////                              (fun el -> (createAnnoBox anno semanticApp camView (pos |> Mod.force |> Mod.force)))
  ////                  yield tmp |> Sg.dynamic
  ////              }
  //
  ////            sgSet |> Sg.set
  //
  ////    (Mod.map sgSetLayers (AList.isEmpty annos))
  ////      |> Sg.dynamic
  //
  //
  //
  /////////////////////////
  //
  //
  ////  let createAnnoBox (anno : MAnnotation) 
  ////                    (semanticApp : MSemanticApp) 
  ////                    (view : IMod<CameraView>) 
  ////                    (pos : float) = 
  //////    let range =
  //////      (Annotation.getRange anno) 
  //////    let boxPos = V3d.IOI + (V3d.OIO * pos)
  //////    Sg.box
  //////      (Annotation.getColor' anno semanticApp)  
  //////      (range
  //////        |> (Mod.map 
  //////              (fun x -> 
  //////                (Box3d.FromCenterAndSize(boxPos, V3d.OIO * 2.0))
  //////              )
  //////            )
  //////      )
  ////    Sg.box (Annotation.getColor' anno semanticApp) 
  ////           (Mod.constant (Box3d.FromCenterAndSize(V3d.III, V3d.III * 5.0)))
  ////      |> Sg.shader {
  ////          do! DefaultSurfaces.trafo
  ////          do! DefaultSurfaces.vertexColor
  ////          do! DefaultSurfaces.simpleLighting
  ////      }         
  ////          //|> Sg.trafo scale
  ////      //|> Sg.trafo ( view |> Mod.map ( fun v -> Trafo3d.RotateInto(V3d.OOI, v.Sky) ) )
  ////      |> Sg.noEvents
  ////      //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
  ////      |> Sg.billboard

        //let heightNoBounds = 
        //  lst 
        //    |> List.tail
        //    |> List.truncate ((List.length lst) - 2)
        //    |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
        //let avgBounds = (heightNoBounds / (float) ((List.length lst) - 2)) * 0.5
        //let replacedBounds = 
        //  lst
        //    |> List.map
        //      (fun x ->
        //        match x.lBoundary.anno.semanticType, x.uBoundary.anno.semanticType with
        //          | SemanticType.Dummy, _ -> 
        //            {x with range = {
        //                              Rangef.init with min = x.uBoundary.point.Length - avgBounds
        //                                               max = x.uBoundary.point.Length
        //                            }
        //            }
        //          | _, SemanticType.Dummy ->
        //            {x with range = {
        //                              Rangef.init with min = x.lBoundary.point.Length
        //                                               max = x.lBoundary.point.Length + avgBounds
        //                            }
        //            }
        //          | _, _ -> x
        //      )