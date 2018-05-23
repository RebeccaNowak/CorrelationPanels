namespace CorrelationDrawing
//
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GeologicalLog =

  open System
  open Aardvark.Base
  open Aardvark.Base.Rendering
  open Aardvark.Rendering.Text
  open Aardvark.Base.Incremental
  open Aardvark.Base.Incremental.Operators
  open Aardvark.SceneGraph.SgPrimitives
  open Aardvark.SceneGraph.FShadeSceneGraph
  open Aardvark.UI
  open Aardvark.UI.Primitives
//  open Aardvark.UI.Extensions.Sg
  open Aardvark.SceneGraph
  open UtilitiesRendering
  open Aardvark.Application
  open UtilitiesGUI

  let getSecond (_, s) = s

  

  let copyAnnoWith (v : V3d) (aToAdd : (V3d * Annotation)) = 
    let a = 
      (fun (p,a) -> 
        (v, {a with points = PList.ofList [{AnnotationPoint.initial with point    = v
                                                                         selected = false
                                          }]
            }
        )
      ) aToAdd
    a

  let getMinLevel ( model : MGeologicalLog) =
    model.nodes |> AList.toList
      |> List.map (fun (n : MLogNode) -> Mod.force n.level)
      |> List.min 

  let generateNonLevelNodes (annos : plist<Annotation>) (lp, la) (up, ua) (semApp : SemanticApp) =                   
    annos 
      |> PList.map 
        (fun a -> 
          let lBorder = {Border.initial with anno  = a
                                             point = lp} // TODO think of a better way
          let uBorder = {Border.initial with anno  = a
                                             point = up}         
          
          match (Annotation.getType semApp a) with
           | SemanticType.Hierarchical -> (LogNode.initialHierarchical a lBorder uBorder) //happens if not selected
           | SemanticType.Angular -> (LogNode.intialAngular a lBorder uBorder)
           | SemanticType.Metric -> (LogNode.intialMetric a  lBorder uBorder)
           | SemanticType.Undefined -> (LogNode.initialEmpty) //TODO something useful
           | _ -> (LogNode.initialEmpty) //TODO something useful
        )
      |> PList.filter (fun n -> n.nodeType <> LogNodeType.Empty)

      
  let rec generateLevel (lst : List<V3d * Annotation>) 
                        (annos : plist<Annotation>) 
                        (semApp : SemanticApp) 
                        (lowerBorder : Border) 
                        (upperBorder : Border) =
    match lst with
      | [] -> plist.Empty
      | lst ->
        let currentLevel =
          lst
            |> List.map (fun (p,a) -> Annotation.getLevel semApp a)
            |> List.min

        let filteredList = 
          lst 
            |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) = currentLevel)
            |> List.sortBy (fun (p, a) -> (p.Length))

        let lBorder = {lowerBorder with anno = {lowerBorder.anno with overrideLevel = Some currentLevel}}
        let uBorder = {upperBorder with anno = {upperBorder.anno with overrideLevel = Some currentLevel}}
        
        let listWithBorders = 
          List.concat 
                [
                  [(lBorder.point, lBorder.anno)]
                  filteredList;
                  [(uBorder.point, uBorder.anno)]
                ]
          |> List.sortBy (fun (p, a) -> (p.Length))

        
        let pwList =
          listWithBorders
            |> List.pairwise

        let restAnnos =
          annos
            |> PList.filter (fun a -> (Annotation.getLevel semApp a) <> currentLevel)

        let restSelPoints =
          lst 
            |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) <> currentLevel)
        
        let foo =
          seq {
            for ((p1, a1), (p2, a2)) in pwList do
              // sort pairs
              let (lp, la) = if p1.Length < p2.Length then (p1, a1) else (p2, a2) //TODO refactor
              let (up, ua) = if p1.Length < p2.Length then (p2, a2) else (p1, a1)
              let layerChildren = 
                let between (a : Annotation) = (lp.Length < (Annotation.elevation a) && (up.Length > (Annotation.elevation a)))
                let layerRestAnnos = 
                  restAnnos
                    |> PList.filter between
                let layerRestSelPoints = 
                  restSelPoints
                    |> List.filter (
                      fun (p, a) -> 
                        (lp.Length < p.Length && (up.Length > p.Length)))

                match layerRestSelPoints with
                  | []    -> generateNonLevelNodes layerRestAnnos (lp, la) (up, ua) semApp
                  | cLst  -> 

                    let lBorder = {Border.initial with anno  = la
                                                       point = lp}
                    let uBorder = {Border.initial with anno  = ua
                                                       point = up} 
                    generateLevel layerRestSelPoints layerRestAnnos semApp lBorder uBorder

              yield LogNode.initialTopLevel (up, ua) (lp, la) layerChildren currentLevel

              
          }

        foo
          |> Seq.sortByDescending (fun x -> x.elevation)
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
                                            + V3d.IOO * 20.0 // * (float) (x.level + 1) //TODO hardcoded stuff //Performance
                                    
                            }
                          )
              |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                                {b with logYPos = a.logYPos + a.size.Y
                                        logXPos = 0.0}
                                        //logXPos = a.logXPos + a.size.X}
                            ) {LogNode.initialEmpty with logYPos = startAt}
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
      let heightNoBounds = 
        lst 
          |> List.tail
          |> List.truncate ((List.length lst) - 2)
          |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
      let avgBounds = (heightNoBounds / (float) ((List.length lst) - 2)) * 0.5
      let replacedBounds = 
        lst
          |> List.map
            (fun x ->
              match x.lBoundary.anno.semanticType, x.uBoundary.anno.semanticType with
                | SemanticType.Dummy, _ -> 
                  {x with range = {
                                    Rangef.init with min = x.uBoundary.point.Length - avgBounds
                                                     max = x.uBoundary.point.Length
                                  }
                  }
                | _, SemanticType.Dummy ->
                  {x with range = {
                                    Rangef.init with min = x.lBoundary.point.Length
                                                     max = x.lBoundary.point.Length + avgBounds
                                  }
                  }
                | _, _ -> x
            )

      let accHeight = replacedBounds |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
      let factor = logHeight / accHeight
      let result = 
        replacedBounds
          |> List.map (fun (x : LogNode) -> //TODO replace with max child or remove
                        {x with size = V3d.OOI 
                                        + V3d.OIO * (Rangef.calcRange x.range) * factor
                                        + V3d.IOO * 30.0 * (float) (x.level + 1) //TODO hardcoded stuff
                        }
                      )
          |> List.scan (fun (x : LogNode) (y : LogNode) -> 
                            {y with logYPos = x.logYPos + x.size.Y}
                        ) (LogNode.initialEmpty) 
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

  let intial (id : string)
             (lst : List<V3d * Annotation>) 
             (annos : plist<Annotation>) 
             (semApp : SemanticApp) :
              GeologicalLog = {
    id          = id
    label       = "log"

    nodes       = (generateLevel 
                    lst 
                    annos //(annos |> PList.map (Annotation.update Annotation.Action.HoverOut))
                    semApp 
                    {point = (V3d.OOO)
                     anno = (Annotation.initialDummyWithPoints (V3d.OOO))}
                    {point = (V3d.PositiveInfinity)
                     anno = (Annotation.initialDummyWithPoints (V3d.PositiveInfinity))}
                  ) |> calcLogYPos 300.0

    annoPoints  = lst
    range       = Rangef.init
    camera      = 
      { ArcBallController.initial with 
                     view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
  }

  type Action =
      | CameraMessage             of ArcBallController.Message    
//      | AddNode                   of (V3d * Annotation)
      

  let update (model : GeologicalLog) (semanticApp : SemanticApp) (msg : Action) =
    match msg with 
      | CameraMessage m -> 
          {model with camera = ArcBallController.update model.camera m}

  //let calculatePositions (model : MGeologicalLog) (semanticApp : MSemanticApp)  (viewType : LogNodeView) =
    

  let svgView (model : MGeologicalLog) (semanticApp : MSemanticApp) (isSelected : bool) (viewType : LogNodeView) =
    let minLvl = getMinLevel model
    let minLvlNodes =
      model.nodes
        |> AList.filter (fun n -> Mod.force n.level = minLvl)
        |> AList.toSeq // TODO check if OK
        |> Seq.sortByDescending (fun n -> Mod.force n.elevation)

    let nodeViews =
      alist {
        for n in minLvlNodes do
          let v = (LogNode.View.Svg.view n semanticApp viewType)
          for it in v do
            yield it           
      }
    nodeViews  

  let view (model : MGeologicalLog) (semanticApp : MSemanticApp) (isSelected : bool)  =
    let minLvl = getMinLevel model
    let minLvlNodes =
      model.nodes
        |> AList.filter (fun n -> Mod.force n.level = minLvl)
        |> AList.toSeq
        |> Seq.sortByDescending (fun n -> Mod.force n.elevation)
      

     

    let nodeViews =
      alist {
        for n in minLvlNodes do
          yield 
              Incremental.ul ([clazz  "ui inverted list"] |> AttributeMap.ofList) 
                        (alist {
                          let! v = (LogNode.View.Debug.debugView n semanticApp)
                          for it in v do
                            yield it
                        })                      
              
      }


    let attributes = 
      match isSelected with
        | true  -> [style "border: 2px solid orange"]
        | false -> []
      |> AttributeMap.ofList
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
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.vertexColor
                toEffect DefaultSurfaces.thickLine                                
                ] 
            |> Sg.noEvents
            |> Sg.uniform "LineWidth" width
            |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
            |> Sg.depthTest (Mod.constant DepthTestMode.None)

      
      




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
