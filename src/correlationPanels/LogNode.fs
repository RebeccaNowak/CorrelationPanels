namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module LogNode =

    open Aardvark.Base
    open Aardvark.UI
    open UtilitiesGUI
    open Aardvark.Base.Incremental



          

    type Action =
      | ChangeXAxis       of SemanticId
      | MouseOver         of LogNodeId
      | ToggleSelectNode  of LogNodeId
      | DrawCorrelation   of LogNodeId



    let initialEmpty (id : LogNodeId) : LogNode = {
      id           = id
      isSelected   = false
      hasDefaultX  = false
      nodeType     = LogNodeType.Empty
      label        = "log node"
      level        = -1
      lBoundary    = Border.initial (Annotation.initial "-1") (V3d(1.0))
      uBoundary    = Border.initial (Annotation.initial "-1") (V3d(1.0))
      children     = plist.Empty
      logYPos      = 0.0
      logXPos      = 0.0
      pos          = V3d.OOO
      size         = V3d.OOO
    }


    let initialTopLevel 
      (id : LogNodeId) 
      ((up, ua) : (V3d * Annotation)) 
      ((lp, la) : (V3d * Annotation)) 
      (children : plist<LogNode>)
      (level    : int) : LogNode = 
    
      let lBoundary = Border.initial la lp
      let uBoundary = Border.initial ua up

      {initialEmpty id with
        nodeType      = 
          match lBoundary.borderType, uBoundary.borderType with
            | BorderType.NegativeInfinity, BorderType.Normal 
              -> LogNodeType.NegInfinity
            | BorderType.Normal, BorderType.PositiveInfinity
              -> LogNodeType.PosInfinity
            | BorderType.NegativeInfinity, BorderType.PositiveInfinity
              -> LogNodeType.Infinity
            | BorderType.Normal, BorderType.Normal
              -> LogNodeType.Hierarchical
            | _,_ -> LogNodeType.Empty

        label         = "log node"
        level         = level
        lBoundary     = lBoundary
        uBoundary     = uBoundary
        children      = children
      }

    // TODO add level
    let initialHierarchical (id : LogNodeId)  (anno : Annotation) (lower : Border) (upper : Border) =
      {initialEmpty id with
        nodeType    = LogNodeType.HierarchicalLeaf
       // elevation   = Annotation.elevation anno
        lBoundary   = lower
        uBoundary   = upper}

    let intialMetric (id : LogNodeId)  (anno : Annotation)  =

      {initialEmpty id with
        nodeType     = LogNodeType.Metric
        lBoundary    = Border.initial anno (Annotation.lowestPoint anno).point
        uBoundary    = Border.initial anno (Annotation.highestPoint anno).point
      }


    let intialAngular (id : LogNodeId)  (anno : Annotation) =
      {initialEmpty id with
        nodeType    = LogNodeType.Angular
        lBoundary    = Border.initial anno (Annotation.lowestPoint anno).point
        uBoundary    = Border.initial anno (Annotation.highestPoint anno).point}
    /////////////////////


    let rec apply (n : LogNode) (f : LogNode -> LogNode) =
      match PList.count n.children with
              | 0     -> f n 
              | other -> 
                  let c = n.children |> PList.map (fun (n : LogNode) -> apply n f)
                  f {n with children = c}

    let rec filterAndCollect (n : LogNode) (f : LogNode -> bool) =
      match PList.count n.children, f n with
      | 0, true      -> [n]
      | 0, false     -> []
      | other, true  -> 
        [n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect x f))
      | other, false -> 
        [] @  (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect x f))
  
    let rec filterAndCollect' (n : MLogNode) (f : MLogNode -> bool) =
      match PList.count (Mod.force n.children.Content), f n with //TODO evil
      | 0, true      -> [n]
      | 0, false     -> []
      | other, true  -> 
        [n] @ 
          (n.children 
                |> AList.toList
                |> List.collect (fun (x : MLogNode) -> filterAndCollect' x f))
      | other, false -> 
        [] @  (n.children 
                |> AList.toList
                |> List.collect (fun (x : MLogNode) -> filterAndCollect' x f))

    let elevation  (model : LogNode) = 
      (Annotation.elevation model.lBoundary.anno) 
        + (Annotation.elevation model.uBoundary.anno) * 0.5

    let elevation'  (model : MLogNode) = 
      Mod.map2 (fun x y -> (x + y) * 0.5)
               (Annotation.elevation' model.lBoundary.anno) 
               (Annotation.elevation' model.uBoundary.anno)
              

    let findLowestBorder (lst : plist<LogNode>) =
      lst |> PList.minMapBy (fun p -> p.lBoundary) (fun p -> Border.calcElevation p.lBoundary)

    let findHighestBorder (lst : plist<LogNode>) =
      lst |> PList.maxMapBy (fun p -> p.uBoundary) (fun p -> Border.calcElevation p.uBoundary)
      

    let rec replaceInfinity (model : LogNode) =
      match model.children.IsEmpty(), model.nodeType with
        | true, _ -> model
        | false, LogNodeType.NegInfinity -> 
              let children   = model.children |> PList.map (fun c -> replaceInfinity c)   
              let lb = findLowestBorder children
              {model with lBoundary = lb
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
        | false, LogNodeType.PosInfinity ->
              let children   = model.children |> PList.map (fun c -> replaceInfinity c)   
              let ub = findHighestBorder children
              {model with uBoundary = ub
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
        | _ , _ -> model


    let isInfinityType (model : LogNode) =
      match model.nodeType with 
        | LogNodeType.NegInfinity
        | LogNodeType.PosInfinity -> false
        | _ -> true


    let calcMetricValue (n : LogNode) =
      let points = n.lBoundary.anno.points |> PList.toList
      let h = List.tryHead points
      let t = List.tryLast points
      Option.map2 (fun (x : AnnotationPoint) (y : AnnotationPoint) -> 
                      let x = x.point
                      let y = y.point
                      V3d.Distance(x,y)) h t

    let calcSizeX (model : LogNode) (xAxis : SemanticId) = 
      let cs (node : LogNode) = 
        let metricNodes = filterAndCollect node (fun n -> n.lBoundary.anno.semanticId = xAxis)
        let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n)
        let sizeX = (metricValues |> List.filterNone |> List.averageOrZero) * 30.0 //TODO hardcoded
        {node with size = (node.size * V3d.OII) + (V3d.IOO) * sizeX}
      apply model cs

    
    let defaultIfZero (model : LogNode) (defaultSizeX : float) =
      let diz (node : LogNode) = 
        match node with
          | n when n.size.X = 0.0 -> 
            {n with size        = (node.size * V3d.OII) + (V3d.IOO) * defaultSizeX
                    hasDefaultX = true}
          | _ -> node
      apply model diz

    /////////////////////////////////////////////////////////////////////////////////
    let update  (action : Action) (model : LogNode) =
      match action with
        | ChangeXAxis id -> calcSizeX model id
        | MouseOver id -> 
            model //(printf "w = %f h = %f" size.X size.Y)
        | ToggleSelectNode id -> 
          let res = 
            (fun (n : LogNode) -> 
                match (id = model.id) with
                | true -> {n with isSelected = not n.isSelected}
                | false -> n)
          apply model  res
        | DrawCorrelation id -> model
      
        



    /////////////////////////////////////////////////////////////////////////////////

    let hasChildren (model : MLogNode) =
      let isEmpty = AList.isEmpty model.children
      Mod.map (fun x -> not x) isEmpty

    module View =

      module Debug =
        let description (model : MLogNode) (semApp : MSemanticApp) = 
          

          let createDomNode (descString : IMod<string>) (sel : IMod<bool>) =
            Incremental.div
              (amap {
                let! sel = model.isSelected
                match sel with
                  | true  -> yield style "border: 2px solid orange"
                  | false -> yield style "border: none"
              }
              |> AttributeMap.ofAMap)        
              
              (AList.ofList 
                [
                  Annotation.View.getColourIcon model.uBoundary.anno semApp
                  Annotation.View.getColourIcon model.lBoundary.anno semApp
                  Incremental.text descString
                ])

          let modStr = 
            model.nodeType 
              |> Mod.bind 
                (fun t -> 
                  match t with
                    | LogNodeType.HierarchicalLeaf 
                    | LogNodeType.NegInfinity
                    | LogNodeType.PosInfinity
                    | LogNodeType.Hierarchical ->
                        model.nodeType |> Mod.map (fun x -> x.ToString())
                      //(Mod.map2 (fun (u : V3d) (l : V3d)  -> 
                                      //sprintf "%.2f-%.2f" l.Length u.Length)
                                      //model.uBoundary.point model.lBoundary.point)
                    | LogNodeType.Angular | LogNodeType.Metric ->
                        //Mod.map (sprintf "%s" ) model.label
                        model.nodeType |> Mod.map (fun x -> x.ToString())
                    | LogNodeType.Empty | LogNodeType.Infinity -> model.nodeType |> Mod.map (fun x -> x.ToString())
                )
      
          createDomNode modStr model.isSelected

        let rec debugView (model : MLogNode) (semApp : MSemanticApp) =
          let childrenView = 
            alist {
              for c in model.children do
                let! (v : alist<DomNode<'a>>) = (debugView c semApp)
                for it in v do
                  yield it
            }
    
          let rval =
            adaptive {
              let isEmpty = AList.isEmpty model.children
              let! (b : bool) = isEmpty
              match b with
                | true  -> 
                    return AList.ofList [li [attribute "value" ">"] [(description model semApp)]]
                | false ->                
                    return AList.ofList [li [attribute "value" "-"] [(description model semApp)];
                           ul [] [Incremental.li (AttributeMap.ofList [attribute "value" "-"]) childrenView]]
            }
          rval

      module Svg =
      
        let containsHNodes (node : MLogNode) =
          let foo = (Mod.force node.children.Content) 
                      |> PList.filter (fun n -> (Mod.force n.nodeType = LogNodeType.Hierarchical))
          (not (foo.IsEmpty()))
        
        let rec createView (offset        : float)
                           (secondaryLvl  : int)
                           (model         : MLogNode) 
                           (viewFunction  : float 
                                            -> MLogNode 
                                            -> IMod<(float 
                                                      -> Option<float> 
                                                      -> list<DomNode<'msg>>
                                            )>
                           )
                           : alist<DomNode<'msg>> =
          let breadthSec = 20.0
          let offset =
            adaptive {
              let! lvl = model.level 
              let sLvl = secondaryLvl
              return match (lvl = sLvl), lvl = 0 with
                      | true, true  -> offset + breadthSec
                      | false, true -> offset + breadthSec
                      | true, false -> offset
                      | false, false -> offset
            }
          
          let childrenView = 
            alist {
              let! os = offset
              for c in model.children do               
                let v = (createView os secondaryLvl c viewFunction)
                for it in (v : alist<DomNode<'msg>>) do
                  yield it
            }
    
          let rval =
            alist {
              let! os = offset
              let! selfViewFunction = viewFunction os model
              let! hasCs = hasChildren model
              let selfView = selfViewFunction os None
              let! lvl = model.level 
              if lvl = secondaryLvl then
                for v in (selfViewFunction 0.0 (Some breadthSec)) do yield v
              match hasCs with
                | false  -> 
                    for v in selfView do
                      yield v                
                | true   ->  
                  let! lstChildren = childrenView.Content     
                  match (containsHNodes model) with
                    | true  ->
                      yield (Svg.toGroup (lstChildren |> PList.toList) [])                                              
                    | false ->
                      for v in selfView do
                        yield v 
                      yield (Svg.toGroup (lstChildren |> PList.toList) [])
            }
          rval

        let view (model        : MLogNode) 
                 (secondaryLvl : IMod<int>)
                 (viewType     : CorrelationPlotViewType) 
                 (styleFun     : float -> IMod<LogNodeStyle>) =
          let f = LogNodeSvg.getDomNodeFunction 
                    viewType styleFun 
                    (fun id lst -> ToggleSelectNode id) 
                    (Some (fun id lst -> DrawCorrelation id))
          adaptive {
            let! sLvl = secondaryLvl
            return createView 0.0 sLvl model f          
          }
                                        
          

         