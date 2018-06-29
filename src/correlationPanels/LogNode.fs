namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module LogNode =

    open Aardvark.Base
    open Aardvark.UI
    open UtilitiesGUI
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph


    type Action =
      | ChangeXAxis       of SemanticId
      | MouseOver         of LogNodeId
      | ToggleSelectNode  of LogNodeId
      | BorderMessage     of Border.Action
      | DrawCorrelation   of BorderId



    let initialEmpty  : LogNode = {
      id           = LogNodeId.invalid
      isSelected   = false
      hasDefaultX  = false
      nodeType     = LogNodeType.Empty
      label        = "log node"
      level        = -1
      lBorder      = Border.initialEmpty
      uBorder      = Border.initialEmpty
      children     = plist.Empty
      //svgPos.Y      = 0.0
      //svgPos.X      = 0.0
      svgPos       = V2d.OO
      size         = V2d.OO
    }


    let initialTopLevel 
      (logId    : LogId)
      ((up, ua) : (V3d * Annotation)) 
      ((lp, la) : (V3d * Annotation)) 
      (children : plist<LogNode>)
      (level    : int) : LogNode = 
      let nodeId = LogNodeId.newId()
      let lBorder = Border.initial la lp nodeId logId
      let uBorder = Border.initial ua up nodeId logId
      let nodeType = 
        match (lp = Border.negInf),
              (up = Border.posInf) with
          | true, false  -> LogNodeType.NegInfinity
          | false, true  -> LogNodeType.PosInfinity
          | false, false -> LogNodeType.Hierarchical
          | true, true   -> LogNodeType.Infinity

      { 
        initialEmpty with 
          id          = nodeId
          nodeType    = nodeType
          label       = "log node"
          level       = level
          lBorder     = lBorder
          uBorder     = uBorder
          children    = children
      }

    let initialTLWithId
      (nodeId   : LogNodeId)
      (logId    : LogId)
      ((up, ua)  : (V3d * Annotation)) 
      ((lp, la) : (V3d * Annotation)) 
      (children : plist<LogNode>)
      (level    : int) : LogNode = 
      let lBorder = Border.initial la lp nodeId logId
      let uBorder = Border.initial ua up nodeId logId
      let n = initialTopLevel logId (up, ua) (lp, la) children level
      {
        n with id       = nodeId
               lBorder  = lBorder
               uBorder  = uBorder
      }


    // TODO add level
    let initialHierarchicalLeaf 
      (logId    : LogId)
      (anno     : Annotation) 
      (lp       : V3d ) 
      (up       : V3d )  =
      let nodeId = LogNodeId.newId()
      {initialEmpty with
        id        = nodeId
        nodeType  = LogNodeType.HierarchicalLeaf
        lBorder   = Border.initial anno lp nodeId logId
        uBorder   = Border.initial anno up nodeId logId}

    let intialMetric (logId : LogId)  (anno : Annotation)  =
      let nodeId = LogNodeId.newId()
      {initialEmpty with
        id         = nodeId
        nodeType   = LogNodeType.Metric
        lBorder    = Border.initial anno (Annotation.lowestPoint anno).point nodeId logId
        uBorder    = Border.initial anno (Annotation.highestPoint anno).point nodeId logId
      }


    let intialAngular (logId : LogId) (anno : Annotation) =
      let nodeId = LogNodeId.newId()
      {initialEmpty with
        id         = nodeId
        nodeType   = LogNodeType.Angular
        lBorder    = Border.initial anno (Annotation.lowestPoint anno).point nodeId logId
        uBorder    = Border.initial anno (Annotation.highestPoint anno).point nodeId logId}
    /////////////////////
    let rec mapAndCollect (n : LogNode) (f : LogNode -> 'a) =
      match PList.count n.children with
      | 0      -> [f n]
      | other  -> 
        [f n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> mapAndCollect x f))

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

    let elevation  (model : LogNode) = //TODO function in Border
      (Annotation.elevation model.lBorder.anno) 
        + (Annotation.elevation model.uBorder.anno) * 0.5

    let elevation'  (model : MLogNode) = 
      Mod.map2 (fun x y -> (x + y) * 0.5)
               (Annotation.elevation' model.lBorder.anno) 
               (Annotation.elevation' model.uBorder.anno)
              

    let findLowestBorder (lst : plist<LogNode>) =
      lst |> PList.minMapBy (fun p -> p.lBorder) (fun p -> Border.calcElevation p.lBorder)

    let findHighestBorder (lst : plist<LogNode>) =
      lst |> PList.maxMapBy (fun p -> p.uBorder) (fun p -> Border.calcElevation p.uBorder)
      

    let rec replaceInfinity (model : LogNode) =
      match model.children.IsEmpty(), model.nodeType with
        | true, _ -> model
        | false, LogNodeType.NegInfinity -> 
              let children   = model.children |> PList.map (fun c -> replaceInfinity c)   
              let lb = findLowestBorder children
              {model with lBorder = {lb with nodeId = model.id}
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
        | false, LogNodeType.PosInfinity ->
              let children   = model.children |> PList.map (fun c -> replaceInfinity c)   
              let ub = findHighestBorder children
              {model with uBorder = {ub with nodeId = model.id}
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
        | _ , _ -> model


    let isInfinityType (model : LogNode) =
      match model.nodeType with 
        | LogNodeType.NegInfinity
        | LogNodeType.PosInfinity -> false
        | _ -> true

    let findBorder (model : LogNode) (id : BorderId) =
      match model.lBorder.id = id, model.uBorder.id = id with
        | true, _      -> Some model.lBorder
        | _,true       -> Some model.uBorder
        | false, false -> None
        //TODO false,false


    let calcMetricValue (n : LogNode) =
      let points = n.lBorder.anno.points |> PList.toList
      let h = List.tryHead points
      let t = List.tryLast points
      Option.map2 (fun (x : AnnotationPoint) (y : AnnotationPoint) -> 
                      let x = x.point
                      let y = y.point
                      V3d.Distance(x,y)) h t

    let calcSizeX (model : LogNode) (xAxis : SemanticId) = 
      let cs (node : LogNode) = 
        let metricNodes = filterAndCollect node (fun n -> n.lBorder.anno.semanticId = xAxis)
        let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n)
        let sizeX = (metricValues |> List.filterNone |> List.averageOrZero) * 30.0 //TODO hardcoded
        {node with size = (node.size * V2d.OI) + (V2d.IO) * sizeX}
      apply model cs

    
    let defaultIfZero (model : LogNode) (defaultSizeX : float) =
      let diz (node : LogNode) = 
        match node with
          | n when n.size.X = 0.0 -> 
            {n with size        = (node.size * V2d.OI) + (V2d.IO) * defaultSizeX
                    hasDefaultX = true}
          | _ -> node
      apply model diz

    /////////////////////////////////////// UPDATE //////////////////////////////////////////
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
        | DrawCorrelation id -> 
            match (id = model.lBorder.id), (id = model.uBorder.id) with
              | true, true -> model //TODO debug output
              | true, false -> {model with lBorder = (Border.update model.lBorder (Border.Correlate id))} //TODO Lens 
              | false, true -> {model with uBorder = (Border.update model.uBorder (Border.Correlate id))}
              | false, false -> model
        | BorderMessage m -> //TODO performance! stop messages sooner
          let f model = {model with lBorder = (Border.update model.lBorder m)
                                    uBorder = (Border.update model.uBorder m)}
          apply model f //TODO performance!
        



    /////////////////////////////////////////////////////////////////////////////////

    module Debug =
      let print (n : LogNode) = 
        printf "%s\n%i\nn=%s\nub=%s\nlb=%s\n" 
                  (n.nodeType.ToString()) 
                  (n.level)
                  n.id.id 
                  n.uBorder.nodeId.id 
                  n.lBorder.nodeId.id

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
                Annotation.View.getColourIcon model.uBorder.anno semApp
                Annotation.View.getColourIcon model.lBorder.anno semApp
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
                                    //model.uBorder.point model.lBorder.point)
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
      let view  (model        : MLogNode) 
                (secondaryLvl : IMod<int>)
                (viewType     : IMod<CorrelationPlotViewType>) 
                (styleFun     : float -> IMod<LogNodeStyle>) =
        let f = LogNodeSvg.getDomNodeFunction 
                  viewType styleFun 
                  (ToggleSelectNode) 
                  (BorderMessage) //(fun (id : BorderId) lst -> Border.ToggleSelect id)
                  
        adaptive {
          let! sLvl = secondaryLvl
          return LogNodeSvg.createView 0.0 sLvl model f          
        }



                                        
          

         