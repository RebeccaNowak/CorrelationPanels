namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Helper = 
    open Aardvark.Base
    open Aardvark.UI
    open Aardvark.Base.Incremental
    open Aardvark.Base.MultimethodTest


    let hasChildren (model : MLogNode) =
      let isEmpty = DS.AList.isEmpty model.children
      Mod.map (fun x -> not x) isEmpty

    let hasNodeType (model : MLogNode) (t : LogNodeType) =
      adaptive {
        let! mt = model.nodeType
        return mt = t
      }
      
    let annoIdOrInvalid (n : LogNode) =
      Option.defaultValue AnnotationId.invalid n.annotation 

    let semanticIdOrInvalid (n : LogNode) ( annoApp : AnnotationApp) =
      let anno = AnnotationApp.findAnnotation annoApp (annoIdOrInvalid n)
      match anno with
        | None -> SemanticId.invalid
        | Some a -> a.semanticId

    let elevationRange (node : LogNode) (annoApp : AnnotationApp) = 
      match node.lBorder, node.uBorder, node.annotation with
        | Some lb, Some ub, _ ->
          let lower = Border.tryElevation lb annoApp
          let upper = Border.tryElevation ub annoApp
          match lower, upper with
            | Some lo, Some up ->
              {Rangef.init with min = lo
                                max = up}
            | _,_ -> 
              Log.warn "range calc failed"
              Rangef.init
        | None,None, Some a -> 
          let optEl = AnnotationApp.tryElevation annoApp a
          match optEl with
            | Some el ->
              {Rangef.init with min = el
                                max = el}
            | None ->           
              Log.warn "range calc failed"
              Rangef.init
        | _,_,_ ->
          Log.warn "range calc failed"
          Rangef.init

    let elevationLowerBorder (node : LogNode) (annoApp : AnnotationApp) = 
       (elevationRange node annoApp).min

    let elevationUpperBorder (node : LogNode) (annoApp : AnnotationApp) = 
       (elevationRange node annoApp).max


/////////////////////////////////////////////////////////

    let elevation  (model : LogNode) (annoApp : AnnotationApp) =
      let r = elevationRange model annoApp
      r.average


    //TODO need to change for Pro3D integration
    let calcMetricValue (n : LogNode) (annoApp : AnnotationApp) =
      let anno = Option.bind (fun a -> AnnotationApp.findAnnotation annoApp a) n.annotation
      match anno with
        | None -> None
        | Some anno ->
          let points = anno.points |> PList.toList
          let h = List.tryHead points
          let t = List.tryLast points
          Option.map2 (fun (x : AnnotationPoint) (y : AnnotationPoint) -> 
                          let x = x.point
                          let y = y.point
                          V3d.Distance(x,y)) h t
    
    //TODO need to change for Pro3D integration
    let calcMetricValue' (n : MLogNode) (annoApp : MAnnotationApp) = 
      adaptive {
        let! annoId = n.annotation
        let! anno =  AnnotationApp.findAnnotation'' annoApp annoId
        //let lowerAnno = annoId |> Option.bind (fun a -> AnnotationApp.findAnnotation' annoApp a)
        return match anno with
                | None -> None
                | Some anno ->
                  let points = anno.points |> AList.toList
                  let h = List.tryHead points
                  let t = List.tryLast points
                  Option.map2 (fun (x : MAnnotationPoint) (y : MAnnotationPoint) -> 
                                  let x = x.point
                                  let y = y.point
                                  V3d.Distance(x,y)) h t
      }
     

    //TODO need to change for Pro3D integration
    let calcAngularValue' (n : MLogNode) (annoApp : MAnnotationApp) = 
      adaptive {
        let! id = n.annotation
        let! lowerAnno = AnnotationApp.findAnnotation'' annoApp id
        return 
          match lowerAnno with
            | None -> None
            | Some anno ->
              let points = anno.points |> AList.toList
              let h = List.tryHead points
              let t = List.tryLast points
              Option.map2 (fun (x : MAnnotationPoint) (y : MAnnotationPoint) -> 
                              let x = x.point
                              let y = y.point
                              SimpleTypes.Math.Angle.init (V3d.Distance (x,y))) h t
      }

    let filterInfinity annoApp border  = 
      let opt = Border.tryElevation border annoApp
      let keep =
        match opt, border.borderType with
          | Some d, BorderType.Normal -> Some (border, d)
          | _, _ -> None
      keep

    let tryLowestBorder (lst : plist<LogNode>) (annoApp : AnnotationApp)  =

      let opt = 
        lst |> PList.map (fun p -> p.lBorder) 
            |> DS.PList.filterNone
            |> PList.map (filterInfinity annoApp)
            |> DS.PList.filterNone
            |> DS.PList.tryMinBy (fun (b, d) -> d)
      Option.map (fun (b,d) -> b) opt

    let tryHighestBorder (lst : plist<LogNode>) (annoApp : AnnotationApp) =
      let opt =
        lst |> PList.map (fun p -> p.uBorder) 
            |> DS.PList.filterNone
            |> PList.map (filterInfinity annoApp)
            |> DS.PList.filterNone
            |> DS.PList.tryMaxBy (fun (b, d) -> d)
      Option.map (fun (b,d) -> b) opt

    let findLowestNode (lst : plist<LogNode>) (annoApp : AnnotationApp)  =
      lst |> DS.PList.tryMaxBy (fun n -> elevation n annoApp)

    let findHighestNode (lst : plist<LogNode>) (annoApp : AnnotationApp)  =
      lst |> DS.PList.tryMinBy (fun n -> elevation n annoApp)      


/////////////////////////////////////////////////////////////


    let private isInfinityTypeLeaf (model : LogNode) =
      let hasChildren = not 
      let isInfType = (model.nodeType = LogNodeType.NegInfinity 
                      || model.nodeType = LogNodeType.PosInfinity)
      let isDataNode = (model.nodeType <> LogNodeType.Angular 
                          || model.nodeType <> LogNodeType.Metric)
      ((model.children.IsEmpty () ) && isInfType)
        
    let rec replaceInfinity (model : LogNode) (annoApp : AnnotationApp) : (LogNode) =
      let (newNode) = 
        let noChildren = model.children.IsEmpty()
        match noChildren, model.nodeType with
          | false, LogNodeType.NegInfinity -> 
            let children   = model.children 
                               |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                               |> PList.map (fun c -> replaceInfinity c annoApp)   
            if children.IsEmpty() then model else
              let optLb = tryLowestBorder children annoApp
              {model with lBorder   =  Option.map (fun lb -> {lb with nodeId = model.id}) optLb
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
            
          | false, LogNodeType.PosInfinity ->
            let children   = model.children 
                              |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                              |> PList.map (fun c -> replaceInfinity c annoApp)   
            if children.IsEmpty() then model else
              let optUb = tryHighestBorder children annoApp
              let ub = optUb |> Option.map (fun b -> {b with nodeId = model.id})
              {model with uBorder   =   ub
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
          | _ , _ -> (model)
      (newNode)

    let replaceInfinity' (nodes : plist<LogNode>) 
                         (annoApp : AnnotationApp) : plist<LogNode> =
      let _nodes = 
        nodes
          |> PList.map (fun n -> (replaceInfinity n annoApp))
          |> PList.filter (fun n -> not (isInfinityTypeLeaf n ))
      
      //for n in _nodes do
      //  printf "%s" (n.nodeType.ToString ())

      _nodes
        
///////////////////////////////////////////      


    let findBorder (model : LogNode) (id : BorderId) =
      Option.map2 ( fun (lb : Border) (ub : Border) ->
          match lb.id = id, ub.id = id with
            | true, _      -> Some model.lBorder
            | _,true       -> Some model.uBorder
            | false, false -> None
      ) model.lBorder model.uBorder
      |> Option.flatten
      |> Option.flatten
 












                                        
          

         