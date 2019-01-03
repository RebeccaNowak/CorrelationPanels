namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Helper = 
    open Aardvark.Base
    open Aardvark.UI
    open Aardvark.Base.Incremental


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


/////////////////////////////////////////////////////////

    let elevation  (model : LogNode) (annoApp : AnnotationApp) =
      match model.lBorder, model.uBorder, model.annotation with
        | Some lb, Some ub, _ ->
          Option.map2 (fun a b -> a + b * 0.5)
            (Border.tryElevation lb annoApp) 
            (Border.tryElevation ub annoApp)
        | _,_, Some a -> 
          AnnotationApp.tryElevation annoApp a
        | _,_,_ -> 
          printf "could not calculate elevation" //TODO proper error handling
          None

    let elevation'  (model : MLogNode) (annoApp : MAnnotationApp) =
      let hasAnno = Option.modIsSome model.annotation
      let hasLb   = Option.modIsSome model.lBorder
      let hasUb   = Option.modIsSome model.uBorder

      adaptive {
        let! hasLb = hasLb
        let! hasUb = hasUb
        let! hasAnno = hasAnno

        match hasLb, hasUb, hasAnno with
          | true, true,  _ ->
              let! lb = model.lBorder
              let lb = lb.Value
              let! ub = model.uBorder
              let ub = ub.Value
              let! lbel = Border.elevation' lb annoApp
              let! ubel = Border.elevation' ub annoApp
              return lbel + ubel * 0.5
          | _,_, true -> 
              let a = model.annotation |> Mod.map (fun a -> a.Value)
              return! AnnotationApp.elevation' annoApp a
          | _,_,_ -> 
            printf "could not calculate elevation" //TODO proper error handling
            return 0.0
      }
    //let elevation'  (model : MLogNode) (annoApp : MAnnotationApp) = 
    //  adaptive {
    //    let! lb = model.lBorder
    //    let! lu = model.uBorder
    //    let! a = model.annotation
    //    return 
    //      match lb , lu, a with
    //        | Some lb, Some ub, _ ->
    //            (Border.elevation' lb annoApp) 
    //              + (Border.elevation' ub annoApp) * 0.5
    //        | _,_, Some a -> 
    //          AnnotationApp.elevation' annoApp a
    //        | _,_,_ -> 
    //          printf "could not calculate elevation" //TODO proper error handling
    //          0.0
      
        
    //      //let! lowerBorderElevation = (Border.elevation' model.lBorder annoApp) 
    //      //let! upperBorderElevation = (Border.elevation' model.uBorder annoApp)
    //      //return (lowerBorderElevation + upperBorderElevation) * 0.5
    //  }         


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

    let tryLowestBorder (lst : plist<LogNode>) (annoApp : AnnotationApp)  =
        lst |> PList.map (fun p -> p.lBorder) 
            |> DS.PList.filterNone
            |> DS.PList.tryMinBy (fun b -> Border.tryElevation b annoApp)

    let tryHighestBorder (lst : plist<LogNode>) (annoApp : AnnotationApp) =
        lst |> PList.map (fun p -> p.uBorder) 
            |> DS.PList.filterNone
            |> DS.PList.tryMaxBy (fun b -> Border.tryElevation b annoApp)

    let findLowestNode (lst : plist<LogNode>) (annoApp : AnnotationApp)  =
      lst |> DS.PList.tryMaxBy (fun n -> elevation n annoApp)

    let findHighestNode (lst : plist<LogNode>) (annoApp : AnnotationApp)  =
      lst |> DS.PList.tryMinBy (fun n -> elevation n annoApp)      


/////////////////////////////////////////////////////////////
    let elevationRange (node : LogNode)  = 
      match node.lBorder, node.uBorder with
        | Some lb, Some ub ->
          {Rangef.init with min = lb.point.Length
                            max = ub.point.Length}
        | _,_ -> 
          printf "range calc failed"
          Rangef.init

    let private isInfinityTypeLeaf (model : LogNode) =
      let noChildren = (model.children.IsEmpty())
      let infType = (model.nodeType = LogNodeType.NegInfinity 
                      || model.nodeType = LogNodeType.PosInfinity)
      let notDataNode = (model.nodeType <> LogNodeType.Angular 
                          && model.nodeType <> LogNodeType.Metric)
      (noChildren && infType && notDataNode)
        
  

    let rec replaceInfinity (model : LogNode) (annoApp : AnnotationApp) : (LogNode) =
      let (newNode) =
        let notLeaf = not (isInfinityTypeLeaf model)  
        let noChildren = model.children.IsEmpty()
        match notLeaf, noChildren, model.nodeType with
          | true, true, _  -> model //(model, false)
          | true, false, LogNodeType.NegInfinity -> 
            let children   = model.children 
                               |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                               |> PList.map (fun c -> replaceInfinity c annoApp)   
            if children.IsEmpty() then model else
              let optLb = tryLowestBorder children annoApp
              {model with lBorder =  Option.map (fun lb -> {lb with nodeId = model.id}) optLb
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
            
          | true, false, LogNodeType.PosInfinity ->
            let children   = model.children 
                              |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                              |> PList.map (fun c -> replaceInfinity c annoApp)   
            if children.IsEmpty() then model else
              let optUb = tryHighestBorder children annoApp
              let ub = optUb |> Option.map (fun b -> {b with nodeId = model.id})
              {model with uBorder =   ub
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
            
          | _, _ , _ -> (model)
      (newNode)

    let replaceInfinity' (nodes : plist<LogNode>) 
                         (annoApp : AnnotationApp) : plist<LogNode> =
      let _nodes = 
        nodes
          |> PList.map (fun n -> (replaceInfinity n annoApp))
          |> PList.filter (fun n -> not (isInfinityTypeLeaf n ))
      
      for n in _nodes do
        printf "%s" (n.nodeType.ToString ())

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
 












                                        
          

         