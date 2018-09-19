namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Helper = 
    open Aardvark.Base
    open Aardvark.UI
    open Aardvark.Base.Incremental


    let hasChildren (model : MLogNode) =
      let isEmpty = AList.isEmpty model.children
      Mod.map (fun x -> not x) isEmpty

    let hasNodeType (model : MLogNode) (t : LogNodeType) =
      adaptive {
        let! mt = model.nodeType
        return mt = t
      }
      

/////////////////////////////////////////////////////////

    let elevation  (model : LogNode) =
      (Border.elevation model.lBorder) 
        + (Border.elevation model.uBorder) * 0.5

    let elevation'  (model : MLogNode) = 
      Mod.map2 (fun x y -> (x + y) * 0.5)
               (Border.elevation' model.lBorder) 
               (Border.elevation' model.uBorder)

    let calcMetricValue (n : LogNode) =
      let points = n.lBorder.anno.points |> PList.toList
      let h = List.tryHead points
      let t = List.tryLast points
      Option.map2 (fun (x : AnnotationPoint) (y : AnnotationPoint) -> 
                      let x = x.point
                      let y = y.point
                      V3d.Distance(x,y)) h t

    let calcMetricValue' (n : MLogNode) =
      let points = n.lBorder.anno.points |> AList.toList
      let h = List.tryHead points
      let t = List.tryLast points
      Option.map2 (fun (x : MAnnotationPoint) (y : MAnnotationPoint) -> 
                      let x = x.point
                      let y = y.point
                      V3d.Distance(x,y)) h t

    let calcAngularValue' (n : MLogNode) = //TODO DUMMY VALUES!!!
      let points = n.lBorder.anno.points |> AList.toList
      let h = List.tryHead points
      let t = List.tryLast points
      Option.map2 (fun (x : MAnnotationPoint) (y : MAnnotationPoint) -> 
                      let x = x.point
                      let y = y.point
                      Math.Angle.init (V3d.Distance (x,y))) h t
              

    let findLowestBorder (lst : plist<LogNode>) =
      lst |> PList.minMapBy (fun p -> p.lBorder) 
                            (fun p -> Border.elevation p.lBorder)

    let findHighestBorder (lst : plist<LogNode>) =
      lst |> PList.maxMapBy (fun p -> p.uBorder) 
                            (fun p -> Border.elevation p.uBorder)
      

/////////////////////////////////////////////////////////////


    let private isInfinityTypeLeaf (model : LogNode) =
      let noChildren = (model.children.IsEmpty())
      let infType = (model.nodeType = LogNodeType.NegInfinity || model.nodeType = LogNodeType.PosInfinity)
      let notDataNode = (model.nodeType <> LogNodeType.Angular && model.nodeType <> LogNodeType.Metric)
      (noChildren && infType && notDataNode)
        
      

    let rec replaceInfinity (model : LogNode) : (LogNode) =
      let (newNode) =
        match not (isInfinityTypeLeaf model), model.children.IsEmpty(), model.nodeType with
          | true, true, _  -> model //(model, false)
          | true, false, LogNodeType.NegInfinity -> 
            let children   = model.children 
                               |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                               |> PList.map (fun c -> replaceInfinity c)   
            if children.IsEmpty() then model else
              let lb = findLowestBorder children
              {model with lBorder = {lb with nodeId = model.id}
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
            
          | true, false, LogNodeType.PosInfinity ->
            let children   = model.children 
                              |> PList.filter (fun n -> not (isInfinityTypeLeaf n))
                              |> PList.map (fun c -> replaceInfinity c)   
            if children.IsEmpty() then model else
              let ub = findHighestBorder children
              {model with uBorder = {ub with nodeId = model.id}
                          children  = children
                          nodeType  = LogNodeType.Hierarchical
              }
            
          | _, _ , _ -> (model)
      (newNode)

    let replaceInfinity' (nodes : plist<LogNode>) : plist<LogNode> =
      let nodes1 = 
        nodes
          |> PList.map replaceInfinity
      nodes1
        |> PList.filter (fun n -> not (isInfinityTypeLeaf n))

      
      
///////////////////////////////////////////      


    let findBorder (model : LogNode) (id : BorderId) =
      match model.lBorder.id = id, model.uBorder.id = id with
        | true, _      -> Some model.lBorder
        | _,true       -> Some model.uBorder
        | false, false -> None












                                        
          

         