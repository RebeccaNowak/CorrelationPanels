namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Helper = 
    open Aardvark.Base
    open Aardvark.UI
    open UI
    open Aardvark.Base.Incremental
    open Recursive

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
      

    let isBorderInf (n : LogNode) =
      (n.lBorder.borderType = BorderType.PositiveInfinity)
      ||
      (n.lBorder.borderType = BorderType.NegativeInfinity)
      ||
      (n.uBorder.borderType = BorderType.NegativeInfinity)
      ||
      (n.uBorder.borderType = BorderType.PositiveInfinity)


    let isInfinityTypeLeaf (model : LogNode) =
      let noChildren = (model.children.IsEmpty())
      let infType = (model.nodeType = LogNodeType.NegInfinity || model.nodeType = LogNodeType.PosInfinity)
      let notDataNode = (model.nodeType <> LogNodeType.Angular && model.nodeType <> LogNodeType.Metric)
      let infBorder = isBorderInf model
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


    let getAngularChildren (node : MLogNode) =
       collectAndFilterAll' 
        node 
        (fun n -> 
          Mod.map (fun (t : LogNodeType) -> t = LogNodeType.Angular) n.nodeType
        )
      
   

    let calcSizeX (model : LogNode) (xAxis : SemanticId) (xAxisScaleFactor : float) = 
      let cs (node : LogNode) = 
        let metricNodes = filterAndCollect node (fun n -> n.lBorder.anno.semanticId = xAxis)
        let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n)
        let sizeX = (metricValues |> List.filterNone |> List.maxOrZero) * xAxisScaleFactor
        {node with svgSize = (node.svgSize * V2d.OI) + (V2d.IO) * sizeX}
      apply model cs

    
    let defaultIfZero (model : LogNode) (defaultSizeX : float) =
      let diz (node : LogNode) = 
        match node with
          | n when n.svgSize.X = 0.0 -> 
            {n with svgSize        = (node.svgSize * V2d.OI) + (V2d.IO) * defaultSizeX
                    hasDefaultX = true}
          | _ -> node
      apply model diz






                                        
          

         