namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Debug =
    open Aardvark.Base
    open Aardvark.UI
    open UI
    open Aardvark.Base.Incremental

    open Helper
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
                    adaptive {
                      let! t = model.nodeType
                      let! lower = model.lBorder.point
                      let! upper = model.uBorder.point
                        //|> Mod.map (fun x -> x.ToString())
                      return sprintf "%s: %s-%s" (t.ToString ()) (String.fromV3d upper) (String.fromV3d lower)
                    }
                      
                | LogNodeType.Angular ->
                    match (calcAngularValue' model) with //TODO angular
                      | Some v -> 
                        model.nodeType 
                          |> Mod.map (fun x -> sprintf "%s: %.2f" (x.ToString()) v.radians)
                      | None -> 
                        model.nodeType 
                          |> Mod.map (fun x -> x.ToString())
                | LogNodeType.Metric ->
                    match (calcMetricValue' model) with
                      | Some v -> 
                        model.nodeType 
                          |> Mod.map (fun x -> sprintf "%s: %.2f" (x.ToString()) v)
                      | None -> 
                        model.nodeType 
                          |> Mod.map (fun x -> x.ToString())
                | LogNodeType.Empty | LogNodeType.Infinity -> model.nodeType |> Mod.map (fun x -> x.ToString())
            )
      
      createDomNode modStr model.isSelected

    let rec view (model : MLogNode) (semApp : MSemanticApp) =
      let rval =
        adaptive {
          let isEmpty = AList.isEmpty model.children
          let! (b : bool) = isEmpty
          //let! level = model.level
          match b with
            | true -> 
              return AList.ofList [li [attribute "value" ">"] [(description model semApp)]]
            | false ->                
              let childrenView = 
                alist {
                  for c in model.children do
                    let! (v : alist<DomNode<'a>>) = (view c semApp)
                    for it in v do
                      yield it
                }
              return AList.ofList [li [attribute "value" "-"] [(description model semApp)];
                      ul [] [Incremental.li (AttributeMap.ofList [attribute "value" "-"]) childrenView]]
      }
      rval

