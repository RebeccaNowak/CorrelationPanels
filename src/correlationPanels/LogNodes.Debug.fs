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
                (n.level.level)
                n.id.id 

    let incrPrint (n : MLogNode) = 
      adaptive {
        let! t =  (n.nodeType)
        let! lvl = (n.level)

        let str = 
          sprintf "%s : lvl = %i : id =%s\n" 
                  (t.ToString ())
                  lvl.level
                  (n.id.id.Substring (0,5))
        return str
      }

    let description (model : MLogNode) =
      let ubColor = Border.colorOrDefault model.uBorder
      let lbColor = Border.colorOrDefault model.lBorder
      let atts = 
       (amap {
                let! sel = model.isSelected
                match sel with
                  | true  -> yield style "border: 2px solid orange"
                  | false -> yield style "border: none"
              }
              |> AttributeMap.ofAMap)        

      Incremental.div atts
        (AList.ofList 
          [
            //Annotation.getColourIcon' model.uBorder.anno semApp
            //Annotation.getColourIcon' model.lBorder.anno semApp
            Incremental.text (incrPrint model)
          ]
        )

    let rec view (model : MLogNode) (semApp : MSemanticApp) =
      let domNodes = 
        alist {
          let isEmpty = AList.isEmpty model.children
          let! (b : bool) = isEmpty
          //let! level = model.level
          match b with
            | true -> 
              //let lst = AList.ofList [li [attribute "value" ">"] [()]]
              yield description model
            | false ->                
              for n in model.children do
                yield! view n semApp
        }
      domNodes
