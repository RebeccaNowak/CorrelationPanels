namespace CorrelationDrawing.LogNodes
open CorrelationDrawing
open Svgplus
open Svgplus.DA
open Aardvark.UI
open Aardvark.Base.Incremental
open UIPlus
open UIPlus.Table

module View =
  let listViewSingle (model       : MLogNode) 
                     (diagramApp  : MDiagram) =
      
    let atts = 
      amap {
            let! lvl = model.level
            let leftMargin = (float lvl.level) * 2.0
            let styl = sprintf ("%s; margin-left: %.2fem; margin-top: 0.3em; margin-bottom: 0.5em;") 
                                "text-align: left" leftMargin
            yield style styl
            yield onClick (fun _ -> Action.ToggleSelectNode model.id)
          } |> AttributeMap.ofAMap  

    let domNode = 
      let domNodeLevel = 
        (Incremental.text (Mod.map(fun (x : NodeLevel) -> 
                                    sprintf "%i" x.level) 
                                  model.level))
          |> intoTd
      let domNodeSemanticType =  
        intoTd <|
          Incremental.text (Mod.map(fun x -> x.ToString()) 
                              model.nodeType)
      let domNodeLabel =
        alist {
          let! rect = Diagram.findRectangle_M diagramApp model.rectangleId 
          if rect.IsSome then
            let r = rect.Value
            let v = TextInput.view r.label
            yield v
        }

      [domNodeLevel;domNodeSemanticType]

    Table.intoTr domNode //WIP
