namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Debug =
    open Aardvark.Base
    open Aardvark.UI
    open UIPlus
    open Aardvark.Base.Incremental

    open Helper
    open UIPlus

    let print (n : LogNode) = 
      printf "%s--%i--n=%s--ub=%s--lb=%s" 
                (n.nodeType.ToString()) 
                (n.level.level)
                n.id.id 

    //let incrPrint (n : MLogNode) = 
    //  adaptive {
    //    let! t =  (n.nodeType)
    //    let! lvl = (n.level)
    //    //let tab = 
    //    //  [0 .. lvl.level] 
    //    //    |> List.map (fun i -> "...")
    //    //    |> List.fold (fun s1 s2 -> sprintf "%s%s" s1 s2) ("") 
    //    let str = 
    //      sprintf "%s : lvl = %i : id =%s" 
    //              (t.ToString ())
    //              lvl.level
    //              (n.id.id.Substring (0,5))
    //    return str
    //  }

    let viewAll' (lst     : alist<MLogNode>)
                 (f       : MLogNode -> alist<DomNode<'a>>) =
      let rows = 
        alist {
          for el in lst do 
            yield! Recursive.mapAndCollect_M el f
        }  
        
      Table.toTableView (div[][]) rows ["Nodes"]

    let view (semApp  : MSemanticApp) 
             (annoApp : MAnnotationModel)
             (lifter  : (LogNodeId * Action) -> 'a)
             (model   : MLogNode) =
      
      let ubColor = Border.colorOrDefault model.uBorder
      let lbColor = Border.colorOrDefault model.lBorder
      
      let atts = 
        amap {
                let! lvl = model.level
                let leftMargin = (float lvl.level) * 2.0
                let styl = sprintf ("%s; margin-left: %.2fem; margin-top: 0.3em; margin-bottom: 0.5em;") 
                                    "text-align: left" leftMargin
                yield style styl
                yield onClick (fun _ -> Action.ToggleSelectNode model.id)
             }
               |> AttributeMap.ofAMap      
        
      let columns = 
        let typ = Mod.map (fun t -> t.ToString ()) model.nodeType
        let lvl = Mod.map (fun (x : NodeLevel) -> sprintf "Level %i" x.level) model.level
        let pos = Mod.map (fun (v : V2d) -> sprintf "(%.1f,%.1f)" v.X v.Y) model.mainBody.pos
        let dim = Mod.map (fun (v : SimpleTypes.Size2D) -> sprintf "(%.1f,%.1f)" v.X v.Y) model.mainBody.dim
        alist {
            yield (UIPlus.Labels.Incremental.labelCi lvl (model.mainBody.colour)) |> UIPlus.Table.intoTd
            yield (UIPlus.Labels.Incremental.labelCi typ (model.mainBody.colour)) |> UIPlus.Table.intoTd
            yield (ColorPicker.view model.mainBody.colour) |> UI.map ColorPickerMessage |> UIPlus.Table.intoTd
            yield (UIPlus.Labels.Incremental.labelCi pos (model.mainBody.colour)) |> UIPlus.Table.intoTd
            yield (UIPlus.Labels.Incremental.labelCi dim (model.mainBody.colour)) |> UIPlus.Table.intoTd
            //yield ( "draw" (fun _ -> Svgplus.Rectangle.Action.ToggleDraw))
            //        |> UI.map Action.RectangleMessage
        }
      
      let domNode = 
        Incremental.div atts columns

      let mapped =
        domNode |> UI.map (fun action -> lifter (model.id, action))

      mapped
        
