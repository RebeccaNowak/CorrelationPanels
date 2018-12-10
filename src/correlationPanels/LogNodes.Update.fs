namespace CorrelationDrawing.LogNodes

 open CorrelationDrawing
 
 module Update = 
    open Svg
    open Recursive

    let private updateOpt (border : option<Border>) (a : Border.Action) =
      Option.map (fun b -> (Border.update b a))
                 border

    let update  (action : Action) (model : LogNode) =
      match action with
        | ChangeXAxis (annoApp, id, scaleFactor) -> Calc.sizeX model id scaleFactor annoApp
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
          match model.lBorder, model.uBorder with
            | Some lb, Some ub ->
              match (id = lb.id), (id = ub.id) with
                | true, true -> model //TODO debug output
                | true, false -> 
                  {model with 
                    lBorder = 
                      updateOpt model.lBorder 
                                (Border.Correlate id)
                      
                  } //TODO Lens 
                | false, true -> {model with uBorder = (updateOpt model.uBorder (Border.Correlate id))}
                | false, false -> model
            | _,_ -> model
        | BorderMessage m -> //TODO performance! stop messages sooner
          let f model = {model with lBorder = (updateOpt model.lBorder m)
                                    uBorder = (updateOpt model.uBorder m)}
          apply model f //TODO performance!
       


