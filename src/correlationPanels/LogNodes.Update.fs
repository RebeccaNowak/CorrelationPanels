namespace CorrelationDrawing.LogNodes

 open CorrelationDrawing
 
 module Update = 
    open Svg
    open Recursive

    let update  (action : Action) (model : LogNode) =
      match action with
        | ChangeXAxis (id, scaleFactor) -> calcSizeX model id scaleFactor
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
       


