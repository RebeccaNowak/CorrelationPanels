namespace Svgplus
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ConnectionApp =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open Svgplus.Base
    open SimpleTypes

    type Action =
      | ButtonMessage   of Button.Action
      | MouseMoved      of V2i

    let init : ConnectionApp =
      {
        connections = PList.empty
        connecting  = None
        mouseposition = V2i(0)
      }

    let update (model : ConnectionApp) (action : Action) =
      match action with
        | ButtonMessage m -> 
          match m with
            | Button.Action.OnLeftClick p ->
                match model.connecting with
                  | Some  pFrom ->
                    {model with connections = (model.connections.Append {bFrom = pFrom; bTo = p})
                                connecting  = None}
                  | None ->
                    {model with connecting = Some p}
            | _ -> model
        | MouseMoved pos -> {model with mouseposition = pos}
                
    let view (model : MConnectionApp) =
      alist {
        let! connecting = model.connecting
        let! mouse = model.mouseposition
        match connecting with
          | Some p1 ->
            yield drawLine p1 (V2d mouse) C4b.Black 5.0 
          | None    ->
            ()
        for c in model.connections do
          yield drawLine c.bFrom c.bTo C4b.Black 5.0 
      }




