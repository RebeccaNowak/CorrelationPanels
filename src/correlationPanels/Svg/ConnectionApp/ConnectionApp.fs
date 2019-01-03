namespace Svgplus
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ConnectionApp =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open Svgplus.Base
    open Svgplus
    open Svgplus.CA

    type Action =
      | ButtonMessage   of Button.Action
      | MouseMoved      of V2i

    let init : ConnectionApp =
      {
        connections = PList.empty
        connecting  = None
        mouseposition = V2i(0)
      }

    let hasConnection (model : ConnectionApp) (p : IMod<V2d>) =
      let filtered = 
        PList.filter (fun (c : Connection) -> Connection.contains c p) model.connections
      (filtered.Count > 0)

    let deleteConnection (model : ConnectionApp) (p : IMod<V2d>) =
      let _connections =
        model.connections
          |> PList.filter (fun c -> not (Connection.contains c p))
      {model with connections = _connections}

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
            | Button.Action.OnRightClick p ->
                match model.connecting with
                  | Some  pFrom ->
                    {model with connecting = None}
                  | None -> 
                    if hasConnection model p then deleteConnection model p else model
                  
            | _ -> model
        | MouseMoved pos -> {model with mouseposition = pos}
                
    let view (model : MConnectionApp) =
      alist {
        let! connecting = model.connecting
        let! mouse = model.mouseposition
        match connecting with
          | Some p1 ->
            let! p1 = p1
            yield drawLine p1 (V2d mouse) C4b.Black 5.0 
          | None    ->
            ()
        for c in model.connections do
          let! bFrom = c.bFrom
          let! bTo   = c.bTo
          yield drawLine bFrom bTo C4b.Black 5.0 
      }




