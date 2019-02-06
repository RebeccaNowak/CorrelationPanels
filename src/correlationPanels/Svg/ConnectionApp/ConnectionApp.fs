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
      | ButtonMessage       of Button.Action
      | MouseMoved          of V2d
      | ConnectionMessage   of (ConnectionId * Connection.Action)

    let init : ConnectionApp =
      {
        connections = HMap.empty
        connecting  = None
        mouseposition = V2d(0)
      }

    let hasConnection (model : ConnectionApp) (p : IMod<V2d>) =
      let filtered = 
        model.connections
          |> DS.HMap.values
          |> List.filter (fun (c : Connection) -> Connection.contains c p)
      (filtered.Length > 0)

    let deleteConnection (model : ConnectionApp) (p : IMod<V2d>) =
      let _connections =
        model.connections
          |> HMap.filter (fun id c -> not (Connection.contains c p))
      {model with connections = _connections}

    let update (model : ConnectionApp) (action : Action) =
      match action with
        | ButtonMessage m -> 
          match m with
            | Button.Action.OnLeftClick p ->
                match model.connecting with
                  | Some  pFrom ->
                    let _con = Connection.init (ConnectionId.newId ()) pFrom p
                    {model with connections = (model.connections.Add (_con.id,_con))
                                connecting  = None}
                  | None ->
                    {model with connecting = Some p}
            | Button.Action.OnRightClick p ->
                match model.connecting with
                  | Some  pFrom ->
                    {model with connecting = None}
                  | None -> 
                    if hasConnection model p then deleteConnection model p 
                                             else model
                  
            | _ -> model
        | MouseMoved pos -> {model with mouseposition = pos}
        | ConnectionMessage (c_id,m)  ->
            let contains = HMap.containsKey c_id model.connections
            let _cons =
              if contains 
                then
                  model.connections
                    |> HMap.update c_id (fun el -> Connection.update el.Value m)
                else
                  model.connections
            let _cons =
              match m with
                | Connection.MouseMessage mm ->
                  match mm with 
                    | MouseAction.OnRightClick ->
                        _cons |> HMap.filter (fun id c -> id <> c_id)
                    | _ -> _cons
            {model with connections = _cons}
                
    let view (model : MConnectionApp) =
      let currentLine =
        alist {
          let! connecting = model.connecting
          if connecting.IsSome then
            let mouse = model.mouseposition
            let p1 = connecting.Value
            let! p1 = p1
            let! m = mouse
            yield drawLine p1 m C4b.Black 5.0    
          }
      let oldConnections = 
        let conAList = DS.AMap.valuesToAList model.connections
        alist {
          for c in conAList do
            let domNodeLst = 
              Connection.view c
                |> AList.map (fun el -> el |> UI.map (fun m -> ConnectionMessage (c.id, m)))
            yield! domNodeLst
        }

      AList.append currentLine oldConnections




