namespace Svgplus.CA

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Connection =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open Svgplus

    type Action =
      | MouseMessage of MouseAction

    let contains (model : Connection) (v : IMod<V2d>) =
      (model.bTo == v) || (model.bFrom == v)

    let init id pFrom p: Connection = 
      {
        id = id
        bFrom = pFrom
        bTo = p
        colour = C4b.Black
        dotted = false
        weight = 5.0
        dashLength = 4.0
        dashDist   = 1.0
      }

    let update (model : Connection) (a : Action) =
      match a with
        | MouseMessage mm ->
          match mm with
            | OnRightClick  -> model
            | OnLeftClick   -> {model with dotted = not model.dotted}
            | OnMouseDown b -> model
            | OnMouseUp  b  -> model
            | OnMouseEnter  -> {model with weight = 8.0}
            | OnMouseLeave  -> {model with weight = 5.0}
         

    let view (model : MConnection) =
      let actions = MouseActions.init ()
      alist {
        let! fr = model.bFrom
        let! t  = model.bTo
        let! dotted = model.dotted
        let dNode = 
          match dotted with
            | true ->
              (Incremental.drawDottedLine fr t model.colour model.weight model.dashLength model.dashDist actions)
            | false ->
              (Incremental.drawLine fr t model.colour model.weight actions)
              
                       
        
        yield dNode |> UI.map MouseMessage
      }
      
