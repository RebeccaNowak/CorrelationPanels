namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Svgplus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module DiagramApp = 

    type Action =
        | RectMessage       of (RectangleId * Rectangle.Action)
        | MouseMove         of V2i
        | ConnectionMessage of ConnectionApp.Action

    let init : DiagramApp =
      let r1 = {Rectangle.init (RectangleId.newId ()) with 
                  draw = true
                  colour = {c = C4b.Green}
               }
      let r2 = {Rectangle.init (RectangleId.newId ()) with 
                  draw = true
                  colour = {c = C4b.Blue}
                }
      let r3 = {Rectangle.init (RectangleId.newId ()) with 
                  draw = true
                  colour = {c = C4b.Red}}
      let r2 = Rectangle.Lens.posX.Update (r2, (fun d -> d * 2.0))
      let r3 = Rectangle.Lens.posX.Update (r3, (fun d -> d * 4.0))
      let rmap = HMap.ofList
                  [
                        yield (r1.id, r1)
                        yield (r2.id, r2)
                        yield (r3.id, r3)
                  ]    

      {
        rectangles    = rmap
        connectionApp = ConnectionApp.init
      }

    let update (model : DiagramApp) (msg : Action) =
      let updateRect (optr : option<Rectangle>) (m : Rectangle.Action) = 
        match optr with
          | Some r -> Svgplus.Rectangle.update r m
          | None   -> Rectangle.init (RectangleId.newId ())

      let updateConnections model msg =
        ConnectionApp.update model.connectionApp 
                              (ConnectionApp.Action.ButtonMessage msg)

      match msg with
        | RectMessage msg -> 
          let (id, m) = msg
          let _rects = model.rectangles 
                        |> HMap.update id (fun x -> updateRect x m)
          let _cons  = 
            match msg with 
              | id, Rectangle.Action.NWButtonMessage m -> 
                updateConnections model m
              | id, Rectangle.Action.SWButtonMessage m ->
                updateConnections model m
              | id, Rectangle.Action.NEButtonMessage m ->
                updateConnections model m
              | id, Rectangle.Action.SEButtonMessage m ->
                updateConnections model m
              | _ -> model.connectionApp
           
          {model with rectangles    = _rects
                      connectionApp = _cons}

        | ConnectionMessage msg -> model
        | MouseMove p -> 
          {model with connectionApp = ConnectionApp.update model.connectionApp (ConnectionApp.Action.MouseMoved p)}


    let view (model : MDiagramApp) =
      let foo = 
        Svgplus.Rectangle.view >> UIMapping.mapAListId  

      //let v r =
      //  (Svgplus.Rectangle.view r) 
      //    |> AList.map (fun el -> (UI.map (fun x -> RectMessage (r.id, x)) el))
      let rdnMap = model.rectangles 
                    |> AMap.map (fun k r -> foo r k RectMessage) 

      let ralst = DS.AMap.toFlatAList rdnMap

      let connections = 
        (ConnectionApp.view model.connectionApp) 
          |> UIMapping.mapAList (Action.ConnectionMessage)

      let content = ralst |> AList.append connections
      content