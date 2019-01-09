namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Svgplus.CA
open Svgplus.DA
open Svgplus.RS

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module DiagramApp = 

    type Action =
        | RectStackMessage  of (RectangleStackId * RectangleStack.Action)
        //| HeaderMessage     of Header.Action
        | MouseMove         of V2i
        | ConnectionMessage of ConnectionApp.Action
        | AddStack          of RectangleStack
        

    let init : DiagramApp = 
      {
        rectangleStacks    = HMap.empty
        order              = PList.empty
        connectionApp      = ConnectionApp.init
        rstackGap          = 50.0
        marginLeft         = 50.0
        marginTop          = 50.0
      }

    let sampleInit : DiagramApp =
      let s1 = RectangleStack.initSample (RectangleStackId.newId ())
      let s2 = RectangleStack.initSample (RectangleStackId.newId ())
      let s3 = RectangleStack.initSample (RectangleStackId.newId ())

      let _p1 = (V2d (0.0, 50.0))
      let _p2 = (V2d (RectangleStack.width s1 * 2.0, 50.0))
      let _p3 = (V2d (RectangleStack.width s1 * 4.0, 50.0))
      let s1 = RectangleStack.Lens.pos.Set (s1, _p1)
      let s2 = RectangleStack.Lens.pos.Set (s2, _p2)
      let s3 = RectangleStack.Lens.pos.Set (s3, _p3)

      let smap =
        [
          yield (s1.id, s1)
          yield (s2.id, s2)
          yield (s3.id, s3)
        ] |> HMap.ofList   

      let order = [s1.id;s2.id;s3.id] |> PList.ofList

      //let headers =
      //  [
      //    yield (s1.id, Header.init)
      //    yield (s2.id, Header.init)
      //    yield (s3.id, Header.init)
      //  ] |> HMap.ofList

      {init with 
        rectangleStacks    = smap
        order              = order
        connectionApp      = ConnectionApp.init
      }

    let layout (model : DiagramApp) =
      match model.order.Count > 0 with
        | true ->
          let clean = 
            model.rectangleStacks
              |> HMap.map (fun id s -> RectangleStack.Lens.pos.Set (s, V2d 0.0))
              |> HMap.update (model.order.Item 0) 
                             (fun opts -> RectangleStack.Lens.pos.Set (opts.Value, V2d (model.marginLeft, 0.0))) //hack

          let f (prev : RectangleStack) (curr : RectangleStack) =
            let _pos =
              let cx = model.rstackGap + prev.pos.X + (RectangleStack.width prev)
              V2d (cx,curr.pos.Y)
            RectangleStack.Lens.pos.Set (curr, _pos)

          let _rs = 
            DS.PList.mapPrev' model.order clean None f
          {model with rectangleStacks = _rs}
        | false -> model

    let update (model : DiagramApp) (msg : Action) =
      let updateRect (optr : option<RectangleStack>) (m : RectangleStack.Action) = 
        match optr with
          | Some r -> RectangleStack.update r m
          | None   -> RectangleStack.initSample (RectangleStackId.newId ())

      let updateConnections model msg =
        ConnectionApp.update model.connectionApp 
                              (ConnectionApp.Action.ButtonMessage msg)

      match msg with
        | AddStack r ->
          let _r = model.rectangleStacks.Add (r.id,r)
          let _order = model.order.Append r.id
          {model with
            rectangleStacks = _r
            order           = _order
          } |> layout

        | RectStackMessage msg -> 
          let (id, m) = msg
          let _rects = model.rectangleStacks 
                        |> HMap.update id (fun x -> updateRect x m)
          let _cons  = 
            match msg with 
              | id, RectangleStack.RectangleMessage rm ->
                match rm with
                  | id, Rectangle.Action.NWButtonMessage m -> 
                    updateConnections model m
                  | id, Rectangle.Action.SWButtonMessage m ->
                    updateConnections model m
                  | id, Rectangle.Action.NEButtonMessage m ->
                    updateConnections model m
                  | id, Rectangle.Action.SEButtonMessage m ->
                    updateConnections model m
                  | _ -> model.connectionApp
              | _ -> model.connectionApp
           
          {model with rectangleStacks   = _rects
                      connectionApp     = _cons}

        | ConnectionMessage msg -> 
          {model with connectionApp = ConnectionApp.update model.connectionApp msg}
        | MouseMove p -> 
          {model with connectionApp = ConnectionApp.update model.connectionApp (ConnectionApp.Action.MouseMoved p)}

    let view (model : MDiagramApp) =
      let rectangles = 
        let foo = 
          RectangleStack.view >> UIMapping.mapAListId  
        model.rectangleStacks 
                    |> AMap.map (fun k r -> foo r k RectStackMessage) 
                    |> DS.AMap.toFlatAList

      let connections = 
        (ConnectionApp.view model.connectionApp) 
          |> UIMapping.mapAList (Action.ConnectionMessage)

      let content = rectangles 
                      //|> AList.append headers
                      |> AList.append connections
                      
      content

    let standaloneView (model : MDiagramApp) =

      let rectangles = 
        let foo = 
          RectangleStack.view >> UIMapping.mapAListId  
        model.rectangleStacks 
                    |> AMap.map (fun k r -> foo r k RectStackMessage) 
                    |> DS.AMap.toFlatAList

      let connections = 
        (ConnectionApp.view model.connectionApp) 
          |> UIMapping.mapAList (Action.ConnectionMessage)

        

      let content = rectangles 
                      //|> AList.append headers
                      |> AList.append connections
                      


      let svgAtts = 
        [
          clazz "svgRoot"
          style "border: 1px solid black"
          attribute "xmlns" "http://www.w3.org/2000/svg"
          attribute "preserveAspectRatio" "xMinYMin meet"
          attribute "height" "100%"
          attribute "width" "100%"
          (onMouseMove (fun p -> MouseMove p))
        ] |> AttributeMap.ofList

      require (GUI.CSS.myCss) (
        body [] [
            Incremental.Svg.svg svgAtts content
        ]
      )      