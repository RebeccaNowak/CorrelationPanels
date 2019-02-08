namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Svgplus.RectangleType
open Svgplus.CA
open Svgplus.DA
open Svgplus.RectangleStackTypes
open UIPlus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module DiagramApp = 

    type Action =
      | RectStackMessage  of (RectangleStackId * RectangleStack.Action)
      //| HeaderMessage     of Header.Action
      | MouseMove         of V2d
      | ConnectionMessage of ConnectionApp.Action
      | AddStack          of RectangleStack
      | MoveLeft          of RectangleStackId
      | MoveRight         of RectangleStackId
      | UpdateColour      of ColourMap


        
    let init : DiagramApp = 
      {
        rectangleStacks    = HMap.empty
        order              = PList.empty
        connectionApp      = ConnectionApp.init
        rstackGap          = 50.0
        marginLeft         = 50.0
        marginTop          = 50.0
        selectedRectangle  = None
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
              |> HMap.map (fun id s -> RectangleStack.resetPosition s (V2d 0.0))
              |> HMap.update (model.order.Item 0) 
                             (fun opts -> RectangleStack.Lens.pos.Set (opts.Value, V2d (model.marginLeft, model.marginTop))) //hack

          let f (prev : RectangleStack) (curr : RectangleStack) =
            let _pos =
              let cx = model.rstackGap + prev.pos.X + (RectangleStack.width prev)
              V2d (cx,model.marginTop)
            RectangleStack.Lens.pos.Set (curr, _pos)

          let _rs = 
            DS.PList.mapPrev' model.order clean None f
          {model with rectangleStacks = _rs}
        | false -> model

    let findRectangle_M (model : MDiagramApp) (id : RectangleId) =
      let optList = 
        AMap.map
          (fun sid x -> 
            RectangleStack.tryfindRectangle_M x id)
          model.rectangleStacks

      let vals = DS.AMap.valuesToAList optList
      let filtered = 
        alist {
          for s in vals do
            let! s = s
            if s.IsSome then yield s
        }
        
        
      let len = AList.count filtered

      adaptive {
        let! len = len
        let! h = DS.AList.tryHead filtered
        return 
          match len with
            | len when len <= 0 -> None
            | len when len = 1  -> Option.flatten h
            | len when len >= 2 -> None //TODO error message
            | _ -> None
      }

    let tryFindRectangleFromId (model : DiagramApp) (rid : RectangleId) =
      let foundmany = 
        HMap.map (fun sid rs -> 
                    match ((RectangleStack.tryfindRectangle rs rid) ) with
                      | Some r -> Some (sid, r)
                      | None   -> None
                 ) model.rectangleStacks
          |> DS.HMap.filterNone
      foundmany
        |> DS.HMap.values
        |> List.tryHead
      
    let tryFindRectangle  (model : DiagramApp) 
                          (sid : RectangleStackId) 
                          (rid : RectangleId) =
      let optstack = HMap.tryFind sid model.rectangleStacks
      Option.bind (fun s -> (RectangleStack.tryfindRectangle s rid)) optstack

     

    let update (model : DiagramApp) (msg : Action) =
      let updateRStack (optr : option<RectangleStack>) (m : RectangleStack.Action) = 
        match optr with
          | Some r -> RectangleStack.update r m
          | None   -> RectangleStack.initSample (RectangleStackId.newId ())

      let updateRStackFromId (id : RectangleStackId) (m : RectangleStack.Action) =
        model.rectangleStacks
         |> HMap.update id (fun optr -> updateRStack optr m)

      let updateConnections model message = 
        let upd model msg =
          ConnectionApp.update model.connectionApp 
                               (ConnectionApp.Action.ButtonMessage msg)
        match message with 
            | id, RectangleStack.RectangleMessage rm ->
              match rm with
                | id, Rectangle.NWButtonMessage m -> 
                  upd model m
                | id, Rectangle.SWButtonMessage m ->
                  upd model m
                | id, Rectangle.NEButtonMessage m ->
                  upd model m
                | id, Rectangle.SEButtonMessage m ->
                  upd model m
                | _ -> model.connectionApp
            | _ -> model.connectionApp

      match msg with
        | AddStack r ->
          let _r = model.rectangleStacks.Add (r.id,r)
          let _order = model.order.Append r.id
          {model with
            rectangleStacks = _r
            order           = _order
          } |> layout

        | RectStackMessage msg -> 
          let (sid, m) = msg
          let (_sel, rstacks) = 
            match m with 
              | RectangleStack.RectangleMessage (rid, m) -> 
                match m with
                  | Rectangle.Select rid ->
                    match model.selectedRectangle with
                      | Some (oldr, olds) -> 
                        let _m = RectangleStack.RectangleMessage (oldr, (Rectangle.Deselect oldr))
                        let rstacks = updateRStackFromId olds _m
                        (Some (rid, sid), rstacks)
                      | None ->
                        (Some (rid, sid), model.rectangleStacks)
                    
                  | _ -> 
                    (model.selectedRectangle, model.rectangleStacks)
              | _ -> (model.selectedRectangle, model.rectangleStacks)

          let _rstacks = rstacks 
                          |> HMap.update sid (fun x -> updateRStack x m)
          let _cons  = updateConnections model msg
          {model with rectangleStacks   = _rstacks
                      connectionApp     = _cons
                      selectedRectangle = _sel}

        | ConnectionMessage msg -> 
          {model with connectionApp = ConnectionApp.update model.connectionApp msg}
        | MouseMove p -> 
          let _conApp = ConnectionApp.update 
                          model.connectionApp 
                          (ConnectionApp.Action.MouseMoved p)
          {model with connectionApp = _conApp}
        | MoveLeft id ->
          let _order = 
            model.order 
              |> PList.toList
              |> DS.List.shiftLeft id 
              |> PList.ofList
          {model with order = _order }
        | MoveRight id ->
          let _order = 
            model.order 
              |> PList.toList
              |> DS.List.shiftRight id 
              |> PList.ofList
          {model with order = _order }
        | UpdateColour cmap ->
          let _stacks =
            model.rectangleStacks
              |> HMap.map (fun id r -> RectangleStack.update r (RectangleStack.UpdateColour cmap) )
          {model with rectangleStacks = _stacks}


    let view (model : MDiagramApp) =
      //let foo = 
      //    RectangleStack.view >> UIMapping.mapAListId  
      let stacks = 
        alist {
          for id in model.order do
            let! stack = 
              (AMap.find id model.rectangleStacks)
            let stackView =
              RectangleStack.view stack
            yield! (UIMapping.mapAListId stackView id Action.RectStackMessage)
        }


      let connections = 
        (ConnectionApp.view model.connectionApp) 
          |> UIMapping.mapAList (Action.ConnectionMessage)

      let content = stacks 
                      |> AList.append connections
                      
      content

    let standaloneView (model : MDiagramApp) =

      let rectangles = 

        alist {
          for id in model.order do
            let! rstack = AMap.find id model.rectangleStacks
            let vlst = RectangleStack.view rstack
            let mapped = vlst |> AList.map (fun el -> UI.map (fun a -> Action.RectStackMessage (id, a)) el)
            
            yield! mapped
        }

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
          (onMouseMove (fun p -> MouseMove (V2d p)))
        ] |> AttributeMap.ofList

      require (GUI.CSS.myCss) (
        body [] [
          div [attribute "contenteditable" "true"]
              [
                Incremental.Svg.svg svgAtts content
              ]
        ]
      )      