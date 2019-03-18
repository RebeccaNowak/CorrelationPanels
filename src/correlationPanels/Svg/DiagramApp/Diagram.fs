namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Svgplus.RectangleType
open Svgplus.CA
open Svgplus.DA
open UIPlus
open SimpleTypes
open Svgplus.DiagramItemType

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Diagram = 

    type Action =
      | DiagramItemMessage    of (DiagramItemId * DiagramItem.Action)
      //| RectStackMessage      of (RectangleStackTypes.RectangleStackId * RectangleStack.Action)
      | MouseMove             of V2d
      | ConnectionMessage     of ConnectionApp.Action
      | AddItem              of DiagramItem
      | DeleteStack           of DiagramItemId
      | MoveLeft              of DiagramItemId
      | MoveRight             of DiagramItemId
      | UpdateColour          of (Rectangle -> Rectangle) //(ColourMap * CMItemId)
      | UpdateRectangle       of (RectangleIdentification * Rectangle.Action)
      | UpdateYSizes          of (float -> float)
      | UpdateXSizes          of (float -> float)

    type UnpackAction =
      | MouseMessage      of MouseAction
      | RectangleMessage  of Rectangle.Action
      | LeftArrowMessage  of Arrow.Action
      | RightArrowMessage of Arrow.Action
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UnpackAction =
      let OnLeftClick =
        UnpackAction.MouseMessage MouseAction.OnLeftClick
      let SelectRectangle =
        UnpackAction.RectangleMessage (Rectangle.Action.Select RectangleId.invalid)
      let SelectUpperBorder =
        UnpackAction.RectangleMessage (Rectangle.Action.SelectUpperBorder)
      let SelectLowerBorder =
        UnpackAction.RectangleMessage (Rectangle.Action.SelectLowerBorder)
           
          
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Action =
      let unpack (fromAction : Action) (toAction : UnpackAction) f (def : 'model) =
        match fromAction, toAction with
        | DiagramItemMessage (itemId, im), _ ->
          match im, toAction with
          | DiagramItem.HeaderMessage hm, _ ->
            match hm, toAction with
              | Header.TextMessage tm, toAction ->
                match tm, toAction with 
                  | Text.MouseMessage mm, MouseMessage mm2 ->
                    match mm, mm2 with 
                      | MouseAction.OnLeftClick, MouseAction.OnLeftClick -> 
                          f itemId RectangleId.invalid
                      //| MouseAction.OnMouseDown Aardvark.Application.MouseButtons.Left, MouseAction.OnLeftClick -> 
                      //    f itemId RectangleId.invalid
                      | MouseAction.OnMouseEnter, MouseAction.OnMouseEnter ->
                          def
                      | _ -> def
                  | _ -> def
              | _,_ -> def
          | DiagramItem.RectangleStackMessage (stackId,ra), _ -> 
            match ra, toAction with 
              | RectangleStack.HeaderMessage sm, toAction ->
                match sm, toAction with
                  | Header.TextMessage tm, toAction ->
                    match tm, toAction with 
                      | Text.MouseMessage mm, MouseMessage mm2 ->
                        match mm, mm2 with 
                          | MouseAction.OnLeftClick, MouseAction.OnLeftClick -> 
                              f itemId RectangleId.invalid
                          | MouseAction.OnMouseEnter, MouseAction.OnMouseEnter ->
                              def
                          | _ -> def
                      | _ -> def
                  | Header.LeftArrowMessage sm, LeftArrowMessage _lam ->
                    match sm = _lam with 
                      | true -> f itemId RectangleId.invalid
                      | false -> def
                  | Header.RightArrowMessage sm, RightArrowMessage _ram ->
                    match sm = _ram with 
                       | true -> f itemId RectangleId.invalid
                       | false -> def
                  | _,_ -> def
              | RectangleStack.RectangleMessage (rid, rm), RectangleMessage rm2 -> 
                match rm, rm2 with
                  | Rectangle.Select rid,  Rectangle.Select dummy ->
                    f itemId rid
                  | Rectangle.SelectLowerBorder, Rectangle.SelectLowerBorder ->
                    f itemId rid
                  | Rectangle.SelectUpperBorder, Rectangle.SelectUpperBorder ->
                    f itemId rid
                  | _,_ -> def
                | _ -> def       
          | _ -> def
        | _ -> def

       
    let init dataToY yToData : Diagram = 
      {
        items              = HMap.empty
        order              = PList.empty
        connectionApp      = ConnectionApp.init
        itemGap            = 50.0
        marginLeft         = 50.0
        marginTop          = 50.0
        selectedRectangle  = None
        dataToY            = dataToY
        yToData            = yToData
        dataRange          = Rangef.init
      }

    let sampleInit : Diagram =
      let s1 = RectangleStack.initSample (RectangleStackTypes.RectangleStackId.newId ())
      let s2 = RectangleStack.initSample (RectangleStackTypes.RectangleStackId.newId ())
      let s3 = RectangleStack.initSample (RectangleStackTypes.RectangleStackId.newId ())

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

      let item = DiagramItem.init smap order
      let items = HMap.single item.id item

      {init (fun x -> x) (fun x -> x) with 
        items              = items
        order              = PList.ofList [item.id]
        connectionApp      = ConnectionApp.init
      }

    let layout (model : Diagram) =
      let setFirstItemPosition (optItem : option<DiagramItem>)  =
        Svgplus.DiagramItem.Lens.pos.Set 
          (optItem.Value, V2d (model.marginLeft, model.marginTop))
        
      let cleanItems items =
        items
          |> HMap.map (fun id s -> DiagramItem.resetPosition s (V2d 0.0))
          |> HMap.map (fun k item -> DiagramItem.layout item)
          |> HMap.update (model.order.Item 0) 
                         (fun opts -> setFirstItemPosition opts) //hack


      let f (prev : DiagramItem) (curr : DiagramItem) =
        let _pos =
          let cx = model.itemGap + prev.pos.X + prev.maxWidth
          V2d (cx, model.marginTop)
        Svgplus.DiagramItem.Lens.pos.Set (curr, _pos)

      let _items = 
        match model.order.Count > 0 with
          | true ->
            let _items = cleanItems model.items
            let _rs = 
              DS.PList.mapPrev' model.order _items None f
            _rs 
          | false -> model.items

      {model with items = _items}

    //let findRectangle_M (model : MDiagram) (id : RectangleId) =
    //  let optList = 
    //    AMap.map
    //      (fun sid x -> 
    //        RectangleStack.tryfindRectangle_M x id)
    //      model.rectangleStacks

    //  let vals = DS.AMap.valuesToAList optList
    //  let filtered = 
    //    alist {
    //      for s in vals do
    //        let! s = s
    //        if s.IsSome then yield s
    //    }
        
        
    //  let len = AList.count filtered

    //  adaptive {
    //    let! len = len
    //    let! h = DS.AList.tryHead filtered
    //    return 
    //      match len with
    //        | len when len <= 0 -> None
    //        | len when len = 1  -> Option.flatten h
    //        | len when len >= 2 -> None //TODO error message
    //        | _ -> None
    //  }

    let tryFindRectangleFromId (model : Diagram) (rid : RectangleId) =
      let foundmany =
        model.items
          |> HMap.map (fun key item -> DiagramItem.tryFindRectangleFromId item rid)
          |> DS.HMap.filterNone
      let rectangle =
        foundmany
          |> DS.HMap.values
          |> List.tryHead
      rectangle



    let tryFindRectangle  (model : Diagram) 
                          (selRect : RectangleIdentification) =
      let optstack = HMap.tryFind selRect.diagramItemId model.items
      Option.bind (fun s -> (DiagramItem.tryFindRectangle s selRect)) optstack

    let updateItemFromId  (model : Diagram) 
                          (id    : DiagramItemId) 
                          (m     : DiagramItem.Action) =
      let _items =
        model.items
          |> HMap.update id (fun optm -> DiagramItem.updateOptItem optm m)
      {model with items = _items}

    let updateSelectedRectangle (model  : Diagram)
                                (iid    : DiagramItemId)
                                (m      : DiagramItem.Action) =
      let (_sel, _model) = 
        match m with 
        | DiagramItem.RectangleStackMessage (sid, sm) ->
          match sm with
          | RectangleStack.RectangleMessage (rid, m) -> 
            match m with
              | Rectangle.Select rid ->
                match model.selectedRectangle with
                  | Some selr -> 
                    let oldr = selr.rid
                    let olds = selr.stackid
                    let _m = DiagramItem.Action.RectangleStackMessage 
                              (olds, RectangleStack.RectangleMessage (oldr, (Rectangle.Deselect oldr)))
                    let rstacks = updateItemFromId model iid _m
                    (Some {rid = rid; stackid = sid; diagramItemId = iid}, rstacks)
                  | None ->
                    (Some {rid = rid; stackid = sid; diagramItemId = iid}, model)
              | _ -> (model.selectedRectangle, model)
          | _ -> (model.selectedRectangle, model)
        | _ -> (model.selectedRectangle, model)
      {
        _model with selectedRectangle = _sel
      }

    let moveItemLeft (model : Diagram) id = 
      let _order = 
        model.order 
          |> PList.toList
          |> DS.List.shiftLeft id 
          |> PList.ofList
      {model with order = _order }

    let moveItemRight (model : Diagram) id =
      let _order = 
        model.order 
          |> PList.toList
          |> DS.List.shiftRight id 
          |> PList.ofList
      {model with order = _order }

    let update (model : Diagram) (msg : Action) =
      let updateConnections model message = 
        let upd model msg =
          ConnectionApp.update model.connectionApp 
                               (ConnectionApp.Action.ButtonMessage msg)
        match message with 
          | DiagramItem.RectangleStackMessage (sid, sm) ->
            match sm with 
            | RectangleStack.RectangleMessage rm ->
              match rm with
                | id, Rectangle.NWButtonMessage m 
                | id, Rectangle.SWButtonMessage m 
                | id, Rectangle.NEButtonMessage m 
                | id, Rectangle.SEButtonMessage m ->
                  upd model m
                | _ -> model.connectionApp
            | _ -> model.connectionApp
          | _ -> model.connectionApp

      match msg with
        | AddItem r ->
          let _r = model.items.Add (r.id,r)
          let _order = model.order.Append r.id
          {model with
            items = _r
            order = _order
          } |> layout

        | DeleteStack sid ->
          let _stacks = model.items.Remove sid
          let _order  = model.order.Remove sid
          {model with
            items         = _stacks
            order           = _order
          } |> layout          

        | DiagramItemMessage msg -> 
          let (iid, m) = msg
          let _model = 
            match m with 
              | DiagramItem.HeaderMessage hm ->
                match hm with
                  | Header.LeftArrowMessage lam ->
                    match lam with
                      | Arrow.MouseMessage mm ->
                        match mm with 
                          | MouseAction.OnLeftClick -> 
                            moveItemLeft model iid
                          | _ -> model
                      | _ -> model
                  | Header.RightArrowMessage lam ->
                    match lam with
                      | Arrow.MouseMessage mm ->
                        match mm with 
                          | MouseAction.OnLeftClick -> 
                            moveItemRight model iid
                          | _ -> model
                      | _ -> model
                  | _ -> model
              | _ -> model
                

          let _model = updateSelectedRectangle _model iid m
          let _rstacks = _model.items
                          |> HMap.update iid (fun x -> DiagramItem.updateOptItem x m)
          let _cons  = updateConnections model m
          let __model = 
            {_model with items   = _rstacks
                         connectionApp     = _cons}
          layout __model
          //match needsLayouting with
          //  | true -> 
          //    layout __model
          //  | false -> __model
                
        | ConnectionMessage msg -> 
          {model with connectionApp = ConnectionApp.update model.connectionApp msg}
        | MouseMove p -> 
          let _conApp = ConnectionApp.update 
                          model.connectionApp 
                          (ConnectionApp.Action.MouseMoved p)
          {model with connectionApp = _conApp}
        | MoveLeft id ->
          moveItemLeft model id
        | MoveRight id ->
          moveItemRight model id
        | UpdateColour rectFun -> //(cmap, _id) ->
          let _stacks =
            model.items
              |> HMap.map (fun id r -> DiagramItem.update r (DiagramItem.UpdateColour rectFun)) //(cmap, _id)))
          {model with items = _stacks}
        | UpdateRectangle (id, m) ->
            let stackMessage = 
              DiagramItem.RectangleStackMessage
                ( id.stackid, RectangleStack.RectangleMessage (id.rid, m))
            let _model = 
              updateItemFromId model id.diagramItemId stackMessage
            _model
        | UpdateYSizes f ->
          let _stacks =
            model.items
                |> HMap.map (fun id r -> DiagramItem.update r (DiagramItem.UpdateYSizes f) )
          {model with items = _stacks} |> layout
        | UpdateXSizes f ->
          let _stacks =
            model.items
                |> HMap.map (fun id r -> DiagramItem.update r (DiagramItem.UpdateXSizes f) )
          {model with items = _stacks} |> layout            
        
            
    let updateRectangleFromId model rid m =
      let opt = tryFindRectangleFromId model rid
      match opt with
      | Some (id, r) -> 
        let rm =
          DiagramItemMessage 
            (id.diagramItemId, DiagramItem.RectangleStackMessage 
                                    (id.stackid, (RectangleStack.RectangleMessage (id.rid, m))))
        update model rm
      | None -> model

    let findStackFromId (model : Diagram) sid =
      let foundmany =
        model.items
          |> HMap.map (fun key item -> (DiagramItem.tryFindStackFromId item sid)
                                          |> Option.map (fun x -> (key, x)))
          |> DS.HMap.filterNone
      let rectangleStack =
        foundmany
          |> DS.HMap.values
          |> List.tryHead
      rectangleStack

    let updateStackFromId (model : Diagram) sid m =
      let opt = findStackFromId model sid
      let _items = 
        match opt with
        | Some (iid, stack) -> 
          let itemMessage = DiagramItem.RectangleStackMessage m
          model.items 
            |> HMap.update iid (fun item -> DiagramItem.updateOptItem item itemMessage)
        | None -> model.items
      {model with items = _items}
    
    let view (model : MDiagram) =
      let stacks = 
        alist {
          for id in model.order do
            let! stack = 
              (AMap.find id model.items)
            let stackView =
              DiagramItem.view stack
            yield! (UIMapping.mapAListId stackView id Action.DiagramItemMessage)
        }

      let connections = 
        (ConnectionApp.view model.connectionApp) 
          |> UIMapping.mapAList (Action.ConnectionMessage)

      let content = stacks 
                      |> AList.append connections
                      
      content



    let standaloneView (model : MDiagram) =
      let rectangles = 

        alist {
          for id in model.order do
            let! rstack = AMap.find id model.items
            let vlst = DiagramItem.view rstack
            let mapped = vlst |> AList.map (fun el -> UI.map (fun a -> Action.DiagramItemMessage (id, a)) el)
            
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