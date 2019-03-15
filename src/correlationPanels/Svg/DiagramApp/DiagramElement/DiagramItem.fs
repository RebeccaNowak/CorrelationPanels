namespace Svgplus
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

open Svgplus.RectangleType
open Svgplus.RectangleStackTypes
open Svgplus.CA
open Svgplus
open Svgplus.HeaderType
open UIPlus
open Svgplus.AxesTypes
open SimpleTypes
open Svgplus.DiagramItemType

  module DiagramItem =

    type Action = 
      | RectangleStackMessage of (RectangleStackId * RectangleStack.Action)
      | HeaderMessage         of Header.Action
      | ChangeLabel           of TextInput.Action
      | Delete
      | AddStack              of RectangleStackTypes.RectangleStack
      | DeleteStack           of RectangleStackTypes.RectangleStackId
      | MoveLeft              of RectangleStackTypes.RectangleStackId
      | MoveRight             of RectangleStackTypes.RectangleStackId
      | UpdateColour          of (ColourMap * CMItemId)
      | UpdateRectangle       of (RectangleIdentification * Rectangle.Action)
      | UpdateYSizes          of (float -> float)
      | UpdateXSizes          of (float -> float)



    let layout (model : DiagramItem) =
      let _stacks = 
        match model.order.Count > 0 with
        | true ->
          let cleanItems = 
            model.rectangleStacks
              |> HMap.map (fun id s -> RectangleStack.resetPosition s (V2d 0.0))
              |> HMap.update (model.order.Item 0) 
                             (fun opts -> RectangleStack.Lens.pos.Set 
                                            (opts.Value, V2d (model.pos.X + model.marginLeft, 
                                                              model.pos.Y + model.marginTop)
                                            )
                             ) //hack            
          let f (prev : RectangleStackTypes.RectangleStack) (curr : RectangleStackTypes.RectangleStack) =
            let _pos =
              let rightGap = 
                match HMap.tryFind prev.id model.rightGaps with
                | Some g -> g
                | None   -> 10.0 // TODO hardcoded default
              let cx = rightGap + prev.pos.X + (RectangleStack.width prev)
              V2d (cx,model.marginTop + model.pos.Y)
            RectangleStack.Lens.pos.Set (curr, _pos)

          let _rs = 
            DS.PList.mapPrev' model.order cleanItems None f
          _rs
        | false -> model.rectangleStacks

      let _model = {model with rectangleStacks = _stacks}
      let maxWidth = _model.maxWidth
      let _header = 
        {model.header with centre = V2d(model.pos.X + maxWidth * 0.5, model.pos.Y)
        } |> Header.layout false
      {_model with header = _header}

    let resetPosition (model : DiagramItem) (v : V2d) =
      let _stacks = 
        model.rectangleStacks
          |> HMap.map (fun id r -> RectangleStack.resetPosition r v)
      let _header = Header.Lens.pos.Set (model.header, v)      
      {model with pos             = v
                  rectangleStacks = _stacks
                  header          = _header}
                  
    module Lens =
      let pos =
        { new Lens<DiagramItem, Aardvark.Base.V2d>() with
            override x.Get(s) = s.pos
            override x.Set(item,v) =
              let _stacks = 
                item.rectangleStacks 
                  |> HMap.map (fun id stack -> 
                                  let _x = v.X + stack.pos.X
                                  let _y = v.Y + stack.pos.Y + item.header.dim.height
                                  let _v = V2d (_x, _y)
                                  RectangleStack.Lens.pos.Set (stack, _v))
              let _header = Header.Lens.pos.Set (item.header, v) 
              {
                  item with  rectangleStacks = _stacks
                             header          = _header
                             pos             = v
              }
              
            override x.Update(s,f) =
              let v = f s.pos
              let _stacks = 
                s.rectangleStacks 
                  |> HMap.map (fun id r -> 
                                let _x = v.X + r.pos.X
                                let _y = v.Y + r.pos.Y
                                let _v = V2d (_x, _y)
                                RectangleStack.Lens.pos.Set (r, _v))
              let _header = Header.Lens.pos.Set (s.header, v) 
              {
                s with  rectangleStacks = _stacks
                        header     = _header
                        pos        = v
              }
        }

    let tryFindStackFromId (model : DiagramItem) 
                           (stackId : RectangleStackId) =
      model.rectangleStacks |> HMap.tryFind stackId 

    let updateStackFromId (model : DiagramItem) 
                          (stackId : RectangleStackId) 
                          (m : RectangleStack.Action) =
      model.rectangleStacks 
        |> HMap.update stackId (fun x -> RectangleStack.updateOptRStack x m)

    let tryFindRectangleFromId (model : DiagramItem) (rid : RectangleId) =
      let foundmany = 
        HMap.map (fun sid rs -> 
                    match ((RectangleStack.tryfindRectangle rs rid) ) with
                      | Some r -> Some (sid, r)
                      | None   -> None
                 ) model.rectangleStacks
          |> DS.HMap.filterNone
      let res =
        foundmany
          |> DS.HMap.values
          |> List.tryHead

      
      res 
        |> Option.map (fun (stackid, r) ->
                          let ids : RectangleIdentification =    
                            {
                              rid           = r.id
                              stackid       = stackid
                              diagramItemId = model.id
                            }
                          (ids, r))

    let tryFindRectangle  (model : DiagramItem) 
                          (selRect : RectangleIdentification) =
      let optstack = HMap.tryFind selRect.stackid model.rectangleStacks
      Option.bind (fun s -> (RectangleStack.tryfindRectangle s selRect.rid)) optstack


    let updateRStackFromId  (model : DiagramItem) 
                            (id : RectangleStackId) 
                            (m : RectangleStack.Action) =
      model.rectangleStacks
        |> HMap.update id (fun optr -> RectangleStack.updateOptRStack optr m)

    let init stacks order : DiagramItem =
      {
        id                = DiagramItemId.newId ()
                          
        pos               = V2d(0.0)
        header            = Header.init
                          
        rectangleStacks   = stacks
        order             = order
        rightGaps         = HMap.ofSeq (stacks.Values 
                                            |> Seq.map (fun x -> (x.id, 0.0)))
                          
        marginLeft        = 0.0
        marginRight       = 0.0
        marginTop         = 0.0
      }

    let update (model : DiagramItem) (action : Action) =
      let updateStack (optr : option<RectangleStack>) (m : RectangleStack.Action) = 
        match optr with
          | Some r -> Svgplus.RectangleStack.update r m
          | None   -> RectangleStack.initDummy // TODO dirty hack

      match action with
      | RectangleStackMessage (id, a) ->
        let _stacks = 
          model.rectangleStacks
            |> HMap.update id (fun x -> updateStack x a)
        {model with rectangleStacks     = _stacks} |> layout
      | HeaderMessage m ->
        let _header = Header.update model.header m
        {model with header = _header}
      | ChangeLabel msg ->
        let _header = Header.update model.header (Header.TextMessage (Text.ChangeLabel msg))
        {model with header = _header}
      | Delete ->
        {model with rectangleStacks = HMap.empty
                    header          = {model.header with visible = false}
        }
      | AddStack r ->
        let _r = model.rectangleStacks.Add (r.id,r)
        let _order = model.order.Append r.id
        {model with
          rectangleStacks = _r
          order           = _order
        } |> layout

      | DeleteStack sid ->
        let _stacks = model.rectangleStacks.Remove sid
        let _order  = model.order.Remove sid
        {model with
          rectangleStacks = _stacks
          order           = _order
        } |> layout          
      | UpdateColour (cmap, _id) ->
        let _stacks =
          model.rectangleStacks
            |> HMap.map (fun id r -> RectangleStack.update r (RectangleStack.UpdateColour (cmap, _id)))
        {model with rectangleStacks = _stacks}
      | UpdateRectangle (id, m) ->
          let stackMessage = 
            RectangleStack.RectangleMessage 
              (id.rid, m)
          let _stacks = 
            updateRStackFromId model id.stackid stackMessage
          {model with rectangleStacks = _stacks}
      | UpdateYSizes f ->
        let _stacks =
          model.rectangleStacks
              |> HMap.map (fun id r -> RectangleStack.update r (RectangleStack.UpdateYSizes f) )
        {model with rectangleStacks = _stacks} |> layout
      | UpdateXSizes f ->
        let _stacks =
          model.rectangleStacks
              |> HMap.map (fun id r -> RectangleStack.update r (RectangleStack.UpdateXSizes f) )
        {model with rectangleStacks = _stacks} |> layout   

    let updateOptItem (model : option<DiagramItem>) (action : Action) =
      match model with
        | Some r -> update r action
        | None   -> init HMap.empty PList.empty
        
    let view (model : MDiagramItem) =
      let stacks = 
        alist {
          for id in model.order do
            let! stack = 
              (AMap.find id model.rectangleStacks)
            let stackView =
              RectangleStack.view stack
            yield! (UIMapping.mapAListId stackView id Action.RectangleStackMessage)
        }
      let header =
        (Header.view model.header) |> UIMapping.mapAList HeaderMessage
      let content = AList.append header stacks
      content
