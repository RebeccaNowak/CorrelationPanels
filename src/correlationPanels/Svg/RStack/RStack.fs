namespace Svgplus

  open System
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus.RectangleType // always import this before importing Svgplus, so correct Lenses are used //TODO
  open Svgplus.RectangleStackTypes
  open Svgplus



  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module RectangleStack =
    open System.Windows.Interop
    open Aardvark.SceneGraph
    open UIPlus
    open OpenTK.Graphics.OpenGL

    type Action =
      | RectangleMessage of (RectangleId * Rectangle.Action)
      | ChangeLabel      of TextInput.Action
      | ResetPosition    of V2d
      | AddRectangle     of Rectangle
      | HeaderMessage    of Header.Action
      | UpdatePosition   of V2d
      | UpdateColour     of ColourMap
      | UpdateYSizes     of (float -> float)
      | UpdateXSizes     of (float -> float)
      | Delete

    let stack (model : RectangleStack) =
      match model.order.Count > 0 with
        | true ->
          let clean = 
            model.rectangles
              |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, V2d 0.0))
          

          let f (prev : Rectangle) (curr : Rectangle) =
            let cposy = prev.pos.Y + prev.dim.height
            Rectangle.Lens.posY.Set (curr, cposy)

          let _rs = 
            DS.PList.mapPrev' model.order clean None f

          let maxWidth = model.rectangles 
                          |> DS.HMap.values
                          |> List.map (fun r -> r.dim.width)
                          |> List.max

          let _header = Header.Lens.width.Set (model.header, maxWidth)
          {
            model with  rectangles = _rs
                        header     = _header  
          }
        | false -> model

    let init id rmap order : RectangleStack =
      let header = Header.init

      let rs = 
        {
          id            = id
          needsLayouting = false
          rectangles    = rmap
          header        = header
          order         = order
          pos           = V2d.OO
        } 
      
      let stacked = rs |> stack
      stacked

    let initSample id : RectangleStack =
      let header = Header.init

      let r1 = {Rectangle.init (RectangleId.newId ()) with 
                  draw   = true
                  colour = {c = C4b.Green}
                }
      
      // make room for header
      let r1 = Rectangle.Lens.posY.Set (r1, Header.Lens.height.Get header)

      let r2 = {Rectangle.init (RectangleId.newId ()) with 
                  draw = true
                  colour = {c = C4b.Blue}
                }
      let r3 = {Rectangle.init (RectangleId.newId ()) with 
                  draw = true
                  colour = {c = C4b.Red}}

      let order = [r1.id;r2.id;r3.id]
      let rmap = HMap.ofList
                  [
                        yield (r1.id, r1)
                        yield (r2.id, r2)
                        yield (r3.id, r3)
                  ]    

      let rs = 
        {
          id            = id
          needsLayouting = false
          rectangles    = rmap
          header        = header
          order         = PList.ofList order
          pos           = V2d.OO
        } 
      
      let stacked = rs |> stack
      stacked

    module Lens =
      let pos =
        { new Lens<RectangleStack, Aardvark.Base.V2d>() with
            override x.Get(s) = s.pos
            override x.Set(s,v) =
              let _rectangles = 
                s.rectangles 
                  |> HMap.map (fun id r -> 
                                  let _x = v.X
                                  let _y = v.Y + r.pos.Y + s.header.dim.height
                                  let _v = V2d (_x, _y)
                                  Rectangle.Lens.pos.Set (r, _v))
              let _header = Header.Lens.pos.Set (s.header, v) 
              {
                  s with  rectangles = _rectangles
                          header     = _header
                          pos        = v
              }
            override x.Update(s,f) =
              let v = f s.pos
              let _rectangles = 
                s.rectangles 
                  |> HMap.map (fun id r -> 
                                let _x = v.X
                                let _y = v.Y + r.pos.Y
                                let _v = V2d (_x, _y)
                                Rectangle.Lens.pos.Set (r, _v))
              let _header = Header.Lens.pos.Set (s.header, v) 
              {
                s with  rectangles = _rectangles
                        header     = _header
                        pos        = v
              }
        }

    
    let width (model : RectangleStack) =
      //let folded = 
      //  HMap.fold (fun s k v -> s + v.dim.width) 0.0 model.rectangles
      let max =
        (DS.HMap.values model.rectangles  )
          |> List.map (fun r -> Rectangle.Lens.width.Get r)
          |> List.max
      max

    let height (model : RectangleStack) =
      let folded = 
        HMap.fold (fun s k v -> s + v.dim.height) 0.0 model.rectangles
      folded

    let resetPosition (model : RectangleStack) (v : V2d) =
      let _rectangles = 
        model.rectangles 
          |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, v))
      let _header = Header.Lens.pos.Set (model.header, v)      
      {
        model with  rectangles     = _rectangles
                    header         = _header
                    pos            = v
                    needsLayouting = false
      } |> stack

    let tryfindRectangle_M (model : MRectangleStack) (id : RectangleId) =
      AMap.tryFind id model.rectangles

    let tryfindRectangle (model : RectangleStack) (id : RectangleId) =
      HMap.tryFind id model.rectangles

    let update (model : RectangleStack) (action : Action) =
      let updateRect (optr : option<Rectangle>) (m : Rectangle.Action) = 
        match optr with
          | Some r -> Svgplus.Rectangle.update r m
          | None   -> Rectangle.init (RectangleId.newId ())    

      match action with 
        | ResetPosition v ->
          resetPosition model v    
        | AddRectangle r -> 
          let _rectangles = model.rectangles.Add (r.id, r)
          {model with rectangles = _rectangles} |> stack
        | UpdatePosition v -> Lens.pos.Set (model, v)
        | Delete           -> model
        | RectangleMessage msg ->
          let (id, m) = msg
          let _rects = model.rectangles 
                        |> HMap.update id (fun x -> updateRect x m)
          let needsLayoutingX = 
            HMap.exists (fun key (rect : Rectangle) -> rect.needsLayoutingX) model.rectangles 

          {model with rectangles     = _rects
                      needsLayouting = true}
        | HeaderMessage msg ->
          let _header = Header.update model.header msg
          {model with header = _header}
        | ChangeLabel msg ->
          let _header = Header.update model.header (Header.ChangeLabel msg)
          {model with header = _header}
        | UpdateColour cmap ->
          let _rects =
            model.rectangles
              |> HMap.map (fun id r -> Rectangle.update r (Rectangle.Action.UpdateColour cmap) )
          {model with rectangles = _rects}
        | UpdateYSizes f ->
          let _rects =
            model.rectangles
              |> HMap.map (fun id r -> Rectangle.Lens.height.Update (r,f))
          {model with rectangles = _rects}
        | UpdateXSizes f ->
          let _rects =
            model.rectangles
              |> HMap.map (fun id r -> Rectangle.Lens.width.Update (r,f))
          {model with rectangles = _rects}


    let view (model : MRectangleStack) =

      let viewMap = 
        Svgplus.Rectangle.view >> UIMapping.mapAListId  
    
      let content =
        alist {
          yield (Header.view model.header) |> UI.map HeaderMessage
          for id in model.order do
            let! r = AMap.find id model.rectangles 
            yield! (viewMap r id RectangleMessage)
            
        }

      content

      //let rdnMap = model.rectangles 
      //              |> AMap.map (fun k r -> foo r k RectangleMessage) 

      //let ralst = DS.AMap.toFlatAList rdnMap
      //ralst

      
    



