namespace Svgplus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Rectangle =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open Svgplus
    open Svgplus.Base
    open SimpleTypes
    open UIPlus
    open UIPlus.Mutable
    open Svgplus.RectangleType


    let rand = System.Random ()

    type Action =
      | Select          of RectangleId
      | Deselect        of RectangleId
      | OnMouseEnter
      | OnMouseLeave
      | ToggleDraw
      | NWButtonMessage of Button.Action
      | NEButtonMessage of Button.Action
      | SWButtonMessage of Button.Action
      | SEButtonMessage of Button.Action
      | UpdateColour    of (ColourMap * CMItemId)
      | SetWidth        of (float * ColourMap)
      | SetDottedBorder of bool 
      | LayoutX          
      | LayoutY

    module Lens =
      let width = 
        {new Lens<Rectangle, float>() with
          override x.Get(r)   = r.dim.width
          override x.Set(r,v) = 
            {r with dim = {r.dim with width = v}
                    northEastButton = Button.Lens.posX.Set (r.northEastButton, r.pos.X + v)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + v)
                    needsLayoutingX = true
            }
          override x.Update(r,f) = 
            {r with dim = {r.dim with width = f r.dim.width}
                    northEastButton = Button.Lens.posX.Set (r.northEastButton, r.pos.X + f r.dim.width)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + f r.dim.width)
                    needsLayoutingX = true
            }
        }

      let height = 
        {new Lens<Rectangle, float>() with
          override x.Get(r)   = r.dim.height
          override x.Set(r,v) = 
            {r with dim = {r.dim with height = v}
                    southWestButton = Button.Lens.posX.Set (r.southWestButton, r.pos.X + v)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + v)
                    needsLayoutingY = true
            }
          override x.Update(r,f) = 
            {r with dim = {r.dim with height = f r.dim.height}
                    southWestButton = Button.Lens.posX.Set (r.southWestButton, r.pos.X + f r.dim.height)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + f r.dim.height)
                    needsLayoutingY = true
            }
        }

      let posX = 
        {new Lens<Rectangle, float>() with
          override x.Get(r)   = r.pos.X
          override x.Set(r,v) = 
            {r with pos = V2d (v, r.pos.Y)
                    northWestButton = Button.Lens.posX.Set (r.northWestButton, v)
                    northEastButton = Button.Lens.posX.Set (r.northEastButton, v + width.Get r)
                    southWestButton = Button.Lens.posX.Set (r.southWestButton, v)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, v + width.Get r)
          
            }
          override x.Update(r,f) = 
            {r with pos =  V2d (f r.pos.X, r.pos.Y)
                    northWestButton = Button.Lens.posX.Set (r.northWestButton, f r.pos.X)
                    northEastButton = Button.Lens.posX.Set (r.northEastButton, f r.pos.X + width.Get r)
                    southWestButton = Button.Lens.posX.Set (r.southWestButton, f r.pos.X)
                    southEastButton = Button.Lens.posX.Set (r.southEastButton, f r.pos.X + width.Get r)
            }
        }

      let posY = 
        {new Lens<Rectangle, float>() with
          override x.Get(r)   = r.pos.Y
          override x.Set(r,v) = 
            {r with pos = V2d (r.pos.X, v)
                    northWestButton = Button.Lens.posY.Set (r.northWestButton, v)
                    northEastButton = Button.Lens.posY.Set (r.northEastButton, v)
                    southWestButton = Button.Lens.posY.Set (r.southWestButton, v + height.Get r)
                    southEastButton = Button.Lens.posY.Set (r.southEastButton, v + height.Get r)
            }

          override x.Update(r,f) = 
            {r with pos = V2d (r.pos.X, f r.pos.Y)
                    northWestButton = Button.Lens.posY.Set (r.northWestButton, f r.pos.Y)
                    northEastButton = Button.Lens.posY.Set (r.northEastButton, f r.pos.Y)
                    southWestButton = Button.Lens.posY.Set (r.southWestButton, f r.pos.Y + height.Get r)
                    southEastButton = Button.Lens.posY.Set (r.southEastButton, f r.pos.Y + height.Get r)
            }
        }

      let pos =
          { new Lens<Rectangle, Aardvark.Base.V2d>() with
              override x.Get(r) = V2d (posX.Get r, posY.Get r)
              override x.Set(r,v) =
                let _r = posX.Set (r,v.X)
                posY.Set (_r,v.Y)
              override x.Update(r,f) =
                let _v = f (V2d ((posX.Get r), (posY.Get r)))
                let _r = posX.Set (r,_v.X)
                posY.Set (_r,_v.Y)
          }

      let col =
        {new Lens<Rectangle, C4b>() with
          override x.Get(r)   = 
            match r.overwriteColour with
              | Some c -> c
              | None   -> r.colour.c
          override x.Set(r,v) = 
            {r with colour = {r.colour with c = v}
                    overwriteColour = None}
          override x.Update(r,f) = 
            {r with colour = {r.colour with c = f r.colour.c}
                    overwriteColour = None}
        }

      let dottedBorder = 
        {new Lens<Rectangle, bool>() with
          override x.Get(r)   = r.dottedBorder
          override x.Set(r,v) = 
            {r with dottedBorder = v}
          override x.Update(r,f) = 
            {r with dottedBorder = f r.dottedBorder}
        }

      let isToggeled = 
        {new Lens<Rectangle, bool>() with
          override x.Get(r)   = r.isToggled
          override x.Set(r,v) = 
            {r with isToggled = v}
          override x.Update(r,f) = 
            {r with isToggled = f r.isToggled}
        }

    let init id = 
      let _new = 
        {
          id             = id
          needsLayoutingX = false
          needsLayoutingY = false
          pos            = V2d (0.0)
          dim            = {width = 0.0; height = 0.0}
          colour         = {c = C4b.Gray}
          overwriteColour = Some C4b.White
          borderColour   = C4b.Black
          isToggled      = false
          colChange      = V3i (0,0,0)
          isHovering     = false
          dottedBorder   = true
          draw           = false

          label          = {TextInput.init with text = "label"}

          northWestButton = Button.init
          northEastButton = Button.init
          southWestButton = Button.init
          southEastButton = Button.init
      } 

      let _new = Lens.width.Set (_new, 50.0)
      let _new = Lens.height.Set (_new, 100.0)
      let _new = Lens.posX.Set (_new, 0.0)
      let _new = Lens.posY.Set (_new, 0.0)

      _new

    let update (model : Rectangle) (action : Action) =
      let updateColour cmap r = 
        let opt = ColourMap.svgValueToColourPicker cmap r.dim.width 
        match opt with
          | Some c -> {r with colour = c; overwriteColour = None}
          | None   -> r

      match action with
        | Select  id   -> 
          match model.isToggled with
            | true -> model
            | false ->
              let newCol = (model.colour.c -- model.colChange)
              {model with isToggled  = (not model.isToggled)
                          colour      = {c = newCol}
                          colChange  = -model.colChange
              }
        | Deselect  id   -> 
          match model.isToggled with
            | true -> 
              let newCol = (model.colour.c -- model.colChange)
              {model with isToggled  = false
                          colour     = {c = newCol}
                          colChange  = -model.colChange
              }
            | false -> model
        | OnMouseEnter -> 
          {model with isHovering = true}
        | OnMouseLeave  -> 
          {model with isHovering = false}
        | ToggleDraw ->
          {model with draw = not model.draw}
        | NWButtonMessage m -> {model with northWestButton = Button.update model.northWestButton m}
        | NEButtonMessage m -> {model with northEastButton = Button.update model.northEastButton m}
        | SWButtonMessage m -> {model with southWestButton = Button.update model.southWestButton m}
        | SEButtonMessage m -> {model with southEastButton = Button.update model.southEastButton m}
        | UpdateColour (cmap, itemid) -> updateColour cmap model
        | LayoutX            -> {model with needsLayoutingX = false}
        | LayoutY            -> {model with needsLayoutingY = false}
        | SetWidth (w, cmap)        -> 
          let _model = Lens.width.Set (model, w)
          updateColour cmap _model
        | SetDottedBorder b -> 
          {model with dottedBorder = b}


        

    let view (model : MRectangle) =
      alist {
        let! pos  = model.pos
        let! dim  = model.dim
        let! overwriteColour = model.overwriteColour
        let! baseCol   = model.colour.c
        let col = 
          match overwriteColour with
            | Some c -> c
            | None -> baseCol
        
        let! sel  = model.isToggled
        let! db   = model.dottedBorder

        let! draw = model.draw
        if draw then
          yield (Svgplus.Base.drawBorderedRectangle
                    pos dim col
                    C4b.Black C4b.Black
                    SvgWeight.init
                    (fun _ -> Select model.id)
                    sel db)
          yield (Button.view model.northWestButton) |> UI.map NWButtonMessage
          yield (Button.view model.northEastButton) |> UI.map NEButtonMessage
          yield (Button.view model.southWestButton) |> UI.map SWButtonMessage
          yield (Button.view model.southEastButton) |> UI.map SEButtonMessage

       
      }

