namespace Svgplus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rectangle =
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus
  open Svgplus.Base
  open SimpleTypes


  type Action =
    | OnLeftClick
    | OnMouseEnter
    | OnMouseLeave
    | ToggleDraw
    | NWButtonMessage of Button.Action
    | NEButtonMessage of Button.Action
    | SWButtonMessage of Button.Action
    | SEButtonMessage of Button.Action

  module Lens =
    let width = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.dim.width
        override x.Set(r,v) = 
          {r with dim = {r.dim with width = v}
                  northEastButton = Button.Lens.posX.Set (r.northEastButton, r.pos.X + v)
                  southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + v)
          }
        override x.Update(r,f) = 
          {r with dim = {r.dim with width = f r.dim.width}
                  northEastButton = Button.Lens.posX.Set (r.northEastButton, r.pos.X + f r.dim.width)
                  southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + f r.dim.width)
          }
      }

    let height = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.dim.height
        override x.Set(r,v) = 
          {r with dim = {r.dim with height = v}
                  southWestButton = Button.Lens.posX.Set (r.southWestButton, r.pos.X + v)
                  southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + v)
          }
        override x.Update(r,f) = 
          {r with dim = {r.dim with height = f r.dim.height}
                  southWestButton = Button.Lens.posX.Set (r.southWestButton, r.pos.X + f r.dim.height)
                  southEastButton = Button.Lens.posX.Set (r.southEastButton, r.pos.X + f r.dim.height)
          }
      }

    let posX = 
      {new Lens<Svgplus.Rectangle, float>() with
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
      {new Lens<Svgplus.Rectangle, float>() with
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

    let colour =
      {new Lens<Svgplus.Rectangle, C4b>() with
        override x.Get(r)   = r.colour.c
        override x.Set(r,v) = 
          {r with colour = {r.colour with c = v}}
        override x.Update(r,f) = 
          {r with colour = {r.colour with c = f r.colour.c}}
      }

    let dottedBorder = 
      {new Lens<Svgplus.Rectangle, bool>() with
        override x.Get(r)   = r.dottedBorder
        override x.Set(r,v) = 
          {r with dottedBorder = v}
        override x.Update(r,f) = 
          {r with dottedBorder = f r.dottedBorder}
      }

    let isToggeled = 
      {new Lens<Svgplus.Rectangle, bool>() with
        override x.Get(r)   = r.isToggled
        override x.Set(r,v) = 
          {r with isToggled = v}
        override x.Update(r,f) = 
          {r with isToggled = f r.isToggled}
      }

  let init id = 
    let _new = 
      {
      id            = id
      pos           = V2d (0.0)
      dim           = {width = 0.0; height = 0.0}
      colour        = {c = C4b.Gray}
      borderColour  = C4b.Black
      isToggled     = false
      colChange     = V3i (30,30,30)
      isHovering    = false
      dottedBorder  = true
      draw          = false

      northWestButton = Button.init
      northEastButton = Button.init
      southWestButton = Button.init
      southEastButton = Button.init
    } 

    let _new = Lens.width.Set (_new, 50.0)
    let _new = Lens.height.Set (_new, 100.0)
    let _new = Lens.posX.Set (_new, 100.0)
    let _new = Lens.posY.Set (_new, 100.0)

    _new

  let update (model : Rectangle) (action : Action) =
    match action with
      | OnLeftClick  -> 
        let newCol = (model.colour.c -- model.colChange)
        {model with isToggled  = (not model.isToggled)
                    colour      = {c = newCol}
                    colChange  = -model.colChange
        }
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

  let view (model : MRectangle) =
    alist {
      let! pos  = model.pos
      let! dim  = model.dim
      let! c1   = model.colour.c
      let! sel  = model.isToggled
      let! db   = model.dottedBorder

      let! draw = model.draw
      if draw then
        yield (Svgplus.Base.drawBorderedRectangle
                  pos dim c1
                  C4b.Black C4b.Black
                  SvgWeight.init
                  (fun _ -> OnLeftClick)
                  sel db)
        yield (Button.view model.northWestButton) |> UI.map NWButtonMessage
        yield (Button.view model.northEastButton) |> UI.map NEButtonMessage
        yield (Button.view model.southWestButton) |> UI.map SWButtonMessage
        yield (Button.view model.southEastButton) |> UI.map SEButtonMessage
    }