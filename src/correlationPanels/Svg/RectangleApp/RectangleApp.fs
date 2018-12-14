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

  module Lens =
    let width = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.dim.width
        override x.Set(r,v) = 
          {r with dim = 
                    {r.dim with width = v}
          }
        override x.Update(r,f) = 
          {r with dim = 
                    {r.dim with width = f r.dim.width}
          }
      }

    let height = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.dim.height
        override x.Set(r,v) = 
          {r with dim = 
                    {r.dim with height = v}
          }
        override x.Update(r,f) = 
          {r with dim = 
                    {r.dim with height = f r.dim.height}
          }
      }

    let posX = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.pos.X
        override x.Set(r,v) = 
          {r with pos = V2d (v, r.pos.Y)}
        override x.Update(r,f) = 
          {r with pos =  V2d (f r.pos.X, r.pos.Y)}
      }

    let posY = 
      {new Lens<Svgplus.Rectangle, float>() with
        override x.Get(r)   = r.pos.Y
        override x.Set(r,v) = 
          {r with pos = V2d (r.pos.X, v)}
        override x.Update(r,f) = 
          {r with pos = V2d (r.pos.X, f r.pos.Y)}
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

  let init = {
    pos           = V2d (0.0)
    dim           = {width = 20.0; height = 50.0}
    colour        = {c = C4b.Gray}
    borderColour  = C4b.Black
    isToggled     = false
    colChange     = V3i (30,30,30)
    isHovering    = false
    dottedBorder  = true
    draw          = false
  }

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
    }