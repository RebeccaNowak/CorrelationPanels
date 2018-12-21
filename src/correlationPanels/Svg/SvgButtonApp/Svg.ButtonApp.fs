namespace Svgplus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Button =
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus.Base


  type Action =
  | OnLeftClick       of V2d
  | OnMouseEnter
  | OnMouseLeave
  | SetVisible        of bool

  module Lens =
    let posX = 
      {new Lens<Svgplus.Button, float>() with
        override x.Get(r)   = r.pos.X
        override x.Set(r,v) = 
          {r with pos = V2d (v, r.pos.Y)}
        override x.Update(r,f) = 
          {r with pos =  V2d (f r.pos.X, r.pos.Y)}
      }

    let posY = 
      {new Lens<Svgplus.Button, float>() with
        override x.Get(r)   = r.pos.Y
        override x.Set(r,v) = 
          {r with pos = V2d (r.pos.X, v)}
        override x.Update(r,f) = 
          {r with pos = V2d (r.pos.X, f r.pos.Y)}
      }

  let init = {
    id             = ButtonId.newId ()
    pos            = V2d (0.0)
    radius         = 8.0
    rHoverChange   = 8.0
    stroke         = 1.0
    color          = C4b(44,127,184)
    colChange      = V3i(-20,-20,-20)
    isToggled      = true
    fill           = true
    isHovering     = false
    transitionSec  = 0.5
  }

  let initc (centre : V2d) 
            (radius : float)
            (margin : float) =
   let x = centre.X - radius + margin
   let y = centre.Y - radius + margin
   {init with pos = V2d (x,y)}


  let update (model : Button) (action : Action) =
    match action with
      | OnLeftClick  b -> 
        let newCol = (model.color -- model.colChange)
        {model with isToggled  = (not model.isToggled)
                    color      = newCol
                    colChange  = -model.colChange
        }
      | OnMouseEnter -> 
        {model with radius     = model.radius + model.rHoverChange
                    isHovering = true}
      | OnMouseLeave  -> 
        {model with radius     = model.radius - model.rHoverChange
                    isHovering = false}
      | SetVisible b -> model

  let view (model : MButton) = 
    let atts = 
      Attributes.Incremental.circle 
        model.pos model.color model.stroke model.radius model.fill

    let enter = Aardvark.UI.Events.onMouseEnter (fun _ -> OnMouseEnter)
    let exit = Aardvark.UI.Events.onMouseLeave (fun _ -> OnMouseLeave)
    let left = 
      amap {
        let! pos = model.pos
        yield! (Aardvark.UI.Svg.Events.onClickAttributes [(fun _ -> OnLeftClick pos)])
      }

    let st = [
              (GUI.CSS.Incremental.transition model.transitionSec)
             ] |> GUI.CSS.Incremental.style

    let actions = [enter;exit] |> AMap.ofList
                    
    Incremental.circle' ((AMap.union atts actions) 
                            |> AMap.union left
                            |> AMap.union st)
          
    
