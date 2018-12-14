namespace Svgplus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Button =
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus.Base


  type Action =
  | OnLeftClick
  | OnMouseEnter
  | OnMouseLeave
  | SetVisible     of bool
    

  let init = {
    pos            = V2d (0.0)
    radius         = 10.0
    rHoverChange   = 2.0
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
      | OnLeftClick  -> 
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
    let left = Aardvark.UI.Svg.Events.onClickAttributes [(fun _ -> OnLeftClick)]
    let st = [
              (GUI.CSS.Incremental.transition model.transitionSec)
             ] |> GUI.CSS.Incremental.style

    let actions = [enter;exit]@left
                    |> AMap.ofList
                    
    Incremental.circle' ((AMap.union atts actions) 
                            |> AMap.union st)
          
    
