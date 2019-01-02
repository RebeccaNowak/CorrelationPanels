namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RectangleStack =
  type Action =
    | RectangleMessage of (RectangleId * Rectangle.Action)
    | UpdatePosition   of V2d
    | Delete

  let stack (model : RectangleStack) =
    
    let f (prev : Rectangle) (curr : Rectangle) =
      let cposy = prev.pos.Y + prev.dim.height
      Rectangle.Lens.posY.Set (curr, cposy)
    let _rs = 
      DS.PList.mapPrev' model.order  model.rectangles None f
    {model with rectangles = _rs}

  let init id : RectangleStack =
    let r1 = {Rectangle.init (RectangleId.newId ()) with 
                draw = true
                colour = {c = C4b.Green}
              }
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
        rectangles    = rmap
        order         = PList.ofList order
        pos           = V2d.OO
      } 
      
    let stacked = rs |> stack
    stacked

  module Lens =
    let pos =
      { new Lens<Svgplus.RectangleStack, Aardvark.Base.V2d>() with
          override x.Get(r) = r.pos
          override x.Set(r,v) =
            let _rectangles = 
              r.rectangles |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, r.pos + v))
            {r with rectangles = _rectangles
                    pos        = v}
          override x.Update(r,f) =
            let _v = f r.pos
            let _rectangles = 
              r.rectangles |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, r.pos + _v))
            {r with rectangles = _rectangles
                    pos        = _v}
      }

    
  let width (model : RectangleStack) =
    let folded = 
      HMap.fold (fun s k v -> s + v.dim.width) 0.0 model.rectangles
    folded

  let height (model : RectangleStack) =
    let folded = 
      HMap.fold (fun s k v -> s + v.dim.height) 0.0 model.rectangles
    folded

  let update (model : RectangleStack) (action : Action) =
    let updateRect (optr : option<Rectangle>) (m : Rectangle.Action) = 
      match optr with
        | Some r -> Svgplus.Rectangle.update r m
        | None   -> Rectangle.init (RectangleId.newId ())    

    match action with 
      | UpdatePosition v -> Lens.pos.Set (model, v)
      | Delete           -> model
      | RectangleMessage msg ->
        let (id, m) = msg
        let _rects = model.rectangles 
                      |> HMap.update id (fun x -> updateRect x m)
        {model with rectangles    = _rects}


  let view (model : MRectangleStack) =
    let viewMap = 
      Svgplus.Rectangle.view >> UIMapping.mapAListId  
    
    let content =
      alist {
        for id in model.order do
          let! r = AMap.find id model.rectangles 
          yield! (viewMap r id RectangleMessage)
      }

    content

    //let rdnMap = model.rectangles 
    //              |> AMap.map (fun k r -> foo r k RectangleMessage) 

    //let ralst = DS.AMap.toFlatAList rdnMap
    //ralst

      
    



