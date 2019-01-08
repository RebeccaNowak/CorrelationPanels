namespace Svgplus.RS

  open System
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus



  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module RectangleStack =
    open System.Windows.Interop

    type Action =
      | RectangleMessage of (RectangleId * Rectangle.Action)
      | HeaderMessage    of Header.Action
      | UpdatePosition   of V2d
      | Delete

    let stack (model : RectangleStack) =
      let f (prev : Rectangle) (curr : Rectangle) =
        let cposy = prev.pos.Y + prev.dim.height
        Rectangle.Lens.posY.Set (curr, cposy)
      let _rs = 
        DS.PList.mapPrev' model.order  model.rectangles None f

      let maxWidth = model.rectangles 
                      |> DS.HMap.values
                      |> List.map (fun r -> r.dim.width)
                      |> List.max

      let _header = Header.Lens.width.Set (model.header, maxWidth)
      {
        model with  rectangles = _rs
                    header     = _header  
      }

    let init id : RectangleStack =
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
            override x.Get(r) = r.pos
            override x.Set(r,v) =
              let _rectangles = 
                r.rectangles 
                  |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, r.pos + v))
              let _header = Header.Lens.pos.Set (r.header, v) 
              {
                  r with  rectangles = _rectangles
                          header     = _header
                          pos        = v
              }
            override x.Update(r,f) =
              let _v = f r.pos
              let _rectangles = 
                r.rectangles 
                  |> HMap.map (fun id r -> Rectangle.Lens.pos.Set (r, r.pos + _v))
              let _header = Header.Lens.pos.Set (r.header, _v) 
              {
                r with  rectangles = _rectangles
                        header     = _header
                        pos        = _v
              }
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
        | HeaderMessage msg -> model
          


    let view (model : MRectangleStack) =
      let viewMap = 
        Svgplus.Rectangle.view >> UIMapping.mapAListId  
    
      let content =
        alist {
          //yield (Header.view model.header) |> UI.map HeaderMessage
          for id in model.order do
            let! r = AMap.find id model.rectangles 
            yield! (viewMap r id RectangleMessage)
            
        }

      content

      //let rdnMap = model.rectangles 
      //              |> AMap.map (fun k r -> foo r k RectangleMessage) 

      //let ralst = DS.AMap.toFlatAList rdnMap
      //ralst

      
    



