namespace Svgplus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Header =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open UIPlus
    open Svgplus.HeaderType
    open Svgplus
    open Attributes


    type Action =
      | LeftArrowMessage of ArrowButtonApp.Action
      | RightArrowMessage of ArrowButtonApp.Action

    let init : Header =
      let left = 
        {ArrowButtonApp.init (ArrowButtonId.newId ())
          with direction = Direction.Left}
      let right = 
        {ArrowButtonApp.init (ArrowButtonId.newId ())
          with direction = Direction.Right}
      {
        pos           = V2d.OO
        dim           = {width = 50.0; height = 50.0}
        label         = "label"
        leftButton    = left
        rightButton   = right
      }

    module Lens =
      let pos =
        { new Lens<Header, Aardvark.Base.V2d>() with
            override x.Get(r) = r.pos
            override x.Set(r,v) = {r with pos = v}
            override x.Update(r,f) = {r with pos = f r.pos}
        }
      let dim =
        { new Lens<Header, SimpleTypes.Size2D>() with
            override x.Get(r) = r.dim
            override x.Set(r,v) = {r with dim = v}
            override x.Update(r,f) = {r with dim = f r.dim}
        }
      let width =
        { new Lens<Header, float>() with
            override x.Get(r) = r.dim.width
            override x.Set(r,v) = 
              {r with dim = {width = v;height = r.dim.height}}
            override x.Update(r,f) = 
              {r with dim = {width = f r.dim.width; height = r.dim.height}}
        }
      let height =
        { new Lens<Header, float>() with
            override x.Get(r) = r.dim.height
            override x.Set(r,v) = 
              {r with dim = {height = v;width = r.dim.width}}
            override x.Update(r,f) = 
              {r with dim = {height = f r.dim.width; width = r.dim.width}}
        }



    let view (model : MHeader) =
      let atts = 
        (Incremental.xywh model.pos model.dim)
          |> AttributeMap.ofAMap
        
      let content =
        alist {
          yield ((ButtonApp.view model.leftButton)  |> UI.map LeftArrowMessage)
          yield (Incremental.Svg.text model.label)
          yield ((ButtonApp.view model.rightButton) |> UI.map LeftArrowMessage)
        }

     




      //let namesp =
      //  [attribute "xmlns" "http://www.w3.org/1999/xhtml"] |> AttributeMap.ofList

      //Incremental.Svg.foreignObject 
      //                  atts
      //                  ([Incremental.div namesp content] |> AList.ofList)

  