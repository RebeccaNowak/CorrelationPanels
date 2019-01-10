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
    open UIPlus


    type Action =
      | LeftArrowMessage  of Button.Action
      | RightArrowMessage of Button.Action
      | MouseMessage      of MouseAction
      | ChangeLabel       of TextInput.Action

    let init : Header =
      let left = 
        Button.init 
          
      let right = 
        Button.init
      {
        pos           = V2d.OO
        dim           = {width = 50.0; height = 50.0}
        label         = {TextInput.init with text = "label"}
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

    let update (model : Header) (action : Action) =
      match action with
        | MouseMessage m -> model
        | ChangeLabel  m ->
          let _label = TextInput.update model.label m
          {model with label = _label}
          //match m with
          //  | MouseAction.OnLeftClick -> {model with 
          //  | _ -> model



    let view (model : MHeader) =
      let bold = 
        amap {
            yield (Svgplus.Attributes.ats "font-weight" "bold")
            
        } |> AttributeMap.ofAMap

      let mouseActions =
        (MouseActions.init ()) |> AttributeMap.ofAMap
      let atts = 
        (Incremental.xywh model.pos model.dim)
          |> AttributeMap.ofAMap
          |> AttributeMap.union bold
          |> AttributeMap.union mouseActions

      let label = 
        Aardvark.UI.Incremental.Svg.text atts model.label.text

      label |> UI.map MouseMessage



    

      //let namesp =
      //  [attribute "xmlns" "http://www.w3.org/1999/xhtml"] |> AttributeMap.ofList

      //Incremental.Svg.foreignObject 
      //                  atts
      //                  ([Incremental.div namesp content] |> AList.ofList)

  