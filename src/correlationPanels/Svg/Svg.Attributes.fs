﻿namespace Svgplus
  open Aardvark.Base



  module Attributes =
    open Aardvark.UI
    open Aardvark.Base.Incremental


      
    let inline atf (attributeName : string) (value : float) =
      attribute attributeName (sprintf "%.4f" value)

    let inline ats (attributeName : string) (value : string) =
      attribute attributeName value

    let inline atc (attributeName : string) (value : C4b) =
      attribute attributeName (Html.ofC4b value)

    let inline g'' a x = elemNS "g" Svg.svgNS a x



    let toGroup (content : List<DomNode<'a>>) (atts : List<Attribute<'a>>) =
      g'' ([clazz "g"] @ atts) content




    module Incremental =
      open SimpleTypes

      let inline elemNS' (tagName : string) (ns : string) (attrs : AttributeMap<'msg>) (children : alist<DomNode<'msg>>) =
        DomNode.Node(tagName, ns, attrs, children)

      let inline g' x = elemNS' "g" Incremental.Svg.svgNS x

      let toGroup (content : alist<DomNode<'a>>) 
                  (atts : amap<string, AttributeValue<'a>>) =
        let foo = 
            (amap {yield (clazz "g")})
        let atts = 
          AttributeMap.ofAMap (AMap.union foo atts)
        g' atts content

      let inline position (value : IMod<V2d>) =
        amap {
          let! v = value
          yield atf "cx" v.X
          yield atf "cy" v.Y
        }

      let inline xywh (pos : IMod<V2d>) (dim : IMod<Size2D>) =
        amap {
          let! pos = pos
          let! dim = dim
          yield atf "x" pos.X
          yield atf "y" pos.Y
          yield atf "width"  dim.width
          yield atf "height" dim.height
        }

      let inline stroke (color : IMod<C4b>)
                        (width : IMod<float>) =
        amap {
          let! c = color
          yield atc "stroke" c
          let! w = width
          yield atf "stroke-width" w
        }    

      let inline radius (value : IMod<float>) =
        amap {
          let! v = value
          yield atf "r" v
        }

      let inline fill (color : IMod<C4b>) =
        amap {
           let! c = color
           yield atc "fill" c
        }

      let inline bFill (fill : IMod<bool>) (color : IMod<C4b>) =
        amap {
           let! c = color
           let! fill = fill
           if fill then yield atc "fill" c
        }

      let circle  (_position  : IMod<V2d>)
                  (_color     : IMod<C4b>)
                  (_width     : IMod<float>)
                  (_radius    : IMod<float>)
                  (_fill      : IMod<bool>) =
        let atts = 
          (position _position)
            |> AMap.union (stroke _color _width)
            |> AMap.union (radius _radius)
        atts
          |> AMap.union (bFill _fill _color)

    

    