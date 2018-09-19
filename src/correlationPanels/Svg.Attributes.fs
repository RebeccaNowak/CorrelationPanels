namespace CorrelationDrawing.Svg

  module Attributes =
    open Aardvark.Base
    open Aardvark.UI
    open Aardvark.Base.Incremental
    
    let inline atf (attributeName : string) (value : float) =
      attribute attributeName (sprintf "%.2f" value)

    let inline ats (attributeName : string) (value : string) =
      attribute attributeName value

    let inline atc (attributeName : string) (value : C4b) =
      attribute attributeName (Html.ofC4b value)

    let toGroup (content : List<DomNode<'a>>) (atts : List<Attribute<'a>>) =
      Aardvark.UI.Svg.g ([clazz "g"] @ atts) content

    let inline elemNS (tagName : string) (ns : string) (attrs : AttributeMap<'msg>) (children : alist<DomNode<'msg>>) =
        DomNode.Node(tagName, ns, attrs, children)

    let inline g x = elemNS "g" Incremental.Svg.svgNS x

    let toGroup' (content : alist<DomNode<'a>>) (atts : amap<string, AttributeValue<'a>>) =
      let foo = 
          (amap {yield (clazz "g")})
      let atts = 
        AttributeMap.ofAMap (AMap.union foo atts)
      g atts content



    

    