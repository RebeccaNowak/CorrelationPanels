namespace CorrelationDrawing.Svg

  module Attributes =
    open Aardvark.Base
    open Aardvark.UI
    
    let inline atf (attributeName : string) (value : float) =
      attribute attributeName (sprintf "%.2f" value)

    let inline ats (attributeName : string) (value : string) =
      attribute attributeName value

    let inline atc (attributeName : string) (value : C4b) =
      attribute attributeName (Html.ofC4b value)

    let toGroup (content : List<DomNode<'a>>) (atts : List<Attribute<'a>>) =
      Aardvark.UI.Svg.g ([clazz "g"] @ atts) 
                                    content

    