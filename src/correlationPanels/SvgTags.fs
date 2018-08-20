﻿namespace Aardvark.UI

  module Incremental =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Incremental.Svg

    module Svg =


      let inline elemNS (tagName : string) (ns : string) (attrs : AttributeMap<'msg>) (children : alist<DomNode<'msg>>) =
          DomNode.Node(tagName, ns, attrs, children)

      let inline g (a : AttributeMap<'msg>) (x : alist<DomNode<'msg>>) = elemNS "g" svgNS a x






