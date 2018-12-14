namespace Svgplus

  module Incremental =
    open Aardvark.UI
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Svgplus.Attributes
    open SimpleTypes

    let circle (upperLeft : IMod<V2d>) 
               (radius    : IMod<float>) 
               (color     : IMod<C4b>)
               (stroke    : IMod<float>) 
               (fill      : IMod<bool>) = 
      let atts = 
        (Incremental.circle upperLeft color stroke radius fill)
          |> AttributeMap.ofAMap

      Incremental.elemNS' "circle" Incremental.Svg.svgNS atts (AList.empty)

    let circle' (atts : amap<string,AttributeValue<'a>>) =
      let a = AttributeMap.ofAMap(atts)
      Incremental.elemNS' "circle" Incremental.Svg.svgNS a (AList.empty)

    let drawBorderedRectangle (leftUpper         : IMod<V2d>) 
                              (size              : IMod<Size2D>)
                              (fill              : IMod<C4b>) 
                              (borderColors      : IMod<BorderColors>)
                              (bWeight           : IMod<SvgWeight>)
                              (selectionCallback : _ -> 'msg)
                              (selected          : IMod<bool>)
                              (dottedBorder      : IMod<bool>) =
      adaptive {
        let! size = size
        let! leftUpper = leftUpper
        let! color = fill
        let! bColors = borderColors
        let lborder = bColors.lower
        let uborder = bColors.upper
        let! isSelected = selected 
        let! bWeight = bWeight

        let rfun = Svgplus.Base.drawBorderedRectangle
                            leftUpper
                            size
                            color lborder uborder
                            bWeight
                            selectionCallback
                            isSelected

        let! dottedBorder = dottedBorder
        return match dottedBorder with
                | true ->
                    rfun true
                | false ->
                    rfun false
      }

