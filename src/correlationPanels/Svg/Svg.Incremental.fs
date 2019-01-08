namespace Svgplus

  module Incremental =
    open Aardvark.UI
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Svgplus.Attributes
    open SimpleTypes

    let drawLine (a : IMod<V2d>) 
                 (b : IMod<V2d>) 
                 (color : IMod<C4b>) 
                 (strokeWidth : IMod<float>) 
                 (actions : amap<string,AttributeValue<'a>>)=
      let atts =
        amap {
          let! a = a
          let! b = b
          let! c = color
          let! s = strokeWidth
          yield (atf "x1" a.X)
          yield (atf "y1" a.Y)
          yield (atf "x2" b.X)
          yield (atf "y2" b.Y)
          yield (atc "stroke" c)
          yield (atf "stroke-width" s)
        } |> AttributeMap.ofAMap
          |> AttributeMap.union 
              (actions |> AttributeMap.ofAMap)

      Incremental.elemNS' "line" Incremental.Svg.svgNS atts (AList.empty)

    let drawDottedLine (a : V2d) (b : V2d) //WIP!!!!!!!
                       (color : C4b) 
                       (strokeWidth : float) 
                       (dashWidth : float) 
                       (dashDist : float) =
      Svg.line 
        [
          ats "stroke-dasharray" (sprintf "%f,%f" dashWidth dashDist)
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" b.X
          atf "y2" b.Y
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]

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

