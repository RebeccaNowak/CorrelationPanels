namespace CorrelationDrawing.Svg
  
  module Paths =
    open Aardvark.Base
    open Aardvark.UI
    open CorrelationDrawing.Svg.Attributes


    /////// PATHS
    let inline (>>) (x : string) (y : string) =
      sprintf "%s %s" x y

    let move (a : V2d) =
      sprintf "M%.2f %.2f" a.X a.Y

    let lineTo (a : V2d)  =
      sprintf "L%.2f %.2f" a.X a.Y

    let curveTo (letter : string) (b : V2d) (c : V2d) =
      sprintf "Q %.2f %.2f %.2f %.2f" b.X b.Y c.X c.Y  

    let close = "Z"

    let buildPath (str : string) (color : C4b) (stroke : float) (fill : bool) =
      let fillAttr =
        match fill with
          | true -> [atc "fill" color]
          | false -> [ats "fill" "none"]
      Svg.path
        ([
          ats "d" str
          atc "stroke" color
          atf "stroke-witdth" stroke
        ]@fillAttr)

    let circleSegmentTo (radius : float) (a : V2d) (b : V2d) (color : C4b) =
        sprintf "A %f,%f 0 %f,%f %f,%f" radius radius a.X a.Y b.X b.Y
      

    type CurveType = SmoothCurve | BezierCurve | Elliptical
      
    let drawLinePath (points : List<V2d>) (color : C4b) (stroke : float) =
      let col = Html.ofC4b color
      let d =
        move points.Head 
          >> (points 
              |> List.map (fun x -> lineTo x)
              |> List.tail
              |> List.reduce (fun a b -> a >> b)
             )
      Svg.path 
        [
          ats "d" d
          ats "stroke" col
          atf "stroke-width" stroke
          ats "fill" "none"
        ]


                   

    //let drawCurvedPath (points : List<V2d>) (curveType : CurveType) =
    //  match curveType with
    //    | CurveType.SmoothCurve ->
    //        points |> List.map (fun x -> curveTo "S" x x) //TODO control points
    //    | CurveType.BezierCurve ->
    //        points |> List.map (fun x -> curveTo "S" x x)
    //    | CurveType.Elliptical ->
    //        points |> List.map (fun x -> curveTo "S" x x)
