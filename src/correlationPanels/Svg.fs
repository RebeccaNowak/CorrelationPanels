namespace CorrelationDrawing

  open Aardvark.UI
  //open Aardvark.UI.Incremental.Svg
  open Aardvark.Base
  open Aardvark.Base.Incremental


  module Svg =

    let margin = 5.0
    let sw = "3"

    let inline atf (attributeName : string) (value : float) =
      attribute attributeName (sprintf "%.2f" value)

    let inline ats (attributeName : string) (value : string) =
      attribute attributeName value

    let inline atc (attributeName : string) (value : C4b) =
      attribute attributeName (Html.ofC4b value)
    


    let toGroup (content : List<DomNode<'a>>) (atts : List<Attribute<'a>>) =
      Aardvark.UI.Incremental.Svg.g ([clazz "g"] @ atts) 
                                    content

    

    let drawText (a : V2d) (str : string) =
      Svg.text
        [
          atf "x" a.X
          atf "y" a.Y
        ] str

    let drawLine (a : V2d) (b : V2d) (color : C4b) (strokeWidth : float)=
      Svg.line 
        [
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" b.X
          atf "y2" b.Y
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]

    let drawHorizontalLine (a : V2d) (length : float) (color : C4b) (strokeWidth : float) =
      Svg.line 
        [
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" (a.X + length)
          atf "y2" a.Y
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]

    let drawVerticalLine (a : V2d) (length : float) (color : C4b) (strokeWidth : float) =
      Svg.line 
        [
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" a.X 
          atf "y2" (a.Y + length)
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]

    let drawHorizontalDottedLine  (a : V2d) (length : float) (color : C4b) 
                                  (strokeWidth : float) (dashWidth : float) (dashDist : float) =
      Svg.line 
        [
          ats "stroke-dasharray" (sprintf "%f,%f" dashWidth dashDist)
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" (a.X + length)
          atf "y2" a.Y
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]

    let drawVerticalDottedLine (a : V2d) (length : float) (color : C4b) (strokeWidth : float) =
      Svg.line 
        [
          ats "stroke-dasharray" "5,5"
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" a.X 
          atf "y2" (a.Y + length)
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]



    /////// PATHS
    let inline (>>) (x : string) (y : string) =
      sprintf "%s %s" x y

    let move (a : V2d) =
      sprintf "M%.2f %.2f" a.X a.Y

    let lineTo (a : V2d)  =
      sprintf "L%.2f %.2f" a.X a.Y

    let curveTo (letter : string) (b : V2d) (c : V2d) =
      sprintf "Q %.2f %.2f %.2f %.2f" b.X b.Y c.X c.Y

    let endLine = "Z"

    let buildPath (str : string) (color : C4b) =
      let col = Html.ofC4b color
      Svg.path
        [
          attribute "d" str
          attribute "stroke" col
          attribute "stroke-witdth" sw
          attribute "fill" "none"
        ]

    type CurveType = SmoothCurve | BezierCurve | Elliptical
      
    let drawLinePath (points : List<V2d>) (color : C4b) =
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
          attribute "d" d
          attribute "stroke" col
          attribute "stroke-width" sw
          attribute "fill" "none"
        ]
                   

    let drawCurvedPath (points : List<V2d>) (curveType : CurveType) =
      match curveType with
        | CurveType.SmoothCurve ->
            points |> List.map (fun x -> curveTo "S" x x) //TODO control points
        | CurveType.BezierCurve ->
            points |> List.map (fun x -> curveTo "S" x x)
        | CurveType.Elliptical ->
            points |> List.map (fun x -> curveTo "S" x x)




    let drawCircle (centre : V2d) (radius : float) (color : C4b) = 
      Svg.circle
        [
          atf "cx" (centre.X - radius + margin)
          atf "cy" (centre.Y - radius + margin)
          atf "r" radius
          atc "stroke" C4b.Black //Performance
          ats "stroke-width" "1.0"
          atc "fill" color
        ]

    let drawCircleButton (centre : V2d) (radius : float)
                         (color : C4b) (filled : bool) 
                         (stroke : float)
                         (callback   : list<string> -> 'msg) = 
      let atts = [
          atf "cx" (centre.X - radius + margin)
          atf "cy" (centre.Y - radius + margin)
          atf "r" radius
          atc "stroke" color 
          atf "stroke-width" stroke 
        ]
      toGroup [
        Svg.circle (if filled then atts @ [atc "fill" color] else atts)
      ] (Svg.Events.onClickAttribute (callback))

        

    let drawRectangle (leftUpper : V2d) width height  (color : C4b) =
      Svg.rect [
        atf "x" leftUpper.X
        atf "y" leftUpper.Y
        atf "width" width
        atf "height" height
        atc "fill" color
      ]

    let drawRectangleWs (leftUpper : V2d) width height  (color : C4b) (strokeWidth : float) (strokeColor : C4b)=
      Svg.rect [
        atf "x" leftUpper.X
        atf "y" leftUpper.Y
        atf "width" width
        atf "height" height
        atc "stroke" strokeColor
        atf "stroke-width" strokeWidth
        atc "fill" color
      ]

    let drawRectangle2c (leftUpper : V2d) width height  (color1 : C4b) (color2 : C4b) =
      let half = height * 0.5       
      
      toGroup 
        [ 
          drawRectangle leftUpper width half color1
          drawRectangle (new V2d(leftUpper.X, leftUpper.Y + half)) width half color2
        ]            
        [
          atc "stroke" C4b.Black
          ats "stroke-width" "0"
        ]

    let drawRectangleHBorder (leftUpper   : V2d) 
                              (width      : float)
                              (height     : float)
                              (fill       : C4b) 
                              (uBorder    : C4b) 
                              (lBorder    : C4b)
                              (bWeight    : float) = //TODO WIP
      toGroup 
        [ 
          drawRectangle leftUpper width height fill
          drawLine leftUpper (new V2d(leftUpper.X + width, leftUpper.Y)) uBorder bWeight
          drawLine (new V2d(leftUpper.X, leftUpper.Y + height)) (new V2d(leftUpper.X + width, leftUpper.Y + height)) lBorder bWeight
        ]            
        [
        ]

    let drawHBorders  (leftUpper   : V2d) 
                      (width      : float)
                      (height     : float)
                      (uBorder    : C4b) 
                      (lBorder    : C4b)
                      (bWeight    : float)
                      (callback   : V2i -> 'msg) =
      toGroup 
        [ 
          drawHorizontalLine 
            (new V2d(leftUpper.X, leftUpper.Y + bWeight * 0.5)) 
            width uBorder bWeight
          drawHorizontalLine 
            (new V2d(leftUpper.X, leftUpper.Y + height - bWeight * 0.5)) 
            width lBorder bWeight
        ][ onMouseEnter (callback) ]

    let drawBorderedRectangle (leftUpper    : V2d) 
                              (width        : float)
                              (height       : float)
                              (fill         : C4b) 
                              (uBorder      : C4b) 
                              (lBorder      : C4b)
                              (bWeight      : float)
                              (selectionCallback     : list<string> -> 'msg)
                              (selected     : bool)
                              (dottedBorder : bool) =
      let fill =
        match selected with //TODO read papers: mark selection state
          | true  -> C4b.DarkYellow
          | false -> fill
      let elements = 
          [  
            drawRectangle leftUpper width height fill
            drawHorizontalLine 
              (new V2d(leftUpper.X, leftUpper.Y + bWeight * 0.5)) 
              width lBorder bWeight
            drawHorizontalLine 
              (new V2d(leftUpper.X, leftUpper.Y + height - bWeight * 0.5)) 
              width uBorder bWeight 
            drawVerticalLine leftUpper height C4b.Black 2.0
          ]
      let rBorder = 
        match dottedBorder with
          | true  -> drawVerticalDottedLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black 2.0
          | false -> drawVerticalLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black 2.0

      toGroup 
        (elements @ [rBorder])
        (Svg.Events.onClickAttribute (selectionCallback))


     
    let drawXAxis (leftUpper : V2d) (length : float) (color : C4b) (weight : float) (granularity : float) =
      toGroup
        [
          drawHorizontalLine  (new V2d(leftUpper.X, leftUpper.Y + weight * 0.5)) length color weight
          drawHorizontalDottedLine (new V2d(leftUpper.X, leftUpper.Y + weight)) length color (weight * 3.0) 1.0 (granularity - 1.0)
        ]
        []
