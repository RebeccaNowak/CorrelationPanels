namespace CorrelationDrawing.Svg

  module Base =
    open Aardvark.UI
    open Aardvark.Base
    open System
    open CorrelationDrawing.Math
    open CorrelationDrawing.Svg.Attributes
    open CorrelationDrawing.Svg.Paths
    open CorrelationDrawing

    let margin = 5.0
    let sw = "3"


    let drawBoldText (a : V2d) (str : string) (orientation : Orientation) = //TODO refactor
      let dir = 
        match orientation with
          | Orientation.Vertical ->
            [ats "glyph-orientation-vertical" "90";
             ats "writing-mode" "tb"]
          | Orientation.Horizontal ->
            [ats "glyph-orientation-horizontal" "auto"]
        
      Svg.text
        (dir@[
              atf "x" a.X
              atf "y" a.Y
              ats "font-weight" "bold"
             ]
        ) str

    let drawText (a : V2d) (str : string) (orientation : Orientation) =
      let dir = 
        match orientation with
          | Orientation.Vertical ->
            [ats "glyph-orientation-vertical" "90";
             ats "writing-mode" "tb"]
          | Orientation.Horizontal ->
            [ats "glyph-orientation-horizontal" "auto"]
        
      Svg.text
        (dir@[
              atf "x" a.X
              atf "y" a.Y
             ]
        ) str

    let drawText' (a : V2d) (str : string) (orientation : Orientation) = //TODO hack; need to change padding according to label lengh
      let dir = 
        [ats "glyph-orientation-horizontal" "auto"]
      let a =
        match orientation with
          | Orientation.Vertical -> a - V2d(float str.Length * 3.0, 0.0)
          | Orientation.Horizontal -> a
            
        
      Svg.text
        (dir@[
              atf "x" a.X
              atf "y" a.Y
             ]
        ) str

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

    let drawDottedLine (a : V2d) (b : V2d) 
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

    let drawVerticalDottedLine (a : V2d) (length : float) (color : C4b)// (strokeWidth : float) =
                               (strokeWidth : float) (dashWidth : float) (dashDist : float) =
      Svg.line 
        [ 
          ats "stroke-dasharray" (sprintf "%f,%f" dashWidth dashDist)
          //ats "stroke-dasharray" "5,5"
          atf "x1" a.X
          atf "y1" a.Y
          atf "x2" a.X 
          atf "y2" (a.Y + length)
          atc "stroke" color
          atf "stroke-width" strokeWidth
        ]



   


    
    

    let drawCircle (upperLeft : V2d) (radius : float) (color : C4b) (stroke : float) (fill : bool) = 
      let fillAttr =
        match fill with
          | true -> [atc "fill" color]
          | false -> [ats "fill" "none"]
      Svg.circle
        ([
          atf "cx" (upperLeft.X - radius)
          atf "cy" (upperLeft.Y - radius)
          atf "r" radius
          atc "stroke" C4b.Black //Performance
          atf "stroke-width" stroke
        ]@fillAttr)

    let drawCircle' (centre : V2d) (radius : float) (color : C4b) (stroke : float) (fill : bool) = 
      let upperLeft = centre + (new V2d (radius))
      drawCircle upperLeft radius color stroke fill

    let drawConcentricCircles (upperLeft : V2d) (radius : float) (color : C4b) (nrCircles : int) (weight : float) =
        let centre = upperLeft - (new V2d (radius))
        let dist = radius / (float nrCircles)
        let radii = 
          [1..nrCircles]
            |> List.map (fun i -> (float i) * dist)
        let circles =
          radii
            |> List.map (fun r -> drawCircle' centre r color weight false)
        circles

    let pointFromAngle (start : V2d) (angle : Angle) (length : float) =
      let endX = start.X + Math.Cos(angle.radians) * length
      let endY = start.Y + Math.Sin(angle.radians) * length
      new V2d (endX, endY)

    
    let drawCircleSegment (centre : V2d) (radius : float) 
                          (fromPoint : V2d) (toPoint : V2d)
                          (color : C4b) =
      let pathStr = 
        move centre 
          >> lineTo fromPoint 
          >> (circleSegmentTo radius fromPoint toPoint color)
      buildPath pathStr color 1.0 true

    let drawCircleSegment' (start : V2d) (angleFrom : Angle) (angleTo : Angle) 
                           (radius : float) (color : C4b) =
      let end1 = pointFromAngle start angleFrom radius
      let end2 = pointFromAngle start angleTo radius
      drawCircleSegment start radius  end1 end2 color


    let drawLineFromAngle (start : V2d) (angle : Angle) (length : float) (color : C4b) (width : float)=
      let endPoint = pointFromAngle start angle length
      drawLine start endPoint color width

    let starAngles16 =
        [1..16]
          |> List.map (fun i ->
                          let angle = Angle.eigthPi * (float i)
                          angle
                      )

    let mapToBin (angle : Angle) =
      let isInBin =
        [Angle.init 0.0]@starAngles16
          |> List.pairwise
          |> List.map (fun (lower,upper) -> lower.radians <= angle.radians && angle.radians <= upper.radians) //TODO comparable for angle
      let binIndex = 
        isInBin
          |> List.findIndex (fun b-> b)
      binIndex

      
        

    let drawStarLines' (angles :list<Angle>) (start : V2d) (radius : float) (color : C4b) (weight : float) =
      let lines =
        angles
          |> List.map (fun angle ->
                          drawLineFromAngle start angle radius color weight
                      )
      lines

    let draw16StarLines (start : V2d) (radius : float) (color : C4b) (weight : float) =
      let angles = starAngles16
      let lines =
        angles
          |> List.map (fun angle ->
                          drawLineFromAngle start angle radius color weight
                      )
      lines

    type Bin16x6 = {
      angularBin : int
      radius     : float
    }

    let getBinBorders binNr =
      let fromAngle = Angle.init ((starAngles16.Item binNr).radians - starAngles16.Head.radians) // TODO implement (-)
      let toAngle = starAngles16.Item binNr
      (fromAngle, toAngle)


    let circleSegment (centre : V2d) (bin : Bin16x6)
                      (radius : float) (color : C4b) =
      let angBin = bin.angularBin%15
      let fromAngle = starAngles16.Item angBin
      let toAngle = starAngles16.Item angBin
      drawCircleSegment' centre fromAngle toAngle radius color

    let drawRoseDiagram (leftUpper : V2d) (radius : float) 
                        (color : C4b) (nrCircles : int) 
                        (weight : float) (countPerBin : list<int * int>) =
      let centre = (leftUpper - (new V2d (radius)))
      let circles = drawConcentricCircles leftUpper radius color nrCircles weight
      let lines = draw16StarLines centre radius color weight
      let circleDist = radius / (float nrCircles)
      let circleRadii =
        [1..(nrCircles)]
          |> List.map (fun nr -> (float nr) * circleDist)
      let bins =
        countPerBin
          |> List.map (fun (bin, count) -> {angularBin = bin;radius = (circleRadii.Item (count - 1))})
      let filledBins =
        bins
          |> List.map (fun bin -> circleSegment centre bin radius color)
      toGroup (circles@lines@filledBins) []


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
          | true  -> drawVerticalDottedLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black 2.0 3.0 3.0
          | false -> drawVerticalLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black 2.0 

      toGroup 
        (elements @ [rBorder])
        (Svg.Events.onClickAttribute (selectionCallback))

    let drawLogarithmicXAxis (leftUpper : V2d) (length : float) (color : C4b) (weight : float) (granularity : float) =
      //TODO
      toGroup
        [
          drawHorizontalLine  (new V2d(leftUpper.X, leftUpper.Y + weight * 0.5)) length color weight
          drawHorizontalDottedLine (new V2d(leftUpper.X, leftUpper.Y + weight)) length color (weight * 3.0) 1.0 (granularity - 1.0)
        ]
        [] 

     
    let drawXAxis (leftUpper : V2d) (length : float) (color : C4b) (weight : float) (granularity : float) =
      toGroup
        [
          drawHorizontalLine  (new V2d(leftUpper.X, leftUpper.Y + weight * 0.5)) length color weight
          drawHorizontalDottedLine (new V2d(leftUpper.X, leftUpper.Y + weight)) length color (weight * 3.0) 1.0 (granularity - 1.0)
        ]
        []

    let drawYAxis (leftUpper : V2d) (length : float) (color : C4b) (weight : float) (granularity : float) =
      toGroup
        [
          drawVerticalLine  (new V2d(leftUpper.X, leftUpper.Y + weight * 0.5)) length color weight
          drawVerticalDottedLine (new V2d(leftUpper.X, leftUpper.Y + weight)) length color (weight * 3.0) 1.0 (granularity - 1.0)
        ]
        []
