namespace Svgplus

  module Base =
    open Aardvark.Base
    open System
    open SimpleTypes
    open SimpleTypes.Math
    open Aardvark.UI
    open Svgplus.Attributes
    open Svgplus.Paths


    let inline b0 (a : int) =
      if a < 0 then 0 else a

    let inline (%%) (v : V3i) (a : int) =
      V3i((b0 v.X%a),(b0 v.Y%a),(b0 v.Z%a)) 

    let V3iToC4b (v : V3i) =
      let _v = v%%255
      new C4b(v)

    let inline (--) (c : C4b) (v : V3i) =
      let col = c.ToV3i ()
      V3iToC4b (col - v)

    let inline (++) (c : C4b) (v : V3i) =
      let col = c.ToV3i ()
      V3iToC4b (col + v)

    let lighten (colour : C4b) =
      C4b(int colour.R - 20, int colour.G - 20, int colour.B - 20)

    let gradient (rgb1 : V3d) (rgb2 : V3d) = 
      let RGBToC4b (rgb : V3d) =
        C4b(rgb.X |> Math.Round |> int, 
            rgb.Y |> Math.Round |> int, 
            rgb.Z |> Math.Round |> int)

      let diff = (rgb1 - rgb2) / 8.0 

      let cs = 
        [0 .. 15]
          |> List.map (fun x -> 
                        match x with
                          | a when a <= 8 -> 
                            let rgb = (rgb1 - (float a) * diff)
                            RGBToC4b rgb
                          | a when a >  8 ->
                            let rgb = (rgb2 +  float (a - 8) * diff)
                            RGBToC4b rgb
                          | _ -> C4b.VRVisGreen
                      )
      cs

    let gradient_blue_green =
      let green = V3d(50,180,30)
      let blue  = V3d(50,30,180)
      gradient green blue

    let gradient_blue_red = 
      let red   = V3d(180,50,30)
      let blue  = V3d(50,30,180)
      gradient blue red


    let margin = 5.0
    let sw = "3"

    let clickableRectangle' (centre: V2d) (width : float) (height : float) (fOnClick : _ -> 'a) =
      let leftUpper = V2d(centre.X - width * 0.5, centre.Y - height * 0.5)
      Svg.rect ([
                  clazz "clickable"
                  atf "x" leftUpper.X
                  atf "y" leftUpper.Y
                  atf "width" width
                  atf "height" height
                ]@(Aardvark.UI.Svg.Events.onClickAttributes [fOnClick]))

    let clickableRectangle (centre: V2d) (radius : float) (fOnClick : _ -> 'a) =
      let leftUpper = centre - radius
      Svg.rect ([
                  clazz "clickable"
                  atf "x" leftUpper.X
                  atf "y" leftUpper.Y
                  atf "width" (radius * 2.0)
                  atf "height" (radius * 2.0)
                ]@(Aardvark.UI.Svg.Events.onClickAttributes [fOnClick]))

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
          | true -> [atc "fill" color; atc "stroke" C4b.Black]
          | false -> [ats "fill" "none"; atc "stroke" color]
      Svg.circle
        ([
          atf "cx" (upperLeft.X - radius)
          atf "cy" (upperLeft.Y - radius)
          atf "r" radius
          atf "stroke-width" stroke
        ]@fillAttr)

    let drawCircle' (centre : V2d) (radius : float) (color : C4b) (stroke : float) (fill : bool) = 
      let upperLeft = centre + (new V2d (radius))
      drawCircle upperLeft radius color stroke fill

    let drawConcentricCircles (upperLeft : V2d) (outerRadius : float) 
                              (innerRadius : float) (circleDist : float) 
                              (color : C4b) (nrCircles : int) 
                              (weight : float) =
        let centre = upperLeft - (new V2d (outerRadius))
        let radii = 
          [0..nrCircles]
            |> List.map (fun i -> innerRadius + (float i) * circleDist)
        let circles =
          radii
            |> List.map (fun r -> drawCircle' centre r color weight false)
        circles

    let drawConcentricCircles' (centre : V2d) (outerRadius : float)
                               (innerRadius : float) (color : C4b) 
                               (nrCircles : int) (circleDist : float)
                               (weight : float) =
        let upperLeft = centre + (new V2d (outerRadius))
        drawConcentricCircles upperLeft outerRadius innerRadius circleDist color nrCircles weight


    let pointFromAngle (start : V2d) (angle : Angle) (length : float) =
      let endX = start.X + Math.Cos(angle.radians) * length
      let endY = start.Y + Math.Sin(angle.radians) * length
      new V2d (endX, endY)

    
    let drawCircleSegment (centre : V2d) (radius : float) 
                          (fromPoint : V2d) (toPoint : V2d)
                          (color : C4b) =
      let pathStr = 
         move fromPoint 
          >> (circleSegmentTo radius toPoint)
          >> (lineTo centre)
          >> close
      buildPath pathStr color 1.0 true

    let drawCircleSegment' (start : V2d) (angleFrom : Angle) (angleTo : Angle) 
                           (radius : float) (color : C4b) =
      let end1 = pointFromAngle start angleFrom radius
      let end2 = pointFromAngle start angleTo radius
      drawCircleSegment start radius  end1 end2 color

    let drawDonutSegment  (centre : V2d) 
                          (outerRadius : float) 
                          (innerRadius : float)
                          (angleFrom : Angle) (angleTo : Angle)
                          (color : C4b) =

      let startInner = pointFromAngle centre angleFrom innerRadius
      let endInner   = pointFromAngle centre angleTo   innerRadius
      let startOuter = pointFromAngle centre angleFrom outerRadius
      let endOuter   = pointFromAngle centre angleTo   outerRadius
            
      let pathStr = 
         move startInner 
          >> (circleSegmentTo' innerRadius endInner)
          >> (lineTo endOuter)
          >> (circleSegmentTo outerRadius startOuter)
          >> close
      buildPath pathStr color 1.0 true


    let drawLineFromAngle (start : V2d) (angle : Angle) 
                          (length : float) 
                          (color : C4b) (width : float) =
      let endPoint = pointFromAngle start angle length
      drawLine start endPoint color width

    let starAngles16 =
        [0..15]
          |> List.map (fun i ->
                          let angle = Angle.sixteenthPi + (Angle.eigthPi * (float i))
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

      
        

    let drawStarLines' (angles :list<Angle>) (centre : V2d) 
                       (radius : float) (innerRadius : float) 
                       (color : C4b) (weight : float) =
      let lines =
        angles
          |> List.map (fun angle ->
                          let start = pointFromAngle centre angle innerRadius
                          drawLineFromAngle start angle (radius - innerRadius) color weight
                      )
      lines

    let draw16StarLines (start : V2d) (radius : float) (innerRadius : float) (color : C4b) (weight : float) =
      let angles = starAngles16
      let lines =
        angles
          |> List.map (fun angle ->
                          let start = pointFromAngle start angle innerRadius
                          drawLineFromAngle start angle (radius - innerRadius) color weight
                      )
      lines



    let getBinBorders binNr =
      let fromAngle = Angle.init ((starAngles16.Item binNr).radians - starAngles16.Head.radians) // TODO implement (-)
      let toAngle = starAngles16.Item binNr
      (fromAngle, toAngle)


    let circleSegment (centre : V2d) (bin : Bin16x6)
                      (color : C4b) =
      let angBin = bin.angularBin%15
      let fromAngle = starAngles16.Item angBin
      let toAngle = starAngles16.Item (angBin + 1)
      drawCircleSegment' centre fromAngle toAngle bin.radius color

    let donutSegment (centre : V2d) 
                     (innerRadius : float)
                     (bin : Bin16x6) =
      let fromAngle = starAngles16.Item bin.angularBin
      let toAngle = starAngles16.Item ((bin.angularBin + 1)%16)
      drawDonutSegment centre bin.radius innerRadius fromAngle toAngle bin.colour


    //let drawRoseDiagram (leftUpper : V2d) (outerRadius : float)  (innerRadius : float)
    //                    (color : C4b) (nrCircles : int) 
    //                    (weight : float) (countPerBin : list<int * int>) =
    //  let circleDist = (outerRadius - innerRadius) / (float nrCircles)
      
    //  let centre = (leftUpper - (new V2d (outerRadius)))
    //  let circles = drawConcentricCircles' centre outerRadius innerRadius C4b.Gray nrCircles circleDist 0.5
      
    //  let lines = draw16StarLines centre outerRadius innerRadius C4b.Gray weight
      
    //  let circleRadii =
    //    [1..(nrCircles)]
    //      |> List.map (fun nr -> (float nr) * circleDist)
    //  let bins =
    //    countPerBin
    //      |> List.map (fun (bin, count) -> 
    //                      {
    //                        angularBin = bin;
    //                        radius = innerRadius + (circleRadii.Item (count - 1))
    //                        colour = bin.})
    //  let filledBins =
    //    bins
    //      |> List.map (fun bin -> donutSegment centre innerRadius bin color)
    //  toGroup (circles@lines@filledBins) []





      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
      ] (Svg.Events.onClickToggleButton (callback))

        

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

    let drawBorderedRectangle (leftUpper         : V2d) 
                              (size              : Size2D)
                              (fill              : C4b) 
                              (lowerBorderColor  : C4b)
                              (upperBorderColor  : C4b)
                              (bWeight           : SvgWeight)
                              (selectionCallback : list<string> -> 'msg)
                              (selected          : bool)
                              (dottedBorder      : bool) =
      let width = size.width
      let height = size.height
      let uBorder = lowerBorderColor
      let lBorder = upperBorderColor

      let _bweight =
        match selected with //TODO read papers: mark selection state
          | true  -> bWeight.value * 2.0
          | false -> bWeight.value
      let elements = 
          [  
            drawRectangle leftUpper width height fill
            drawHorizontalLine 
              (new V2d(leftUpper.X, leftUpper.Y + _bweight * 0.5)) 
              width lBorder _bweight
            drawHorizontalLine 
              (new V2d(leftUpper.X, leftUpper.Y + height - _bweight * 0.5)) 
              width uBorder _bweight
            drawVerticalLine leftUpper height C4b.Black _bweight
          ]
      let rBorder = 
        match dottedBorder with
          | true  -> drawVerticalDottedLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black _bweight 3.0 3.0
          | false -> drawVerticalLine (new V2d(leftUpper.X + width , leftUpper.Y)) height C4b.Black _bweight 

      toGroup 
        (elements @ [rBorder])
        (Svg.Events.onClickToggleButton (selectionCallback))

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
