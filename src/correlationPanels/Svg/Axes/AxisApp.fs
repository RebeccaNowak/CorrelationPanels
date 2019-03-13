namespace Svgplus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AxisApp =
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.Base.Incremental
  open UIPlus
  open SimpleTypes
  open Svgplus.AxesTypes
  open Svgplus.Base


  let initial yMapping nativeRange: AxisApp = 
    {
      positionTop     = V2d(0)
      weight          = 2.0
      step            = 1.0
      label  = "axis label"
      defaultGranularity = 1.0
      yMapping = yMapping
      nativeRange = nativeRange
      fontSize = FontSize.defaultSize
      draw     = true
    }
  
  let makeIndexList (a : float) (step: float) (b : float) = 
    seq {a.. step ..b} |> Seq.toList

  let indToPos indList (mapper : float -> float) (step : float) =
    indList|> List.map (fun x -> x * (mapper step))

  let indToText indList (step : float) =
    indList 
      |> List.map (fun x -> sprintf "%.0f" (x * step))

  let centreShift (label : string) (fontSize : int) =
    -((float label.Length) * (float fontSize / 4.0))  //* 2.8) //TODO hardcoded

  let makeNrLabels' (txtList    : list<string>) 
                    (posList    : list<float>) 
                    (startPoint : V2d)  
                    (dir        : Orientation)
                    (fontSize   : int) =
    seq {
      for i in 0..((List.length posList) - 1) do
        let txt = (txtList.Item i)
        let shift = centreShift txt fontSize
        let pos = 
          match dir with
            | Orientation.Horizontal ->
              (new V2d((posList.Item i) + (startPoint.X + shift), (startPoint.Y + 15.0))) //TODO hardcoded
            | Orientation.Vertical ->
              (new V2d((startPoint.X + shift*2.0), (posList.Item i) + (startPoint.Y + shift)))
        yield Svgplus.Base.drawText' pos txt dir // could rotate labels with Svg.drawText
    } |> Seq.toList

  let makeNrLabels (a : float) (step : float) (b : float) (mapper: float -> float) (startPoint : V2d) (dir : Orientation) =
    let nrList  = (makeIndexList a step b) 
    let (txtList, posList) =
      match step < 0.0 with
        | true ->
            let txtList = (indToText nrList -step) 
            let posList = indToPos (makeIndexList 0.0 -step (float nrList.Length - 1.0)) mapper -step
            (txtList, posList)
        | false ->
            let nrList = makeIndexList a step b
            let txtList = indToText nrList step
            let posList = indToPos (makeIndexList 0.0 step (float nrList.Length - 1.0)) mapper step
            (txtList, posList)
    makeNrLabels' txtList.[0..nrList.Length-2] posList.[0..nrList.Length-2] startPoint dir
   

  let svgYAxis (svgStartPoint : V2d) 
               (nativeYRange  : Rangef)
               (weight        : float) 
               (yMapping      : float -> float) 
               (nativeStep    : float)
               (fontSize      : int)
               (label         : string) =
      let domNodes =
        seq {
          // AXIS
          let svgLength = yMapping nativeYRange.range
          yield Svgplus.Base.drawYAxis svgStartPoint svgLength C4b.Black weight (yMapping nativeStep)
          let shift = centreShift label fontSize
          //TODO calc number label width first to determine required padding
          // AXIS LABEL //TODO hardcoded padding
          yield Svgplus.Base.drawBoldText (new V2d(svgStartPoint.X + 2.0*shift, svgStartPoint.Y + (svgLength * 0.5) + shift)) label Orientation.Vertical
          // NR LABELS
          let nrLabels = 
            makeNrLabels ((nativeYRange.max + nativeYRange.range * 0.1))
                         (-nativeStep) 
                         (nativeYRange.min)
                         (fun x -> yMapping x)
                         svgStartPoint
                         Orientation.Vertical
                         fontSize
          yield! nrLabels
        }
      Svgplus.Attributes.toGroup (List.ofSeq domNodes) []//[attribute "font-size" "10px"]

  let view (model       : MAxisApp) =
    adaptive {
      let! weight = model.weight
      let! step = model.step
      let! fontSize = model.fontSize
      let! nativeRange = model.nativeRange
      let! yMapping = model.yMapping
      let! label = model.label

      let! startPoint = model.positionTop

      return svgYAxis 
                      startPoint
                      nativeRange
                      weight
                      yMapping
                      step
                      (fontSize.fontSize)
                      label //TODO hardcoded
    }



  //let svgXAxis (template          : MLogAxisApp) 
  //             (startPoint        : V2d) 
  //             (opt               : MSvgOptions)
  //             (svgLength         : float) 
  //             (label             : IMod<string>) = 
  //  adaptive {
  //    let! templateId = template.selectedTemplate
  //    let! label = label
  //    let! xAxisScaleFactor = opt.xAxisScaleFactor
  //    let! weight = opt.axisWeight
  //    let! fontSize = opt.fontSize
  //    let templateOpt = template.templates |> List.tryFind (fun x -> x.id = templateId)
  //    let nativeLength = svgLength / xAxisScaleFactor
  //    let toSvg (x : float) = x * xAxisScaleFactor
  //    let res = 
  //      match templateOpt with
  //        | Some t ->
  //            let gr = 
  //              seq {
  //                yield Svgplus.Base.drawXAxis startPoint svgLength C4b.Black weight (toSvg t.defaultGranularity)
  //                let shift = centreShift label fontSize.fontSize
  //                // AXIS LABEL
  //                yield Svgplus.Base.drawBoldText (new V2d(startPoint.X + (svgLength * 0.5) + shift, startPoint.Y + 45.0)) label Orientation.Horizontal

  //                // AXIS SECTION LABELS
  //                //filter labels: only show relevant labels
  //                let visibleLabels = 
  //                  t.styleTemplate |> List.filter (fun s -> s.range.min < nativeLength)

  //                for i in 0..((visibleLabels.Length) - 1) do
  //                  let st = (t.styleTemplate.Item i)
  //                  let txt = st.label

  //                  let posX = 
  //                    match st.range.max, st.range.min with //TODO move into Rangef?
  //                                  | System.Double.PositiveInfinity, _ -> (startPoint.X + (((st.range.min * 2.0) - st.range.min)) * xAxisScaleFactor) 
  //                                  | _, System.Double.NegativeInfinity -> startPoint.X
  //                                  | max,min  -> (startPoint.X + ((max - min)) * xAxisScaleFactor)

  //                  // AXIS SECTION LABELS
  //                  yield Svgplus.Base.drawText (new V2d(posX, startPoint.Y + 30.0)) txt Orientation.Horizontal //TODO hardcoded


  //                //NUMBER LABELS
  //                let nrLabels = 
  //                  makeNrLabels 
  //                        0.0 
  //                        (t.defaultGranularity) 
  //                        ((nativeLength + nativeLength * 0.1)) 
  //                        toSvg 
  //                        startPoint
  //                        Orientation.Horizontal
  //                        fontSize.fontSize
  //                yield! nrLabels 
  //              }
  //            Svgplus.Attributes.toGroup (List.ofSeq gr) [] //[attribute "font-size" "10px"] //TODO hardcoded font size
  //        | None -> 
  //            Svgplus.Base.drawXAxis startPoint svgLength C4b.Black 2.0 10.0
  //    return res
  //  }


  //let view (model : MLogAxisApp) = 
  //  alist {
  //    let! selId = model.selectedTemplate
  //    for styleTemplate in model.templates do
  //      match styleTemplate.id with
  //        | id when id = selId ->
  //            yield Buttons.Incremental.iconButton 
  //                    "small yellow arrow left icon" 
  //                    (fun _ -> SetStyle styleTemplate.id)
  //        | _ ->
  //            yield Buttons.Incremental.iconButton 
  //                    "small arrow left icon" 
  //                    (fun _ -> SetStyle styleTemplate.id)

  //  }






