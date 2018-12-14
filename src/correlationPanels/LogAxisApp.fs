namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogAxisApp =
  open Svgplus
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.Base.Incremental
  open UIPlus
  open SimpleTypes

  type Action = 
    | SetStyle of LogAxisConfigId
  
  let update (model : LogAxisApp) (action : Action) =
    match action with
      | SetStyle id -> {model with selectedTemplate = id}

  let invalidTemplate : LogAxisConfig =
    {
      id            = {id = System.Guid.NewGuid().ToString()}
      label         = "invalid"
      defaultRange        = {min = 0.0; max = 0.0}
      defaultGranularity  = 0.0
      //metricToSvgSize     = 0.0
      styleTemplate = 
        [{label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}}]
    }

  let grainsizeStyle2 : LogAxisConfig =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize"
      defaultRange        = {min = 0.0; max = 3000.0}
      defaultGranularity  = 10.0
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = 0.0 ;    max = 1.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 1.0 ;    max = 10.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 10.0 ;  max = 100.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 100.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let grainsizeStyle : LogAxisConfig =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize2"
      defaultRange        = {min = 0.0; max = 10.0}
      defaultGranularity  = 1.0
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = System.Double.NegativeInfinity ;    max = 1.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 1.0 ; max = 2.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 2.0 ; max = 4.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 4.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let initial : LogAxisApp = {
    templates = [grainsizeStyle;grainsizeStyle]
    selectedTemplate = grainsizeStyle.id
  }

  let selectedTemplateOrDefault (model : LogAxisApp) =
    let tm = model.templates |> List.tryFind ( fun x ->  x.id = model.selectedTemplate)
    match tm with
      | Some t -> t
      | None -> invalidTemplate


  let getStyle (model : MLogAxisApp) (xAxisScaleFactor : float) (x : float)  =
    model.selectedTemplate |> Mod.map (fun templId ->
      let templ = 
        (model.templates |> List.tryFind (fun t -> t.id.id = templId.id)
                         |> Option.defaultValue invalidTemplate)
      let mVal = x / xAxisScaleFactor
      templ.styleTemplate
              |> List.tryFind (fun t -> (t.range.min <= mVal) && (mVal < t.range.max))
              |> Option.defaultValue {label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}} )


  
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
               (yMapping      : float) 
               (nativeStep    : float)
               (fontSize      : int)
               (label         : string) =
      let domNodes =
        seq {
          // AXIS
          let svgLength = nativeYRange.range * yMapping
          yield Svgplus.Base.drawYAxis svgStartPoint svgLength C4b.Black weight (nativeStep * yMapping)
          let shift = centreShift label fontSize
          //TODO calc number label width first to determine required padding
          // AXIS LABEL //TODO hardcoded padding
          yield Svgplus.Base.drawBoldText (new V2d(svgStartPoint.X + 2.0*shift, svgStartPoint.Y + (svgLength * 0.5) + shift)) label Orientation.Vertical
          // NR LABELS
          let nrLabels = 
            makeNrLabels ((nativeYRange.max + nativeYRange.range * 0.1))
                         (-nativeStep) 
                         (nativeYRange.min)
                         (fun x -> x * yMapping)
                         svgStartPoint
                         Orientation.Vertical
                         fontSize
          yield! nrLabels
        }
      Svgplus.Attributes.toGroup (List.ofSeq domNodes) []//[attribute "font-size" "10px"]

  let svgYAxis' (opt : MSvgOptions)
                (nativeRange : IMod<Rangef>)
                (yMapping    : IMod<Option<float>>)
                (label       : IMod<string>) =
    adaptive {
      let! weight = opt.axisWeight
      let! step = opt.yAxisStep
      let! fontSize = opt.fontSize
      let! firstOffset = SvgOptions.firstLogOffset' opt
      let! padding = opt.logPadding
      let! nativeRange = nativeRange
      let! yMapping = yMapping
      let! label = label

      let startPoint = 
        (new V2d(firstOffset * 0.8, padding))

      return svgYAxis 
                      startPoint
                      nativeRange
                      weight
                      yMapping.Value //TODO using .Value
                      step
                      fontSize.fontSize
                      label //TODO hardcoded
    }



  let svgXAxis (template          : MLogAxisApp) 
               (startPoint        : V2d) 
               (opt               : MSvgOptions)
               (svgLength         : float) 
               (label             : IMod<string>) = 
    adaptive {
      let! templateId = template.selectedTemplate
      let! label = label
      let! xAxisScaleFactor = opt.xAxisScaleFactor
      let! weight = opt.axisWeight
      let! fontSize = opt.fontSize
      let templateOpt = template.templates |> List.tryFind (fun x -> x.id = templateId)
      let nativeLength = svgLength / xAxisScaleFactor
      let toSvg (x : float) = x * xAxisScaleFactor
      let res = 
        match templateOpt with
          | Some t ->
              let gr = 
                seq {
                  yield Svgplus.Base.drawXAxis startPoint svgLength C4b.Black weight (toSvg t.defaultGranularity)
                  let shift = centreShift label fontSize.fontSize
                  // AXIS LABEL
                  yield Svgplus.Base.drawBoldText (new V2d(startPoint.X + (svgLength * 0.5) + shift, startPoint.Y + 45.0)) label Orientation.Horizontal

                  // AXIS SECTION LABELS
                  //filter labels: only show relevant labels
                  let visibleLabels = 
                    t.styleTemplate |> List.filter (fun s -> s.range.min < nativeLength)

                  for i in 0..((visibleLabels.Length) - 1) do
                    let st = (t.styleTemplate.Item i)
                    let txt = st.label

                    let posX = 
                      match st.range.max, st.range.min with //TODO move into Rangef?
                                    | System.Double.PositiveInfinity, _ -> (startPoint.X + (((st.range.min * 2.0) - st.range.min)) * xAxisScaleFactor) 
                                    | _, System.Double.NegativeInfinity -> startPoint.X
                                    | max,min  -> (startPoint.X + ((max - min)) * xAxisScaleFactor)

                    // AXIS SECTION LABELS
                    yield Svgplus.Base.drawText (new V2d(posX, startPoint.Y + 30.0)) txt Orientation.Horizontal //TODO hardcoded


                  //NUMBER LABELS
                  let nrLabels = 
                    makeNrLabels 
                          0.0 
                          (t.defaultGranularity) 
                          ((nativeLength + nativeLength * 0.1)) 
                          toSvg 
                          startPoint
                          Orientation.Horizontal
                          fontSize.fontSize
                  yield! nrLabels 
                }
              Svgplus.Attributes.toGroup (List.ofSeq gr) [] //[attribute "font-size" "10px"] //TODO hardcoded font size
          | None -> 
              Svgplus.Base.drawXAxis startPoint svgLength C4b.Black 2.0 10.0
      return res
    }


  let view (model : MLogAxisApp) = 
    alist {
      let! selId = model.selectedTemplate
      for styleTemplate in model.templates do
        match styleTemplate.id with
          | id when id = selId ->
              yield Buttons.Incremental.iconButton 
                      "small yellow arrow left icon" 
                      (fun _ -> SetStyle styleTemplate.id)
          | _ ->
              yield Buttons.Incremental.iconButton 
                      "small arrow left icon" 
                      (fun _ -> SetStyle styleTemplate.id)

    }






