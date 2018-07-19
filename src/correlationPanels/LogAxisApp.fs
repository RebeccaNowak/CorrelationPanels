namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogAxisApp =
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.Base.Incremental
  open UtilitiesGUI

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

  let grainsizeStyle : LogAxisConfig =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize"
      defaultRange        = {min = 0.0; max = 3000.0}
      defaultGranularity  = 500.0
      //metricToSvgSize     = 0.2 //TODO needs to be logarithmic
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = 0.0 ;    max = 2.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 2.0 ;    max = 100.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 100.0 ;  max = 2000.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 2000.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let grainsizeStyle2 : LogAxisConfig =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize2"
      defaultRange        = {min = 0.0; max = 10.0}
      defaultGranularity  = 1.0 //TODO can't be < 1.0
      //metricToSvgSize     = 30.0
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = System.Double.NegativeInfinity ;    max = 1.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 1.0 ; max = 2.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 2.0 ; max = 4.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 4.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let initial : LogAxisApp = {
    templates = [grainsizeStyle;grainsizeStyle2]
    selectedTemplate = grainsizeStyle2.id
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

  let centreShift (label : string) =
    -((float label.Length) * 2.8)

  let makeNrLabels' (txtList    : list<string>) 
                    (posList    : list<float>) 
                    (startPoint : V2d)  
                    (dir        : Orientation) =
    seq {
      for i in 0..((List.length posList) - 1) do
        let txt = (txtList.Item i)
        let shift = centreShift txt
        let pos = 
          match dir with
            | Orientation.Horizontal ->
              (new V2d((posList.Item i) + (startPoint.X + shift), (startPoint.Y + 15.0))) //TODO hardcoded
            | Orientation.Vertical ->
              (new V2d((startPoint.X - 10.0), (posList.Item i) + (startPoint.Y + shift)))
        yield Svg.drawText pos txt dir
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
               (nativeStep    : float) //TODO step is truncated to int!!
               (label         : string) =
      let domNodes =
        seq {
          // AXIS
          let svgLength = nativeYRange.range * yMapping
          yield Svg.drawYAxis svgStartPoint svgLength C4b.Black weight (nativeStep * yMapping)
          let shift = centreShift label
          // AXIS LABEL
          yield Svg.drawText (new V2d(svgStartPoint.X - 20.0, svgStartPoint.Y + (svgLength * 0.5) + shift)) label Orientation.Vertical
          // NR LABELS
          let nrLabels = 
            makeNrLabels ((nativeYRange.max + nativeYRange.range * 0.1))
                         (-nativeStep) 
                         (nativeYRange.min)
                         (fun x -> x * yMapping)
                         svgStartPoint
                         Orientation.Vertical
          yield! nrLabels
        }
      Svg.toGroup (List.ofSeq domNodes) [attribute "font-size" "10px"]

  let svgXAxis (template : MLogAxisApp) (startPoint : V2d) (svgLength : float) (weight : float) (xAxisScaleFactor : float) (label : IMod<string>) = 
    adaptive {
      let! templateId = template.selectedTemplate
      let! label = label
      let templateOpt = template.templates |> List.tryFind (fun x -> x.id = templateId)
      let nativeLength = svgLength / xAxisScaleFactor
      let toSvg (x : float) = x * xAxisScaleFactor
      let res = 
        match templateOpt with
          | Some t ->
              let gr = 
                seq {
                  yield Svg.drawXAxis startPoint svgLength C4b.Black weight (toSvg t.defaultGranularity)
                  let shift = centreShift label
                  // AXIS LABEL
                  yield Svg.drawText (new V2d(startPoint.X + (svgLength * 0.5) + shift, startPoint.Y + 45.0)) label Orientation.Horizontal

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
                    yield Svg.drawText (new V2d(posX, startPoint.Y + 30.0)) txt Orientation.Horizontal //TODO hardcoded


                  //NUMBER LABELS
                  let nrLabels = 
                    makeNrLabels 
                          0.0 
                          (t.defaultGranularity) 
                          ((nativeLength + nativeLength * 0.1)) 
                          toSvg 
                          startPoint
                          Orientation.Horizontal
                  yield! nrLabels 
                }
              Svg.toGroup (List.ofSeq gr) [attribute "font-size" "10px"] //TODO hardcoded font size
          | None -> 
              Svg.drawXAxis startPoint svgLength C4b.Black 2.0 10.0
      return res
    }


  let view (model : MLogAxisApp) = 
    alist {
      let! selId = model.selectedTemplate
      for styleTemplate in model.templates do
        match styleTemplate.id with
          | id when id = selId ->
              yield Incremental.iconButton 
                      "small yellow arrow left icon" 
                      (fun _ -> SetStyle styleTemplate.id)
          | _ ->
              yield Incremental.iconButton 
                      "small arrow left icon" 
                      (fun _ -> SetStyle styleTemplate.id)

    }






