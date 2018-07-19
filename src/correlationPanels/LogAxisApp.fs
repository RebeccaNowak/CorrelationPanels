﻿namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogAxisApp =
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.Base.Incremental
  open UtilitiesGUI
  open Aardvark.Base


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


  
  let makeIndexList (a : int) (step: int) (b : int) = 
    seq {a.. step ..b} |> Seq.toList

  let indToPos indList (mapper : float -> float) (step : int) =
    indList|> List.map (fun x -> (float x) * (mapper (float step)))

  let indToText indList (step : int) =
    indList 
      |> List.map (fun x -> sprintf "%i" (x * step))

  let centreShift (label : string) =
    -((float label.Length) * 2.8)

  let makeNrLabels' (nrList : list<int>) (txtList : list<string>) (posList : list<float>) (startPoint : V2d) =
    seq {
      for i in 0..((List.length nrList) - 1) do
        let txt = (txtList.Item i)
        let shift = centreShift txt
        yield Svg.drawText (new V2d((posList.Item i) + (startPoint.X + shift), (startPoint.Y + 15.0))) txt
    } |> Seq.toList

  let makeNrLabels (a : int) (step : int) (b : int) (mapper: float -> float) (startPoint : V2d) =
    let nrList = makeIndexList a step b
    let txtList = indToText nrList step
    let posList = indToPos nrList mapper step
    makeNrLabels' nrList txtList posList startPoint

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
                //let labelIndices = seq { 0..  (int t.defaultGranularity) .. (int (nativeLength + nativeLength * 0.1))} //(int t.defaultRange.max)} //TODO round
                //                      |> List.ofSeq
                //let labelPositions = labelIndices |> List.map (fun x -> (float x) * (toSvg t.defaultGranularity))
                //let labelText = labelIndices |> List.map (fun x -> sprintf "%.0f" ((float x) * t.defaultGranularity))
                seq {
                  yield Svg.drawXAxis startPoint svgLength C4b.Black 2.0 (toSvg t.defaultGranularity)
                  let shift = centreShift label
                  // axis label
                  yield Svg.drawText (new V2d(startPoint.X + (svgLength * 0.5) + shift, startPoint.Y + 45.0)) label

                  // text labels
                  //filter labels: only show relevant labels
                  let visibleLabels = 
                    t.styleTemplate |> List.filter (fun s -> s.range.min < nativeLength)

                  for i in 0..((visibleLabels.Length) - 1) do
                    let st = (t.styleTemplate.Item i)
                    let txt = st.label
                    //let leftShift = ((float txt.Length) * 3.0) //TODO create function
                          
                    let posX = 
                      match st.range.max, st.range.min with //TODO move into Rangef
                                    | System.Double.PositiveInfinity, _ -> (startPoint.X + (((st.range.min * 2.0) - st.range.min)) * xAxisScaleFactor) 
                                    | _, System.Double.NegativeInfinity -> startPoint.X
                                    | max,min  -> (startPoint.X + ((max - min)) * xAxisScaleFactor)
                    yield Svg.drawText (new V2d(posX, startPoint.Y + 30.0)) txt //TODO hardcoded


                  //number labels
                  let nrLabels = 
                    makeNrLabels 0 (int t.defaultGranularity) (int (nativeLength + nativeLength * 0.1)) toSvg startPoint
                  yield (Svg.toGroup nrLabels [])
                  //for i in 0..((List.length labelIndices) - 1) do
                  //  let txt = (labelText.Item i)
                  //  let shift = centreShift txt
                  //  yield Svg.drawText (new V2d((labelPositions.Item i) + (startPoint.X + shift), (startPoint.Y + 15.0))) txt
                }
              Svg.toGroup (List.ofSeq gr) [attribute "font-size" "10px"]
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






