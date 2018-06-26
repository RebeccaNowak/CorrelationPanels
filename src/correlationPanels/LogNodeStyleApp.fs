namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogNodeStyleApp =
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.Base.Incremental
  open UtilitiesGUI
  open Aardvark.Base


  type Action = 
    | SetStyle of LNStyleListId
  
  let update (model : LogNodeStyleApp) (action : Action) =
    match action with
      | SetStyle id -> {model with selectedTemplate = id}


  let invalidTemplate : LogNodeStyleTemplate =
    {
      id            = {id = System.Guid.NewGuid().ToString()}
      label         = "invalid"
      defaultRange        = {min = 0.0; max = 0.0}
      defaultGranularity  = 0.0
      metricToSvgSize     = 0.0
      styleTemplate = 
        [{label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}}]
    }

  let grainsizeStyle : LogNodeStyleTemplate =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize"
      defaultRange        = {min = 0.0; max = 3000.0}
      defaultGranularity  = 500.0
      metricToSvgSize     = 0.2 //TODO needs to be logarithmic
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = 0.0 ;    max = 2.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 2.0 ;    max = 100.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 100.0 ;  max = 2000.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 2000.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let grainsizeStyle2 : LogNodeStyleTemplate =
    {
      id                  = {id = System.Guid.NewGuid().ToString()}
      label               = "grainsize2"
      defaultRange        = {min = 0.0; max = 10.0}
      defaultGranularity  = 1.0
      metricToSvgSize     = 30.0
      styleTemplate       = 
        [
          {label = "clay";  color = new C4b(99,99,99);    range = {min = System.Double.NegativeInfinity ;    max = 2.0}}
          {label = "silt";  color = new C4b(255,247,188); range = {min = 1.0 ;    max = 2.0}}
          {label = "sand";  color = new C4b(254,196,79);  range = {min = 2.0 ;  max = 4.0}}
          {label = "gravel";color = new C4b(217,95,14);   range = {min = 4.0 ; max = System.Double.PositiveInfinity}}
        ]
    }

  let initial : LogNodeStyleApp = {
    templates = [grainsizeStyle;grainsizeStyle2]
    selectedTemplate = grainsizeStyle2.id
  }

  let selectedTemplateOrDefault (model : LogNodeStyleApp) =
    let tm = model.templates |> List.tryFind ( fun x ->  x.id = model.selectedTemplate)
    match tm with
      | Some t -> t
      | None -> invalidTemplate


  let getStyle (model : MLogNodeStyleApp) (x : float) =
    model.selectedTemplate |> Mod.map (fun templId ->
      let templ = 
        (model.templates |> List.tryFind (fun t -> t.id.id = templId.id)
                         |> Option.defaultValue invalidTemplate)
      let mVal = x / templ.metricToSvgSize
      templ.styleTemplate
              |> List.tryFind (fun t -> (t.range.min <= mVal) && (mVal < t.range.max))
              |> Option.defaultValue {label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}} )


  


  let svgXAxis (template : MLogNodeStyleApp) (startPoint : V2d) (length : float) (weight : float) (label : IMod<string>) = 
    adaptive {
      let! templateId = template.selectedTemplate
      let! label = label
      let templateOpt = template.templates |> List.tryFind (fun x -> x.id = templateId)
      let res = match templateOpt with
                | Some t ->
                    let gr = 
                      let toSvg (x : float) = 
                        x * (((startPoint.X + length) - startPoint.X) / t.defaultRange.range)
                      let labelIndices = seq { 0..  (int t.defaultGranularity) .. (int t.defaultRange.max)} //TODO round
                                           |> List.ofSeq
                      let labelPositions = labelIndices |> List.map (fun x -> (float x) * (toSvg t.defaultGranularity))
                      let labelText = labelIndices |> List.map (fun x -> sprintf "%.0f" ((float x) * t.defaultGranularity))
                      seq {
                        yield Svg.drawXAxis startPoint length C4b.Black 2.0 (toSvg t.defaultGranularity)
                        let leftShift = ((float label.Length) * 3.0)
                        // axis label
                        yield Svg.drawText (new V2d(startPoint.X + (length * 0.5) - leftShift, startPoint.Y + 40.0)) label
                        // text labels
                        //for i in 0..((t.styleTemplate.Length) - 1) do
                        //  let st = (t.styleTemplate.Item i)
                        //  let leftShift = ((float st.label.Length) * 3.0) //TODO create function
                        //  let txt = st.label
                        //  let max = match st.range.max with //TODO move into Rangef
                        //               | System.Double.PositiveInfinity -> st.range.min * 2.0
                        //               | _ -> st.range.max
                        //  let min = match st.range.min with
                        //               | System.Double.NegativeInfinity -> 0.0
                        //               | _ -> st.range.min
                        //  let posX =  metricToSvgSize (startPoint.X + ((max - min)))
                        //  yield Svg.drawText (new V2d(posX - leftShift, startPoint.Y + 30.0)) txt
                        //number labels
                        for i in 0..((List.length labelIndices) - 1) do
                          let txt = (labelText.Item i)
                          let leftShift = ((float txt.Length) * 3.0)
                          yield Svg.drawText (new V2d((labelPositions.Item i) + (startPoint.X - leftShift), (startPoint.Y + 15.0))) txt
                      }
                    Svg.toGroup (List.ofSeq gr) [attribute "font-size" "10px"]
                | None -> 
                    Svg.drawXAxis startPoint length C4b.Black 2.0 10.0
      return res
    }

  //let getStyle' (model : MLogNodeStyleApp) (n : MLogNode)  =
  //  let el =  Mod.force (LogNode.elevation n) //TODO make adaptive

  //  model.selectedTemplate |> Mod.map (fun templ ->
  //    (model.templates |> List.filter (fun t -> t.id.id = templ.id)
  //                     |> List.tryHead
  //                     |> Option.defaultValue invalidTemplate).styleTemplate
  //                     |> List.filter (fun t -> (t.range.min <= el) && (el < t.range.max))
  //                     |> List.tryHead
  //                     |> Option.defaultValue {label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}} )
      
    //let st =
    //  styleTemplate.styleTemplate 
    //    |> List.filter (fun t -> (t.range.min <= el) && (el < t.range.max))
    //    |> List.tryHead
    //match st with
    //  | Some t -> t
    //  | None   -> {label = "Invalid";color = new C4b(255,0,0);   range = {min = 0.0 ; max = 0.0}}


    

  let view (model : MLogNodeStyleApp) = 
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






