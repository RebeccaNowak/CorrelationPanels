namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    //open Aardvark.Base.Incremental.Operators
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UtilitiesGUI
    open Aardvark.UI.Primitives
    open Aardvark.Base.MultimethodTest

    type Action =
      | Clear
      | ToggleSelectLog of option<string>
      | NewLog          
      | TogglePoint     of (V3d * Annotation)
      | FinishLog       
      | DeleteLog       
      | LogMessage      of GeologicalLog.Action
      | ChangeView      of LogNodeView

    let initial : CorrelationPlotApp = {
      logs                = PList.empty
      selectedPoints      = List<(V3d * Annotation)>.Empty
      selectedLog         = None
      creatingNew         = false
      viewType            = LogNodeView.StackedView2ColorBoxes
    }

    let update (annos : plist<Annotation>) 
               (semApp : SemanticApp) 
               (action : Action) 
               (model : CorrelationPlotApp) = 
      match action, model.creatingNew with
        | Clear, _                     ->
          {model with logs = PList.empty
                      selectedPoints      = List<(V3d * Annotation)>.Empty
                      selectedLog         = None
                      creatingNew         = false
          }
        | ToggleSelectLog oStr, false  -> 
          match (oStr = model.selectedLog) with
            | true  -> {model with selectedLog = None}
            | false -> {model with selectedLog = oStr}
        | NewLog, false                -> 
          {model with creatingNew     = true
                      selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog, true              ->
          match model.selectedPoints with
            | []      -> 
              printf "no points in list for creating log"
              model
            | working ->
              {model with creatingNew = false
                          logs        = (model.logs.Append 
                                          (GeologicalLog.intial (System.Guid.NewGuid().ToString()) working annos semApp))
                          selectedPoints     = List<(V3d * Annotation)>.Empty}
        | DeleteLog, false             -> model
        | LogMessage m, _              -> model//{model with logs = model.logs.Update m}
        | ChangeView m, _              -> {model with viewType = m}
        | _,_                          -> model


    let viewSvg (model : MCorrelationPlotApp) (semApp : MSemanticApp) =
      let atts = 
        AttributeMap.ofList 
          [
            clazz "svgRoot"; 
            style "border: 1px solid black";
            attribute "height" "100%"
            attribute "width" "100%"
          ]
      
      
      let svgList =
        alist {          //TODO more elegant
          let! length = (AList.count model.logs)
          let! logs = model.logs.Content
          let! viewType = model.viewType
          for i in [0..length - 1] do //log in model.logs do
            let! sel = model.selectedLog
            let isSelected = 
              match sel with
                | Some s  -> s = (logs.Item i).id
                | None    -> false              
            let log = logs.Item i
            let attributes =
              match isSelected with
                | true  -> [style "border: 2px solid yellow";]
                | false -> []
              @ [
                  attribute "x" (sprintf "%i" (i * 250))
                  onMouseClick (fun _ -> ToggleSelectLog (Some log.id))
                ]
              |> AttributeMap.ofList 
            yield Incremental.Svg.svg 
                    (
                      attributes
                    )   
                    (GeologicalLog.svgView log semApp isSelected viewType)
        } 
      
   
      
      let menu = 
        div [clazz "ui horizontal menu";
             style "float:right; vertical-align: top"][
          div [clazz "item"]
              [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView LogNodeView.StackedViewSimpleBoxes)] 
                      [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Stacked view with simple boxes"];
          div [clazz "item"]
              [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView LogNodeView.StackedView2ColorBoxes)] 
                      [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Stacked view with fancy boxes"];
          div [clazz "item"]
              [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView LogNodeView.LineView)] 
                      [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Line view"];
        ]

      div [style "width:100%; height: 100%"] [
              menu
              Incremental.Svg.svg atts svgList
             ]


    let view (model : MCorrelationPlotApp) (semApp : MSemanticApp) =
      let menu =
        let icon =
          alist {
            let! ic =
              (model.creatingNew |> Mod.map (fun n -> 
                                              match n with
                                                | true  -> i [clazz "small yellow plus icon"] [] 
                                                | false -> i [clazz "small plus icon"] []
                                            ))
            yield ic
          }
        div [clazz "ui horizontal inverted menu";
             style "float:top"]
            [
              div [clazz "item"]
                  [Incremental.button (AttributeMap.ofList [clazz "ui small icon button"; onMouseClick (fun _ -> NewLog)]) 
                                       icon
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> FinishLog)] 
                          [i [clazz "small check icon"] [] ] |> UtilitiesGUI.wrapToolTip "done"
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteLog)] 
                          [i [clazz "small minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"
                  ]; 
            ]

      let domList =
         alist {            
            for log in model.logs do
              let! sel = model.selectedLog
              let isSelected = 
                match sel with
                  | Some s  -> s = log.id
                  | None    -> false
              
              yield
                        div [clazz "item"][
                          div [clazz "content"] [
                            div [clazz "header"; style "text-align: center"; onMouseClick (fun _ -> ToggleSelectLog (Some log.id))] [
                              i [clazz "yellow arrow alternate circle down icon"] [] |> UtilitiesGUI.wrapToolTip "select"
                            ]
                            div [] [GeologicalLog.view log semApp isSelected]
                          ]
                        ]
                    

//              yield (Incremental.tr 
//                      (AttributeMap.ofList [st; onClick (fun str -> ToggleSelectLog (Some log.id))])
//                      (GeologicalLog.view log semApp isSelected)
//                    )
          }     

      let myCss = [
          { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
          { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]

      require (myCss) (
        body [] [
          div [] [
            menu
            div [clazz "ui inverted segment"]
                [Incremental.div (AttributeMap.ofList [clazz "ui inverted divided list"])
                                 domList
                ]           
          ]
        ]
      )

//      require (myCss) (
//        body [] [
//          div [] [
//            menu
//            table
//              ([clazz "ui celled striped inverted table unstackable";
//                                    style "padding: 1px 5px 1px 5px"]) (
//                  [thead [][
//                    tr[][th[][text "Name"];
//                                  //th[][text "Weight"];
//                                  //th[][text "Colour"];
//                                  //th[][text "Level"];
//  //                               th[][text "Default Geometry Type"];
//                                  //th[][text "Semantic Type"]
//                    ]
//                  ];
//                  Incremental.tbody  (AttributeMap.ofList []) domList]           
//              )
//          ]
//        ]
//      )

    let getLogConnectionSgs 
          (model : MCorrelationPlotApp)
          (semanticApp : MSemanticApp) 
          (camera : MCameraControllerState) =


      adaptive {
        let! logIdOpt = model.selectedLog
        return match logIdOpt with
                | None      -> Sg.empty
                | Some logId  ->
                  let sgs = 
                    model.logs
                      |> AList.map (fun (x : MGeologicalLog) -> 
                                      (GeologicalLog.getLogConnectionSg x semanticApp (x.id = logId) camera |> Sg.noEvents))
                      |> ASet.ofAList
                      |> Sg.set
                  sgs
      }
      |> Sg.dynamic



        
        
