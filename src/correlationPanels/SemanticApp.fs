namespace CorrelationDrawing

open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open UtilitiesGUI

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SemanticApp = 

  let binarySerializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

  type Action =
    | SetSemantic       of option<SemanticId>
    | AddSemantic
    | CancelNew
    | SaveNew
    | DeleteSemantic
    | SemanticMessage   of Semantic.Action
    | SortBy            


    ///// INITIAL
  let initial : SemanticApp = {
    semantics         = hmap.Empty
    selectedSemantic  = SemanticId.invalid
    semanticsList     = plist.Empty
    sortBy            = SemanticsSortingOption.Level
    creatingNew       = false
  }

  ///// convenience functions Semantics

  let getSemantic (app : SemanticApp) (semanticId : SemanticId) =
    HMap.tryFind semanticId app.semantics

  let getColor (model : MSemanticApp) (semanticId : IMod<SemanticId>) =
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.color.c
                  | None -> Mod.constant C4b.Red) sem


  let getThickness (model : MSemanticApp) (semanticId : IMod<SemanticId>) =
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.thickness.value
                  | None -> Mod.constant 1.0) sem

  let getLabel (model : MSemanticApp) (semanticId : IMod<SemanticId>) = 
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    sem
        |> Mod.bind (fun x ->
                          match x with 
                            | Some s -> s.label.text
                            | None -> Mod.constant "-NONE-")

 

  ///// convienience functions II

  let next (e : SemanticsSortingOption) = 
    let plusOneMod (x : int) (m : int) = (x + 1) % m
    let eInt = int e
    enum<SemanticsSortingOption>(plusOneMod eInt 6) // hardcoded :(

  let setState (state : SemanticState) (s : option<Semantic>)  = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState state)) s)

  let enableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState SemanticState.Edit)) s)

  let disableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState SemanticState.Display)) s)

  let disableSemantic' (s : Semantic) =
    Semantic.update s (Semantic.SetState SemanticState.Display) 


  let sortFunction (sortBy : SemanticsSortingOption) = 
    match sortBy with
      | SemanticsSortingOption.Label        -> fun (x : Semantic) -> x.label.text
      | SemanticsSortingOption.Level        -> fun (x : Semantic) -> (sprintf "%03i" x.level)
//      | SemanticsSortingOption.GeometryType -> fun (x : Semantic) -> x.geometry.ToString()
      | SemanticsSortingOption.SemanticType -> fun (x : Semantic) -> x.semanticType.ToString()
      | SemanticsSortingOption.SemanticId   -> fun (x : Semantic) -> x.id.id // TODO make more elegant
      | SemanticsSortingOption.Timestamp    -> fun (x : Semantic) -> x.timestamp
      | _                                   -> fun (x : Semantic) -> x.timestamp

  let getSortedList (list    : hmap<SemanticId, Semantic>) 
                    (sortBy  : SemanticsSortingOption) =
    UtilitiesDatastructures.sortedPlistFromHmap list (sortFunction sortBy)

  let deleteSemantic (model : SemanticApp)=
      let getAKey (m : hmap<SemanticId, 'a>) =
        m |> HMap.toSeq |> Seq.map fst |> Seq.tryHead

      let rem =
        model.semantics
          |> HMap.remove model.selectedSemantic

      match getAKey rem with
        | Some k  -> 
          let updatedSemantics = (rem |> HMap.alter k enableSemantic)
          {model with 
            semantics = updatedSemantics 
            semanticsList = getSortedList updatedSemantics model.sortBy
            selectedSemantic = k
          }
        | None   -> model

  let insertSemantic (s : Semantic) (state : SemanticState) (model : SemanticApp) = 
    let newSemantics = (model.semantics.Add(s.id, s)
        |> HMap.alter model.selectedSemantic disableSemantic
        |> HMap.alter s.id (setState state))

    {model with selectedSemantic  = s.id
                semantics         = newSemantics
                semanticsList     = getSortedList newSemantics model.sortBy
    }


  let insertSampleSemantic (model : SemanticApp) = 
    let id = System.Guid.NewGuid().ToString()
    let newSemantic = Semantic.Lens._labelText.Set(
                        (Semantic.initial id),"NewSemantic")
    insertSemantic newSemantic SemanticState.New model

  let getInitialWithSamples =
      initial
        |> insertSemantic (Semantic.initialHorizon0 (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialHorizon1 (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialHorizon2 (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialHorizon3 (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialHorizon4 (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialCrossbed (System.Guid.NewGuid().ToString())) SemanticState.Display
        |> insertSemantic (Semantic.initialGrainSize (System.Guid.NewGuid().ToString())) SemanticState.Edit

  let deselectAllSemantics (semantics : hmap<SemanticId, Semantic>) =
    semantics |> HMap.map (fun k s -> disableSemantic' s)

    

  ////// UPDATE 
  let update (model : SemanticApp) (action : Action) =
    match (action, model.creatingNew) with 
      | SetSemantic sem, false ->
        match sem with
          | Some s  ->
              let updatedSemantics = 
                model.semantics
                  |> HMap.alter model.selectedSemantic disableSemantic
                  |> HMap.alter s enableSemantic
                      
              {model with selectedSemantic  = s
                          semanticsList     = getSortedList updatedSemantics model.sortBy 
                          semantics         = updatedSemantics}
          | None    -> model

      | SemanticMessage sem, _   ->
        let fUpdate (semO : Option<Semantic>) = 
            match semO with
                | Some s  -> Some(Semantic.update s sem)
                | None    -> None
        let updatedSemantics = HMap.alter model.selectedSemantic fUpdate model.semantics
        {model with semantics     = updatedSemantics
                    semanticsList = getSortedList updatedSemantics model.sortBy}

      | AddSemantic, false     -> 
          {insertSampleSemantic model with creatingNew = true}
          
      | DeleteSemantic, false  -> deleteSemantic model
      | SortBy, false          ->
        let newSort = next model.sortBy
        {model with sortBy = newSort
                    semanticsList = 
                      model.semanticsList
                        |> PList.toSeq
                        |> Seq.sortBy (sortFunction newSort)
                        |> PList.ofSeq
        }


      | SaveNew, true   -> 
        let updatedSemantics = 
              model.semantics
                |> HMap.alter model.selectedSemantic enableSemantic
        {
          model with creatingNew   = false
                     semanticsList = getSortedList updatedSemantics model.sortBy 
                     semantics     = updatedSemantics
         }
      | CancelNew, true -> 
        {
          deleteSemantic model with creatingNew = false
        }
      | _ -> model

  let save (model : SemanticApp) =
    let arr = binarySerializer.Pickle model.semantics
    System.IO.File.WriteAllBytes("./semantics.save", arr);
    printf "write file" 
    model

  let load (model : SemanticApp) =
    let bytes = System.IO.File.ReadAllBytes("./semantics.save");
    let semantics = binarySerializer.UnPickle(bytes) //TODO catch unpickle exceptions
    printf "load file" 
    let newModel =
      match HMap.isEmpty semantics with
        | true  -> getInitialWithSamples
        | _     ->
          let deselected = deselectAllSemantics semantics
          let upd =
            {model with semantics        = deselected
                        semanticsList    = getSortedList deselected model.sortBy
            }
          update upd (Action.SetSemantic ((upd.semanticsList.TryGet 0) |> Option.map (fun s -> s.id)))
    newModel
                    
  ///// VIEW //TODO modules

  let viewSemantics (model : MSemanticApp) = 
    let menu = 
      div [clazz "ui horizontal inverted menu";
           style "width:100%; height: 10%; float:middle; vertical-align: middle"][
        div [clazz "item"]
            [button [clazz "ui small icon button"; onMouseClick (fun _ -> AddSemantic)] 
                    [i [clazz "small plus icon"] [] ] |> UtilitiesGUI.wrapToolTip "add"];
        div [clazz "item"]
            [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteSemantic)] 
                    [i [clazz "small minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"];
        div [clazz "item"] [
          button 
            [clazz "ui small icon button"; style "width: 20ch; text-align: left"; onMouseClick (fun _ -> SortBy;)]
            [Incremental.text (Mod.map (fun x -> sprintf "sort: %s" (string x)) model.sortBy)]
        ]  
      ]
    
    let domList = 
      alist {                 
        for mSem in model.semanticsList do
          let! state = mSem.state
          let! c = model.creatingNew
          if state = SemanticState.New then 
            let! domNode = Semantic.view mSem
            let menu =
              div[clazz "ui buttons"; style "vertical-align: top; horizontal-align: middle"]
                 [
                    button[clazz "compact ui button"; onMouseClick (fun _ -> SaveNew)][text "Save"];
                    div[clazz "or"][];
                    button[clazz "compact ui button"; onMouseClick (fun _ -> CancelNew)][text "Cancel"]
                 ]
              
            yield tr 
                    ([clazz "active";style UtilitiesGUI.tinyPadding; onClick (fun str -> SetSemantic (Some mSem.id))])
                    ((List.map (fun x -> x |> UI.map SemanticMessage) domNode))
                  
            yield tr 
                    [clazz "active"; style UtilitiesGUI.tinyPadding]
                    [td [clazz "center aligned"; style lrPadding;attribute "colspan" (sprintf "%i" domNode.Length)][menu]]
                  
          else
            
            let! domNode = Semantic.view mSem // DOESN'T WORK             
            //let domNode = [td [] [text "foobar"]|> UI.map Semantic.TextInputMessage] // WORKS
            //yield tr [] [text "foobar"] // WORKS
            yield tr 
                    ([style UtilitiesGUI.tinyPadding; onClick (fun str -> SetSemantic (Some mSem.id))]) 
                    (List.map (fun x -> x |> UI.map SemanticMessage) domNode)

      } 

    let myCss = [
              { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
              { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
              { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]

    require (myCss) (
      body [clazz "ui"; style "background: #1B1C1E;position:fixed;width:100%"] [
        div [] [
          menu
          table
            ([clazz "ui celled striped selectable inverted table unstackable";
                                  style "padding: 1px 5px 1px 5px"]) (
                [
                  thead [][tr[][th[][text "Label"];
                                th[][text "Weight"];
                                th[][text "Colour"];
                                th[][text "Level"];
//                               th[][text "Default Geometry Type"];
                                th[][text "Semantic Type"]]];

                  Incremental.tbody  (AttributeMap.ofList []) domList
                ]
            )
        ]
      ]
    )

  let app : App<SemanticApp, MSemanticApp, Action> =
      {
          unpersist = Unpersist.instance
          threads   = fun _ -> ThreadPool.empty
          initial   = getInitialWithSamples
          update    = update
          view      = viewSemantics
      }

  let start () = App.start app

