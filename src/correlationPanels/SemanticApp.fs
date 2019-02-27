namespace CorrelationDrawing

open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open UIPlus

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

  let getSelectedSemantic (app : SemanticApp) =
    HMap.find app.selectedSemantic app.semantics

  let getSemantic (app : SemanticApp) (semanticId : SemanticId) =
    HMap.tryFind semanticId app.semantics

  let getSemanticOrDefault  (app : SemanticApp) (semanticId : SemanticId) =
    HMap.tryFind semanticId app.semantics
      |> Option.defaultValue Semantic.initInvalid

  let getSemantic' (app : MSemanticApp) (semanticId : SemanticId) =
    AMap.tryFind semanticId app.semantics


  //let getSemanticOrDefault'  (app : MSemanticApp) (semanticId : SemanticId) =
  //  AMap.tryFind semanticId app.semantics

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
   
  let getMetricSemantics (model : SemanticApp) =
    model.semanticsList |> PList.filter (fun s -> s.semanticType = SemanticType.Metric)
  
  let getMetricId (model : SemanticApp) =
    model |> getMetricSemantics
          |> PList.tryAt 0
          |> Option.map (fun x -> x.id)


  ///// convienience functions II

  let next (e : SemanticsSortingOption) = 
    let plusOneMod (x : int) (m : int) = (x + 1) % m
    let eInt = int e
    enum<SemanticsSortingOption>(plusOneMod eInt 6) // hardcoded :(

  let setState (state : State) (s : option<Semantic>)  = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState state)) s)

  let enableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState State.Edit)) s)

  let disableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x (Semantic.SetState State.Display)) s)

  let disableSemantic' (s : Semantic) =
    Semantic.update s (Semantic.SetState State.Display) 


  let sortFunction (sortBy : SemanticsSortingOption) = 
    match sortBy with
      | SemanticsSortingOption.Label        -> fun (x : Semantic) -> x.label.text
      | SemanticsSortingOption.Level        -> fun (x : Semantic) -> (sprintf "%03i" x.level.level)
//      | SemanticsSortingOption.GeometryType -> fun (x : Semantic) -> x.geometry.ToString()
      | SemanticsSortingOption.SemanticType -> fun (x : Semantic) -> x.semanticType.ToString()
      | SemanticsSortingOption.SemanticId   -> fun (x : Semantic) -> x.id.id // TODO make more elegant
      | SemanticsSortingOption.Timestamp    -> fun (x : Semantic) -> x.timestamp
      | _                                   -> fun (x : Semantic) -> x.timestamp

  let getSortedList (list    : hmap<SemanticId, Semantic>) 
                    (sortBy  : SemanticsSortingOption) =
    DS.HMap.toSortedPlist list (sortFunction sortBy)

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

  let insertSemantic (s : Semantic) (state : State) (model : SemanticApp) = 
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
    insertSemantic newSemantic State.New model

  let getInitialWithSamples =
      initial
        |> insertSemantic (Semantic.initialHorizon0   ("Horizon0")) State.Display
        |> insertSemantic (Semantic.initialHorizon1   ("Horizon1")) State.Display
        |> insertSemantic (Semantic.initialHorizon2   ("Horizon2")) State.Display
        |> insertSemantic (Semantic.initialHorizon3   ("Horizon3")) State.Display
        |> insertSemantic (Semantic.initialHorizon4   ("Horizon4")) State.Display
        |> insertSemantic (Semantic.initialCrossbed   ("Crossbed")) State.Display
        //|> insertSemantic (Semantic.initialGrainSize  (System.Guid.NewGuid().ToString())) State.Display
        |> insertSemantic (Semantic.impactBreccia     ("Impact")) State.Display
        |> insertSemantic (Semantic.initialGrainSize2 ("GrainSize")) State.Edit

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

  let save (model : SemanticApp) (savename : string) =
    let arr = binarySerializer.Pickle model.semantics
    
    System.IO.File.WriteAllBytes
      (
        sprintf "%s%s" "./" savename, arr
      )
    Log.line "write file: %s" savename
    model

  let load (model : SemanticApp) (savename : string) =
    let bytes = 
      System.IO.File.ReadAllBytes
        (
          sprintf "%s%s" "./" savename
        ) //"./semantics.save");

    let semantics = 
      try 
        Some (binarySerializer.UnPickle(bytes)) //TODO catch unpickle exceptions
      with 
        | :? MBrace.FsPickler.FsPicklerException -> None
    match semantics with
      | Some semantics ->
        Log.line "load file" 
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
      | None -> 
        Log.line "could not load semantics"
        model
                    
  ///////////////////////////////// VIEW ///////////////////
  module View = 
    let simpleView (model : MSemanticApp) =
      let domList = 
        alist {                 
          for mSem in model.semanticsList do
            let! state = mSem.state
           // match state with 
            let! col = mSem.style.color.c
            let textCol = Table.textColorFromBackground col
            let st = 
              match state with
                | State.Display -> []
                | State.Edit | State.New ->
                  [style (sprintf "background: %s;%s" (GUI.CSS.colorToHexStr col) textCol)]
             
            let domNodes = Semantic.View.miniView mSem
            let domNodes = domNodes |> List.map (UI.map SemanticMessage)


            yield tr 
                     (st@[onClick (fun str -> SetSemantic (Some mSem.id))])
                      domNodes
        } 
        
      Table.toTableView (div[][]) domList ["Annotation Type"]

    let expertGUI (model : MSemanticApp) = 
      let menu = 
        div [clazz "ui horizontal inverted menu";
             style "width:100%; height: 10%; float:middle; vertical-align: middle"][
          div [clazz "item"]
              [button [clazz "ui small icon button"; onMouseClick (fun _ -> AddSemantic)] 
                      [i [clazz "small plus icon"] [] ] |> UIPlus.ToolTips.wrapToolTip "add"];
          div [clazz "item"]
              [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteSemantic)] 
                      [i [clazz "small minus icon"] [] ] |> UIPlus.ToolTips.wrapToolTip "delete"];
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
            if state = State.New then 
              let! domNode = Semantic.View.view mSem
              let menu = Menus.saveCancelMenu SaveNew CancelNew

              yield Table.intoActiveTr 
                      (SetSemantic (Some mSem.id))
                      (domNode |> List.map (fun x -> x |> UI.map SemanticMessage)) 
                     
              yield Table.intoTr [(Table.intoTd' menu domNode.Length)]   
                  
            else if state = State.Edit then
              let! domNode = Semantic.View.view mSem    
              let! col = mSem.style.color.c
              let textCol = Table.textColorFromBackground col
              let st = style (sprintf "background: %s;%s" (GUI.CSS.colorToHexStr col) textCol)
              yield tr 
                      ([st; onClick (fun str -> SetSemantic (Some mSem.id))]) 
                      (List.map (fun x -> x |> UI.map SemanticMessage) domNode)
            else 
              let! domNode = Semantic.View.view mSem           
              yield tr 
                      ([onClick (fun str -> SetSemantic (Some mSem.id))]) 
                      (List.map (fun x -> x |> UI.map SemanticMessage) domNode)
        } 

      Table.toTableView menu domList ["Label";"Weight";"Colour";"Level";"Semantic Type";"Geometry"]


  let threads (model : SemanticApp) = 
    ThreadPool.empty

  let app : App<SemanticApp, MSemanticApp, Action> =
      {
          unpersist = Unpersist.instance
          threads   = threads
          initial   = getInitialWithSamples
          update    = update
          view      = View.expertGUI
      }

  let start () = App.start app

