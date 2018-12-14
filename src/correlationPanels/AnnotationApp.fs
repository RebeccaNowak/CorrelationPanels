namespace CorrelationDrawing

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.SceneGraph
  open Aardvark.UI
  open UIPlus


  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module AnnotationApp =
  
    type Action =
      | Clear
      | AnnotationMessage       of Annotation.Action
      | AddAnnotation           of Annotation
      | DeselectAllPoints       
      | SelectPoints            of hmap<AnnotationId, V3d>


    let initial : AnnotationApp = {
      annotations         = hmap.Empty
      selectedAnnotation  = None
    }
 
    let binarySerializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
    
    let findAnnotation (app : AnnotationApp) (id : AnnotationId) =
      HMap.tryFind id app.annotations
      

    let annotationOrDefault (app : AnnotationApp) (id : AnnotationId) =
      let a = HMap.tryFind id app.annotations
      match a with 
        | Some a -> a
        | None   -> Annotation.initialDummy

    let findAnnotation' (app : MAnnotationApp) (id : AnnotationId) =
      AMap.tryFind id app.annotations

    let findAnnotation'' (app : MAnnotationApp) (optId : Option<AnnotationId>) =
      optId |> Option.map (fun id -> AMap.tryFind id app.annotations)
            |> Option.flattenModOpt
      

    let tryElevation (app : AnnotationApp) (id : AnnotationId) =
      let anno = (findAnnotation app id) 
      let el =
        match anno with
              | Some a -> Some (Annotation.elevation a)
              | None   ->
                printf "could not find annotation id\n" //TODO proper error handling
                None
      el
    
    let elevation' (app : MAnnotationApp) (id : IMod<AnnotationId>) =
      adaptive {
        let! anno = Mod.bind (fun id -> (findAnnotation' app id)) id
        let! el =
          match anno with
                | Some a -> Annotation.elevation' a
                | None   ->
                  printf "could not find annotation id" //TODO proper error handling
                  Mod.constant 0.0
        return el
      }

    let isElevationBetween (annoApp : AnnotationApp) (id : AnnotationId) (lower : V3d) (upper : V3d) =
      let a = findAnnotation annoApp id
      match a with
        | Some a ->
            Annotation.isElevationBetween lower upper a
        | None   -> false

    let getLevel' (annoApp : AnnotationApp) (semApp : SemanticApp) (id : AnnotationId) =
      let anno = annoApp.annotations.Item id
      Annotation.getLevel semApp anno 


    let getSelectedPoints (model : AnnotationApp) =
      model.annotations
        |> HMap.map (fun k a -> Annotation.getSelected a)
        |> HMap.filter (fun k opt ->
                          match opt with 
                            | None -> false
                            | Some p -> true
                       )

    let getSelectedPoints' (model : AnnotationApp) =
        model.annotations
          |> HMap.map (fun k a -> Annotation.getSelected a)
          |> HMap.filterNone
          |> HMap.map (fun k (p,a) -> p.point)

 
    let getLevel  (id           : AnnotationId) //orInvalid
                  (annoApp      : AnnotationApp) 
                  (semanticApp  : SemanticApp) =
      let anno = findAnnotation annoApp id
      let semantic = 
        anno |> Option.bind (fun a ->
          (SemanticApp.getSemantic semanticApp a.semanticId))
      match semantic with
        | Some s -> s.level
        | None   -> NodeLevel.INVALID

    let getColourIcon' (annoId      : AnnotationId) 
                       (semanticApp : MSemanticApp)
                       (annoApp     : MAnnotationApp) =
      let anno = findAnnotation' annoApp annoId
      let iconAttr =
        amap {
          yield clazz "circle icon"
          let! anno = anno
          let! c = Annotation.getColor anno semanticApp
          yield style (sprintf "color:%s" (GUI.CSS.colorToHexStr c))
        }      
      Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList [])


    let update (model       : AnnotationApp)
               (action      : Action) =
      match action with
        | Clear                -> {model with annotations        = HMap.empty
                                              selectedAnnotation = None
                                  }
        | AnnotationMessage m  -> 
            {model with annotations = model.annotations 
                                        |> HMap.map (fun k a -> Annotation.update m a)}
        | AddAnnotation a      ->      
            {model with annotations = model.annotations.Add (a.id,a)}
        | DeselectAllPoints _  -> 
          {model with annotations = 
                        model.annotations 
                          |> HMap.map
                              (fun k a -> Annotation.update (Annotation.Action.Deselect) a)
          }    
        | SelectPoints lst  ->
          let deselected = model.annotations 
                            |> HMap.map
                                (fun k a -> Annotation.update (Annotation.Action.Deselect) a)
          let updateFunction (k : AnnotationId) =
            match (lst.TryFind k) with
              | Some v -> Annotation.update (Annotation.Action.Select v)
              | None   -> (fun a -> a)
            
          let updated = deselected |> HMap.map updateFunction
          {model with annotations = updated}

    let save (model : AnnotationApp) (savename : string) =
      let arr = binarySerializer.Pickle model.annotations
      //let info = System.IO.Directory.CreateDirectory "./saved"
      //let success = info.Exists
      System.IO.File.WriteAllBytes
        (
          sprintf "%s%s" "./" savename, arr
        )
      printf "write file" 
      model

    let load (model : AnnotationApp) (savename : string) =
      let bytes = 
        System.IO.File.ReadAllBytes
          (
            sprintf "%s%s" "./" savename 
          )
      let annos = binarySerializer.UnPickle(bytes)
      printf "load file" 
      {model with annotations = annos}
    
 
    let view (model : MAnnotationApp)  (semanticApp : MSemanticApp)  =
      let annos = AMap.valuesToAList model.annotations
      let domList = 
        alist {
          for a in annos do
            let! annoView = (Annotation.View.view a semanticApp
            
            )
            yield (tr 
              ([style GUI.CSS.tinyPadding])
              (List.map (fun x -> x |> UI.map AnnotationMessage) annoView)
            )
        }  
      div [] [
        Html.SemUi.accordion "Annotations" "File Outline" true [
          table 
            ([clazz "ui celled striped inverted table unstackable"; 
              style "padding: 1px 5px 1px 5px"]) 
            [thead [][tr[][
                           th[][text "Semantic"]
                           th[][text "Geometry"]
                           th[][text "Projection"]
                           th[][text "Text"]
                          ]
                     ];
                     Incremental.tbody (AttributeMap.ofList []) domList]    
            ]
      ]

    module Sg =
      let view (model         : MAnnotationApp) 
               (semApp        : MSemanticApp) 
               (cam           : IMod<CameraView>) =    
        
        let annoSet = AMap.valuesToASet model.annotations
               
        aset {
          for a in annoSet do
                yield! ((Annotation.Sg.view a cam semApp) 
                           |> ASet.map (fun x -> x |> Sg.map AnnotationMessage))
        } |> Sg.set        
        
