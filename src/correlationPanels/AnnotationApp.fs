namespace CorrelationDrawing

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.SceneGraph
  open Aardvark.UI
  open UtilitiesGUI


  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module AnnotationApp =
  
    type Action =
      | Clear
      | AnnotationMessage       of Annotation.Action
      | AddAnnotation           of Annotation
      | DeselectAllPoints       
      | SelectPoints            of List<(V3d * string)>


    let initial : AnnotationApp = {
      annotations         = plist.Empty
      selectedAnnotation  = None
    }
 
    let binarySerializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
    
    let findAnnotation (app : AnnotationApp) (id : string) =
      app.annotations.FirstOrDefault((fun x -> x.id = id), Annotation.initialDummy)

    let getSelected (model : AnnotationApp) =
      model.annotations
        |> PList.map (fun a -> Annotation.getSelected a)
        |> PList.filterNone
 
    let update (model       : AnnotationApp)
               (action      : Action) =
      match action with
        | Clear                -> {model with annotations        = PList.empty
                                              selectedAnnotation = None
                                  }
        | AnnotationMessage m  -> 
            {model with annotations = model.annotations 
                                        |> PList.map (fun a -> Annotation.update m a)}
        | AddAnnotation a      ->      
            {model with annotations = model.annotations.Append a}
        | DeselectAllPoints _  -> 
          {model with annotations = 
                        model.annotations 
                          |> PList.map
                              (fun a -> Annotation.update (Annotation.Action.Deselect) a)
          }    
        | SelectPoints lst  ->
          let deselected = model.annotations 
                            |> PList.map
                                (fun a -> Annotation.update (Annotation.Action.Deselect) a)
          let annoInList (a : Annotation) = 
            lst 
              |> List.map snd
              |> List.contains a.id
              
          let updateFunction (anno : Annotation) =
              match annoInList anno with
                  | true  -> 
                    let ind = lst.FirstIndexOf (fun (p, a) -> a = anno.id)
                    let (pt, a) = lst.Item ind
                    Annotation.update (Annotation.Action.Select pt) anno
                  | false -> anno
          let updated = deselected |> PList.map updateFunction
                  
          {model with annotations = updated}

    let save (model : AnnotationApp) =
      let arr = binarySerializer.Pickle model.annotations
      //let info = System.IO.Directory.CreateDirectory "./saved"
      //let success = info.Exists
      System.IO.File.WriteAllBytes("./annotations.save", arr);
      printf "write file" 
      model

    let load (model : AnnotationApp) =
      let bytes = System.IO.File.ReadAllBytes("./annotations.save");
      let annos = binarySerializer.UnPickle(bytes)
      printf "load file" 
      {model with annotations = annos}
    
 
    let view (model : MAnnotationApp)  (semanticApp : MSemanticApp)  =
      let domList = 
        alist {
          for a in model.annotations do
            let! annoView = (Annotation.View.view a semanticApp)
            yield (tr 
              ([style tinyPadding])
              (List.map (fun x -> x |> UI.map AnnotationMessage) annoView)
            )
        }  
      div [] [
        Html.SemUi.accordion "Annotations" "File Outline" true [
          table 
            ([clazz "ui celled striped inverted table unstackable"; 
              style "padding: 1px 5px 1px 5px"]) 
            [thead [][tr[][th[][text "Semantic"];
                                  th[][text "Geometry"];
                                  th[][text "Projection"];
                                  th[][text "Text"]
                      ]];
            Incremental.tbody (AttributeMap.ofList []) domList]    
        ]
      ]

    module Sg =
      let view (model         : MAnnotationApp) 
               (semApp        : MSemanticApp) 
               (cam           : IMod<CameraView>) =    
        
        let annoSet = ASet.ofAList model.annotations
               
        aset {
          for a in annoSet do
                yield! ((Annotation.Sg.view a cam semApp) 
                           |> ASet.map (fun x -> x |> Sg.map AnnotationMessage))
        } |> Sg.set           