namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Aardvark.Application
  open UIPlus.KeyboardTypes

  module Keyboard =
    type Action =
      | KeyDown of key : Keys
      | KeyUp   of key : Keys     

    let init () : Keyboard<'a> =
      {
        altPressed        = false
        ctrlPressed       = false
        registeredActions = PList.empty
      }

    let register (k : KeyConfig<'a>) 
                 (f : 'a -> 'a)
                 (model : Keyboard<'a>) =
      let _reg = model.registeredActions.Prepend(k)
      {model with registeredActions = _reg}

    let update (model : Keyboard<'a>) 
               (app   : 'a)
               (action : Action) =
       let _model = 
         match action with
           | KeyDown Keys.LeftAlt
           | KeyDown Keys.RightAlt ->
            {model with altPressed = true}
           | KeyUp Keys.LeftAlt
           | KeyUp Keys.RightAlt ->
            {model with altPressed = false}
           | KeyDown Keys.LeftCtrl
           | KeyDown Keys.RightCtrl ->
            {model with ctrlPressed = true}
           | KeyUp Keys.LeftCtrl
           | KeyUp Keys.RightCtrl ->
            {model with ctrlPressed = false}
           | KeyUp _ -> model
           | KeyDown _ -> model
        
       let _app = 
         match action with
          | KeyUp _ -> app
          | KeyDown k ->
            Log.line "Key Pressed" 
            let _filtered = PList.filter (fun (c : KeyConfig<'a>) -> 
                                            c.check
                                                _model.ctrlPressed
                                                _model.altPressed
                                                k
                                         )
                                          _model.registeredActions
            match _filtered.IsEmpty () with
             | true  -> app
             | false ->
               let config = _filtered.Item 0 //TODO: only taking first, could execute list
               config.update app
       (_model, _app)

           


      