﻿namespace UIPlus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ArrowButtonApp =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI

    type Action =
      | OnClick of ArrowButtonId          


    let init id = 
      {
        id        = id
        direction = Direction.Left
        size      = Size.Normal
      }

    let update (model : ArrowButton) (action : Action) =
      match action with
        | OnClick id -> model

    let view (model : MArrowButton) = 
      let content = 
        alist {
          let! size = model.size
          let! dir  = model.direction

          let iconString =
            sprintf "%s arrow %s icon" size.toString dir.toString
          yield (i [clazz iconString] [])
        }

      let attr =
        amap {
          let! size = model.size
          let buttonString =
            sprintf "%s ui icon button" size.toString
          yield (clazz buttonString)
          yield onClick (fun _ -> (OnClick model.id))
        } |> AttributeMap.ofAMap


      Incremental.button attr content
             

    