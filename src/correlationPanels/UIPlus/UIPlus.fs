namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental


  type Direction =
  | Up
  | Down
  | Left
  | Right
  with
    member this.toString =
      match this with
        | Up    -> "up"
        | Down  -> "down"
        | Left  -> "left"
        | Right -> "right"

  type Size =
    | Mini
    | Tiny
    | Small
    | Normal
    | Large
    | Big
    | Huge
    | Massive
    with
      member this.toString =
        match this with
          | Mini      -> "mini"
          | Tiny      -> "tiny"
          | Small     -> "small"
          | Normal    -> ""
          | Large     -> "large"
          | Big       -> "big"
          | Huge      -> "huge"
          | Massive   -> "massive"


  type ArrowButtonId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module ArrowButtonId = 
    let invalid = {id = ""}
    let newId () : ArrowButtonId  = 
      let id = System.Guid.NewGuid ()
      {id = id.ToString () }

  [<DomainType>]
  type ArrowButton = {
    [<NonIncremental>]
    id            : ArrowButtonId

    direction     : Direction
    size          : Size
  }

