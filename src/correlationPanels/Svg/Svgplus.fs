﻿namespace Svgplus

open Aardvark.Base
open Aardvark.Base.Incremental
open SimpleTypes
open Aardvark.UI

  type FontSize = {
    fontSize : int 
  }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module FontSize =
    let defaultSize = {fontSize = 12}
    let min = 10
    let max = 30

    let init d : FontSize = 
      let s =
        match d with
          | a when a <= min -> min
          | b when b >= max -> max
          | _ -> d
      {fontSize = s}

    let add (s : FontSize) (d : int) : FontSize =
      init (s.fontSize + d)

  type MouseAction =
    | OnLeftClick      
    | OnRightClick      
    | OnMouseDown       of ((Aardvark.Application.MouseButtons))
    | OnMouseUp         of ((Aardvark.Application.MouseButtons))
    | OnMouseEnter
    | OnMouseLeave    

  module MouseActions = 
    let init () =
      let rightOrUp b = 
        match b with 
            | Aardvark.Application.MouseButtons.Right -> OnRightClick
            | _ -> OnMouseUp b

      let _enter = Aardvark.UI.Events.onMouseEnter (fun _ -> OnMouseEnter)
      let _exit  = Aardvark.UI.Events.onMouseLeave (fun _ -> OnMouseLeave)
      let _down  = Aardvark.UI.Events.onMouseDown  (fun b v -> OnMouseDown b)
      let _up    = Aardvark.UI.Events.onMouseUp    (fun b v -> rightOrUp b)
      let _left  = 
        amap {
          yield! (Aardvark.UI.Svg.Events.onClickAttributes [(fun _ -> (OnLeftClick))])
        }      
      let actions = [_enter;_exit;_down;_up] |> AMap.ofList
      actions
        |> AMap.union _left

    //let init left right down up enter leave =
    //  let rightOrUp b = 
    //    match b with 
    //        | Aardvark.Application.MouseButtons.Right -> right
    //        | _ -> up b

    //  let _enter = Aardvark.UI.Events.onMouseEnter (fun _ -> enter)
    //  let _exit  = Aardvark.UI.Events.onMouseLeave (fun _ -> leave)
    //  let _down  = Aardvark.UI.Events.onMouseDown  (fun b v -> down b)
    //  let _up    = Aardvark.UI.Events.onMouseUp    (fun b v -> rightOrUp b)
    //  let _left  = 
    //    amap {
    //      yield! (Aardvark.UI.Svg.Events.onClickAttributes [(fun _ -> (left))])
    //    }      
    //  let actions = [_enter;_exit;_down;_up] |> AMap.ofList
    //  actions
    //    |> AMap.union _left

  type Bin = {
    number : int
    value  : int
    colour : C4b
  }


  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Bin =
    let init number value colour =
      {
        number = number
        value  = value
        colour = colour
      }

  type Bin16x6 = {
    angularBin : int
    radius     : float
    colour     : C4b
  }



  type ButtonId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module ButtonId = 
    let invalid = {id = ""}
    let newId () : ButtonId  = 
      {id = System.Guid.NewGuid().ToString ()}

  type ConnectionStatus =
    | NoConnection
    | InProgress
    | Connected


  [<DomainType>]
  type Button = {
    [<NonIncremental>]
    id              : ButtonId

    pos             : V2d
    radius          : float
    rHoverChange    : float
    stroke          : float
    color           : C4b
    colChange       : V3i
    fill            : bool
    isToggled       : bool
    isHovering      : bool
    transitionSec   : float
  }








