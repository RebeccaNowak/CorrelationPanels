﻿namespace UIPlus.KeyboardTypes

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Aardvark.Application


  type KeyConfig<'a> =
    {
      update : 'a -> 'a
      key  : Keys
      ctrl : bool
      alt  : bool
    } with
      member this.check (ctrl : bool) (alt : bool) (key : Keys) =
        let c = (ctrl = this.ctrl)
        let a = (alt = this.alt)
        let k = (key = this.key)
        let res = c && a && k
        res
      



  type Keyboard<'a> =
    {
      altPressed        : bool
      ctrlPressed       : bool
      registeredActions : plist<KeyConfig<'a>>
    } 