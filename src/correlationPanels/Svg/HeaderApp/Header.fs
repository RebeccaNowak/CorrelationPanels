namespace Svgplus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Header =
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus
  open Svgplus.Base
  open SimpleTypes


  type Action =
    | OnLeftClick
    | OnMouseEnter
    | OnMouseLeave
    | ToggleDraw
