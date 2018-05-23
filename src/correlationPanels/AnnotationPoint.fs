namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationPoint =
  open System
  open Aardvark.Base

  let initial : AnnotationPoint = {
    point = V3d.OOO
    selected = false
  }
