namespace CorrelationDrawing

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
//open Aardvark.Base.Vec
//open Aardvark.Base.Incremental
//open Aardvark.SceneGraph
//open Aardvark.UI
//open Aardvark.UI.Primitives


  module Shaders =
      open FShade
      //open FShade.InstrinsicAttributes
      //open FShade.UniformExtensions
      //open Aardvark.SceneGraph.Semantics
      //open Aardvark.Application

      //type Vertex = {
      //      [<Position>]                pos     : V4d
      //      [<WorldPosition>]           wp      : V4d
      //}

      let screenSpaceScale (v : Vertex) =
            vertex {        
              let loc    = uniform.CameraLocation       
              let hvp    = float uniform.ViewportSize.X

              let p    : V3d   = v.wp.XYZ
              let size : float = uniform?Size

              let dist = (p - loc).Length      
              let scale = dist * size / hvp 

              return { 
                v with
                  pos = V4d(v.pos.X * scale, v.pos.Y * scale, v.pos.Z * scale, v.pos.W)
              }
            }

