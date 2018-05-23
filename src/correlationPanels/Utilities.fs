namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

  module Seq =
    let properPairwiseOpt (f : 'a -> 'a -> 'b) (neutral : 'b) (s : seq<Option<'a>>) =
      s
        |> Seq.chunkBySize 2
        |> Seq.map 
          (fun arr -> match arr with
                        | [| a;b |] -> match a, b with
                                        | Some c, Some d -> Some (f c d)
                                        | _ -> None
                        | _ -> None)

    let properPairwise (f : 'a -> 'a -> 'b) (neutral : 'b) (s : seq<'a>) =
      s
        |> Seq.chunkBySize 2
        |> Seq.map 
          (fun arr -> match arr with
                        | [| a;b |] -> (f a b)
                        | _ -> neutral)


  module String =     
    let trimSharp (str : string) =
      match (str.StartsWith "#") with 
        | true  -> (str.TrimStart '#')
        | false -> str
  
    let hexToInt (hex : char) =
      match hex with
        | c when hex >= '0' && hex <= '9'  -> Some ((int hex) - (int '0'))
        | c when hex >= 'A' && hex <= 'F'  -> Some ((int c) - (int 'A') + 10)
        | c when hex >= 'a' && hex <= 'f'  -> Some ((int c) - (int 'a') + 10)
        | _ -> None

    let explode (str : string) =
      seq {
        for i in 0..(str.Length - 1) do
          yield str.Chars i
      }


    let hex2StrToInt (str : string) =
      let hexSeq =
        str
          |> trimSharp
          |> explode
          |> (Seq.map hexToInt)
      
      let check =
        hexSeq
          |> Seq.map (fun x-> x.IsSome)
          |> Seq.reduce (fun x y -> x && y)

      let ans = 
        match check with
          | true  -> (hexSeq
                        |> Seq.filter (fun x -> x.IsSome)
                        |> Seq.map (fun x -> x.Value)
                        |> Seq.properPairwise (fun x y -> x + y) 0)  
          | false -> Seq.empty
      ans


    
      
    
    
//    let str = "#12AD3F"
//    str 
//      |> hex2StrToInt
//      |> (fun x ->
//              for c in x do
//                printf "%i, " c)


module RenderingPars =
    let initial : RenderingParameters = {
        fillMode = FillMode.Fill
        cullMode = CullMode.None
    }

module Time =

  let getTimestamp = 
    let now = System.DateTime.Now
    sprintf "%04i%03i%02i%02i%04i" 
        now.Year 
        now.DayOfYear 
        now.Hour 
        now.Minute 
        now.Millisecond

module UtilitiesRendering =
  open System
  open Aardvark.Base
  open Aardvark.Base.Rendering
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Aardvark.UI.Primitives
  open Aardvark.SceneGraph
  open Aardvark.Rendering.Text


  let makeLblSg' (str : IMod<string>) (pos : IMod<V3d>) =
    
    let sg =
      adaptive {
        let! pos = pos
        return Sg.text (Font.create "arial" FontStyle.Regular) C4b.White str
            |> Sg.billboard
            |> Sg.depthTest (Mod.constant DepthTestMode.None)
            |> Sg.trafo(Mod.constant (Trafo3d.Translation pos)) 
      }
    sg |> Sg.dynamic

  let makeLblSg (str : string) (pos : V3d) =
    Sg.text (Font.create "arial" FontStyle.Regular) C4b.White (Mod.constant str)
        |> Sg.billboard
        |> Sg.depthTest (Mod.constant DepthTestMode.None)
        |> Sg.trafo(Mod.constant (Trafo3d.Translation pos)) 

  let renderToTexture (runtime : IRuntime) (sg : ISg) =
    // in order to render something to texture, we need to specify how the framebuffer should look like
    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    // our render target needs a size. Since aardvark is cool this size can be dynamic of course
    let size = V2i(1024,1024) |> Mod.init 
    //makeLblSg "aardvark" (V3d.III)
    sg
      // attach a constant view trafo (which makes our box visible)
      |> Sg.viewTrafo (
              CameraView.lookAt (V3d.III * 3.0) V3d.Zero V3d.OOI 
                |> CameraView.viewTrafo 
                |> Mod.constant
          )
      // since our render target size is dynamic, we compute a proj trafo using standard techniques
      |> Sg.projTrafo (size |> Mod.map (fun actualSize -> 
              Frustum.perspective 60.0 0.01 10.0 (float actualSize.X / float actualSize.Y) |> Frustum.projTrafo
            )
          )
      // next, we use Sg.compile in order to turn a scene graph into a render task (a nice composable alias for runtime.CompileRender)
      |> Sg.compile runtime signature 
      |> RenderTask.renderToColor size

  let renderLblTextureQuad (runtime : IRuntime) (cView : IMod<CameraView>) =
    Sg.box' C4b.White Box3d.Unit
      |> Sg.diffuseTexture (renderToTexture runtime (makeLblSg "aardvark" V3d.OOO))
      |> Sg.shader {
              do! DefaultSurfaces.trafo
              do! DefaultSurfaces.diffuseTexture
              do! DefaultSurfaces.simpleLighting
          }
      |> Sg.viewTrafo (cView  |> Mod.map CameraView.viewTrafo )
    |> Sg.noEvents    


//  MAKE DOMNODE FROM SG WITH CAMERA
//    let domNode =
//      let attributes =
//        AttributeMap.ofList [
//              attribute "style" "width:100%; height: 100%;border: 1px solid black"
//              clazz "svgRoot"; 
//        ]
//          ArcBallController.controlledControl 
//          model.camera
//          CameraMessage 
//          (Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0))
//          (AttributeMap.ofList [
//            attribute "style" "width:100%; height: 100%;border: 1px solid black"
//            clazz "svgRoot"; 
//          ])
//          (
//            renderLblTextureQuad runtime model.camera.view
//          )

//// STICK TO 
