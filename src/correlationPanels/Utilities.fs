namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering





    
      
    
    
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
