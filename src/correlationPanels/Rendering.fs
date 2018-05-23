namespace CorrelationDrawing
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Rendering
  open Aardvark.UI
  open Aardvark.SceneGraph

  module Sg = 
    let sphereDyn (color : IMod<C4b>) (size : IMod<float>) =
      Sg.sphere 3 color size 
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.noEvents    
    
    let sphereWithEvents color size (events : List<SceneEventKind * (SceneHit -> bool * seq<'msg>)>)  =      
      Sg.sphere 3 color size 
              |> Sg.shader {
                  do! DefaultSurfaces.trafo
                  do! DefaultSurfaces.vertexColor
                  do! DefaultSurfaces.simpleLighting
              }
              |> Sg.requirePicking
              |> Sg.noEvents
              |> Sg.withEvents events
    
    let pathDyn (close : bool) (points : List<V3d>) = 
      let head = points |> List.tryHead
      match head with
        | Some h -> 
            if close then points @ [h] else points
              |> List.pairwise
              |> List.map (fun (a,b) -> new Line3d(a, b)) //Mod.map2 (fun a b -> new Line3d(a, b)) a b)
              |> List.toArray
        | None -> [||]     
      

    let makeCylinderSg (c : C4b) =      
      Sg.cylinder' 3 c 0.1 20.0 
              |> Sg.shader {
                  do! DefaultSurfaces.trafo
                  do! DefaultSurfaces.vertexColor
                  do! DefaultSurfaces.simpleLighting
              }
              |> Sg.noEvents

  //let makeLblSg (str : string) (pos : V3d) =
  //  Sg.text (Font.create "arial" FontStyle.Regular) C4b.White (Mod.constant str)
  //      |> Sg.billboard
  //      |> Sg.noEvents
  //      |> Sg.depthTest (Mod.constant DepthTestMode.None)
  //      |> Sg.trafo(Mod.constant (Trafo3d.Translation pos))          





