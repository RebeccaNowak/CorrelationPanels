namespace Mars

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.SceneGraph.Opc
open MBrace.FsPickler

module Terrain = 
    let up = V3d.OIO
    

    let dummyMars events =
        Sg.sphere 5 (Mod.constant (new C4b(254,178,76))) (Mod.constant 10.0) 
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.requirePicking
            |> Sg.noEvents
            |> Sg.withEvents events
            |> Sg.translate 0.0 10.0 0.0

    let pickler = FsPickler.CreateBinarySerializer()
    let pickle = pickler.Pickle<QTree<Patch>>
    let unpickle = pickler.UnPickle<QTree<Patch>>

//open Aardvark.Opc

//module Terrain =
    
//    let mars () =
//        let patchHierarchies =
//            System.IO.Directory.GetDirectories(@"..\..\data\mars")
//            |> Seq.collect System.IO.Directory.GetDirectories

//        { 
//            useCompressedTextures = true
//            preTransform     = Trafo3d.Identity
//            patchHierarchies = patchHierarchies
//            boundingBox      = Box3d.Parse("[[3376372.058677169, -325173.566694686, -121309.194857123], [3376385.170513898, -325152.282144333, -121288.943956908]]")
//            near             = 0.1
//            far              = 10000.0
//            speed            = 3.0
//            lodDecider       = DefaultMetrics.mars
//        }
    
//    let scene = mars()

//    let up = scene.boundingBox.Center.Normalized

//    let preTransform =
//        let bb = scene.boundingBox
//        //Trafo3d.Translation(-bb.Center) * scene.preTransform
//        Trafo3d.Translation(-bb.Center + (24.0 * up)) * scene.preTransform
    


//    let mkISg() =
//        Aardvark.Opc.Sg2.createFlatISg scene
//        |> Sg.noEvents
//        |> Sg.transform preTransform
    
//    let defaultEffects =
//        [
//            DefaultSurfaces.trafo |> toEffect
//            DefaultSurfaces.constantColor C4f.White |> toEffect
//            DefaultSurfaces.diffuseTexture |> toEffect
//        ]

//    let buildKDTree (g : IndexedGeometry) =
//        let pos = g.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>
//        let index = g.IndexArray |> unbox<int[]>

//        let triangles =
//            [| 0 .. 3 .. index.Length - 2 |] 
//                |> Array.choose (fun bi -> 
//                    let p0 = pos.[index.[bi]]
//                    let p1 = pos.[index.[bi + 1]]
//                    let p2 = pos.[index.[bi + 2]]
//                    if isNan p0 || isNan p1 || isNan p2 then
//                        None
//                    else
//                        Triangle3d(V3d p0, V3d p1, V3d p2) |> Some
//                )
        
//        let tree = Geometry.KdTree.build Geometry.Spatial.triangle (Geometry.KdBuildInfo(100, 5)) triangles
//        tree
    
//    let patchHierarchies =
//        [ 
//            for h in scene.patchHierarchies do
//                let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
//                yield PatchHierarchy.load p
//        ]
    
//    let leaves = 
//        patchHierarchies 
//        |> List.collect(fun x ->  
//            x.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun y -> (x.baseDir, y)))
        
//    let kdTrees =
//        leaves
//        |> List.map(fun (dir,patch) -> (Patch.load dir patch.info, dir, patch.info))
//        |> List.map(fun ((a,_),c,d) -> (a,c,d))
//        |> List.map ( fun (g,dir,info) ->
//            buildKDTree g
//        )
    
//    let pickSg events =
//        leaves
//        |> List.map(fun (dir,patch) -> (Patch.load dir patch.info, dir, patch.info))
//        |> List.map(fun ((a,_),c,d) -> (a,c,d))               
//        |> List.map2 ( fun t (g,dir,info) ->
//            let pckShp = t |> PickShape.Triangles
//            Sg.ofIndexedGeometry g
//            |> Sg.pickable pckShp
//            |> Sg.trafo (Mod.constant info.Local2Global)
//        ) kdTrees
//        |> Sg.ofList
//        |> Sg.requirePicking
//        |> Sg.noEvents
//        |> Sg.withEvents events
//        |> Sg.transform preTransform
//        |> Sg.shader {
//            do! DefaultSurfaces.trafo
//            do! DefaultSurfaces.constantColor C4f.DarkRed
//        }
//        |> Sg.depthTest (Mod.constant DepthTestMode.Never)
