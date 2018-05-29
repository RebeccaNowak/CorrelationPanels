namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI


module UtilitiesGUI = 
    // SEMUI
    type Size = Mini | Tiny | Small | Medium | Large | Big | Huge | Massive
      with member this.semuiString = 
            match this with
              | Mini    -> "mini"
              | Tiny    -> "tiny"
              | Small   -> "small"
              | Medium  -> "medium"
              | Large   -> "large"
              | Big     -> "big"
              | Huge    -> "huge"
              | Massive -> "massive"

    let myCss = [
              { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
              { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
              { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]

    // TOOLTIPS
    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =
        let attr = 
            [attribute "title" text
             attribute "data-position" "top center"
             attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipRight (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "right center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipBottom (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "bottom center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )


    // ICONS


    module Icons =
      type Type = ArrowDown | ArrowUp 
        with member this.semuiString =
              match this with
               | ArrowDown      -> "arrow down"
               | ArrowUp        -> "arrow up"

      
      type Icon = {
        typ   : Type
        size  : Size
      } with
          member this.semuiString =
                sprintf "%s %s icon" this.size.semuiString this.typ.semuiString

      let icon (t : Type) : Icon = {
        typ = t
        size = Size.Medium
      }
      
      type IconButton = {
        typ   : Type
        size  : Size
      }

    let iconButton (iconStr : string) (tooltip : string) (onClick : V2i -> 'msg) = 
      div [clazz "item"]
          [
            button [clazz "ui icon button"; onMouseClick onClick] 
                    [i [clazz iconStr] [] ] |> wrapToolTip tooltip
          ]
      
      

    // GENERAL
    let colorToHexStr (color : C4b) = 
        let bytes = [| color.R; color.G; color.B |]
        let str =
            bytes 
                |> (Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x)))
                |> (String.concat System.String.Empty)
        String.concat String.Empty ["#";str] 


    

    // ATTRIBUTES
    let bgColorAttr (color : C4b) =
      style (sprintf "background: %s" (colorToHexStr color))

    let bgColorStr (color : C4b) =
      (sprintf "background: %s" (colorToHexStr color))

    let incrBgColorAMap (colorMod : IMod<C4b>) =      
      amap { 
        let! col =  colorMod
        let str = (sprintf "background: %s" (colorToHexStr col))
        yield style str
      }

    let incrBgColorAttr (colorMod : IMod<C4b>) =
      colorMod 
        |> Mod.map (fun x -> 
                      style (sprintf "background-color: %s" (colorToHexStr x)))      

       

    let modColorToColorAttr (c : IMod<C4b>) =
      let styleStr = Mod.map (fun x -> (sprintf "color:%s" (colorToHexStr x))) c
      Mod.map (fun x -> style x) styleStr  

    let noPadding  = "padding: 0px 0px 0px 0px"
    let tinyPadding  = "padding: 1px 1px 1px 1px"
    let lrPadding = "padding: 1px 4px 1px 4px"





        // GUI ELEMENTS

