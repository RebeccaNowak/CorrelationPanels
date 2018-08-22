namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI


module UI = 
    // GENERAL
    let colorToHexStr (color : C4b) = 
        let bytes = [| color.R; color.G; color.B |]
        let str =
            bytes 
                |> (Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x)))
                |> (String.concat System.String.Empty)
        String.concat String.Empty ["#";str] 

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


    let toggleButton (str : string) (onClick : list<string> -> 'msg) = 
      Incremental.button
        (AttributeMap.ofList (
                              [clazz "small ui toggle button";
                               style "margin: 1px 1px 1px 1px"
                              ]@(Event.toggleAttribute onClick)) 
                             )
        (AList.ofList [text str])
          

    module Incremental =
      let iconButton (iconStr : string) (onClick : V2i -> 'msg) = 
        //div [clazz "item"] 
            //[
              button [clazz "ui icon button"; onMouseClick onClick] 
                      [i [clazz iconStr] [] ] //TODO |> wrapToolTip tooltip 
            //]
      let toggleButton (str : IMod<string>) (onClick : V2i -> 'msg) = 
        button [clazz "ui toggle button"; onMouseClick onClick] [Incremental.text str]
      
    let getColourIconButton (color : IMod<C4b>) (label : IMod<string>) (onClick : V2i -> 'msg) =
      let icon = 
        let iconAttr =
          amap {
            yield clazz "circle icon"
            let! c = color
            yield style (sprintf "color:%s" (colorToHexStr c))
          }      
        Incremental.i (AttributeMap.ofAMap iconAttr) (alist{yield Incremental.text label})
      button [clazz "ui labeled icon button"; onMouseClick onClick] 
             [icon]

    let getColourIconButton' (color : IMod<C4b>) (onClick : V2i -> 'msg) =
      let icon = 
        let iconAttr =
          amap {
            yield clazz "circle icon"
            let! c = color
            yield style (sprintf "color:%s" (colorToHexStr c))
          }      
        Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList [])
      button [clazz "ui icon button"; onMouseClick onClick] 
             [icon]



    

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
     
  module Table =
    let intoTd (x) = 
      td [clazz "center aligned"] [x]

    let toTableView (menu : DomNode<'msg>) 
                    (rows : alist<DomNode<'msg>>) 
                    (columnNames : list<string>) = 
      let myCss = [
                { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
                { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
              ]

      let header = 
        columnNames
          |> List.map (fun str -> th[] [text str])

      require (myCss) (
        body [clazz "ui"; style "background: #1B1C1E;position:fixed;width:100%;overflow: auto;"] [
          div [] [
            menu
            table
              ([clazz "ui celled striped inverted table unstackable";
                                    style "padding: 1px 5px 2px 5px"]) (
                  [
                    thead [][tr[] header]
                    Incremental.tbody  (AttributeMap.ofList []) rows
                  ]
              )
          ]
        ]
      )
