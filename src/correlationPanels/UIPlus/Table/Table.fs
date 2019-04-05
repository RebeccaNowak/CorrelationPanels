namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open UIPlus
  open UIPlus.TableTypes

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Table =

    let init mapper colHeadings =
      {
        mapper      = mapper
        colHeadings = colHeadings
      }

    let view  (guiModel  : Table<'dtype, 'arg, 'mtype, 'action>)
              (data      : alist<'mtype>) 
              (args      : alist<'arg>) =
      let magic (d : 'mtype) (arg : 'arg) =
        let row = guiModel.mapper arg  d 
        let domNode = TableRow.view row d
        domNode
      
      let header = 
        guiModel.colHeadings
          |> List.map (fun str -> th[] [text str])

      require (GUI.CSS.myCss) (
        table
          ([clazz "ui celled striped inverted table unstackable";
                                style "padding: 1px 5px 2px 5px"]) (
              [
                thead [][tr[] header]
                //Incremental.tbody  (AttributeMap.ofList []) rows
              ]
          )
      )
