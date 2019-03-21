namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open UIPlus
  open UIPlus.TableTypes
  open SimpleTypes

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module TableRow =
    
    let init isSelected onSelect update displayView editView alignment : TableRow<'dtype, 'mtype, 'action> = //, 'mtype, 'action> =
      {
        isSelected    = isSelected
        onSelect      = onSelect
        update        = update
        displayView   = displayView
        editView      = editView
        align         = alignment
      }

    let update (guiModel  : TableRow<'dtype, 'mtype, 'action>) 
               (dataModel : 'dtype)
               (action    : 'action) =
      guiModel.update dataModel action

    let intoTd (domNode) = 
      td [clazz "center aligned"] [domNode]

    let intoLeftAlignedTd (domNode) =
      td [clazz "left aligned"] [domNode]

    let intoTd' domNode colSpan = 
      td [clazz "center aligned";
          style GUI.CSS.lrPadding;
          attribute "colspan" (sprintf "%i" colSpan)]
          [domNode]

    let intoTr domNodes =
      tr [] domNodes

    let intoTr' (domNode) (atts : list<string * AttributeValue<'a>>) =
      tr atts [domNode]

    let intoTrOnClick fOnClick domNodeList =
      tr [onClick (fun () -> fOnClick)] domNodeList

    let intoActiveTr fOnClick domNodeList =
      tr [clazz "active";onClick (fun () -> fOnClick)] domNodeList

    let view   (guiModel  : TableRow<'dtype, 'mtype, 'action>)
               (dataModel : 'mtype) =
      alist {
        let! sel = guiModel.isSelected dataModel
        let row = 
          match sel with
            | true ->
              let itemsContent = guiModel.editView dataModel
              let items =
                itemsContent |> List.map intoTd
              intoActiveTr guiModel.onSelect items
            | false ->
              let itemsContent = guiModel.displayView dataModel
              let items =
                itemsContent |> List.map intoTd
              intoTrOnClick guiModel.onSelect items
        yield row
      }
      

