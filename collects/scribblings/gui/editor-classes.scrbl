#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]

@title[#:style '(toc quiet)]{Editor Class Reference}

@local-table-of-contents[]

@require["add-color-intf.scrbl"]
@require["editor-intf.scrbl"]
@;{
@require["editor-admin-class.scrbl"]
@require["editor-canvas-class.scrbl"]
@require["editor-data-class.scrbl"]
@require["editor-data-class-class.scrbl"]
@require["editor-data-class-list-intf.scrbl"]
@require["editor-snip-editor-admin-intf.scrbl"]
@require["editor-snip-class.scrbl"]
@require["editor-stream-in-class.scrbl"]
@require["editor-stream-in-base-class.scrbl"]
@require["editor-stream-in-bytes-base-class.scrbl"]
@require["editor-stream-out-class.scrbl"]
@require["editor-stream-out-base-class.scrbl"]
@require["editor-stream-out-bytes-base-class.scrbl"]
@require["editor-wordbreak-map-class.scrbl"]
@require["image-snip-class.scrbl"]
@require["keymap-class.scrbl"]
@require["mult-color-intf.scrbl"]
@require["pasteboard-class.scrbl"]
@require["readable-snip-intf.scrbl"]
@require["snip-class.scrbl"]
@require["snip-admin-class.scrbl"]
@require["snip-class-class.scrbl"]
@require["snip-class-list-intf.scrbl"]
@require["string-snip-class.scrbl"]
@require["style-intf.scrbl"]
@require["style-delta-class.scrbl"]
@require["style-list-class.scrbl"]
@require["tab-snip-class.scrbl"]
@require["text-class.scrbl"]
}

@include-class[add-color<%>]
@include-class[editor<%>]
