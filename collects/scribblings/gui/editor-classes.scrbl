#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]

@title[#:style '(toc quiet)]{Editor Classes}

Editors:

@diagram->table[editor-diagram]

Snips:

@diagram->table[snip-diagram]

Displays, Administrators, and Mappings:

@diagram->table[editor-diagram]

Styles:

@diagram->table[style-diagram]

Streams for Saving and Cut-and-Paste:

@diagram->table[stream-diagram]

Alphabetical:

@local-table-of-contents[]

@require["add-color-intf.scrbl"]
@require["editor-intf.scrbl"]
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

@include-class[add-color<%>]
@include-class[editor<%>]
@include-class[editor-admin%]
@include-class[editor-canvas%]
@include-class[editor-data%]
@include-class[editor-data-class%]
@include-class[editor-data-class-list<%>]
@include-class[editor-snip-editor-admin<%>]
@include-class[editor-snip%]
@include-class[editor-stream-in%]
@include-class[editor-stream-in-base%]
@include-class[editor-stream-in-bytes-base%]
@include-class[editor-stream-out%]
@include-class[editor-stream-out-base%]
@include-class[editor-stream-out-bytes-base%]
@include-class[editor-wordbreak-map%]
@include-class[image-snip%]
@include-class[keymap%]
@include-class[mult-color<%>]
@include-class[pasteboard%]
@include-class[readable-snip<%>]
@include-class[snip%]
@include-class[snip-admin%]
@include-class[snip-class%]
@include-class[snip-class-list<%>]
@include-class[string-snip%]
@include-class[style<%>]
@include-class[style-delta%]
@include-class[style-list%]
@include-class[tab-snip%]
@include-class[text%]
