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

@include-section["add-color-intf.scrbl"]
@include-section["editor-intf.scrbl"]
@include-section["editor-admin-class.scrbl"]
@include-section["editor-canvas-class.scrbl"]
@include-section["editor-data-class.scrbl"]
@include-section["editor-data-class-class.scrbl"]
@include-section["editor-data-class-list-intf.scrbl"]
@include-section["editor-snip-editor-admin-intf.scrbl"]
@include-section["editor-snip-class.scrbl"]
@include-section["editor-stream-in-class.scrbl"]
@include-section["editor-stream-in-base-class.scrbl"]
@include-section["editor-stream-in-bytes-base-class.scrbl"]
@include-section["editor-stream-out-class.scrbl"]
@include-section["editor-stream-out-base-class.scrbl"]
@include-section["editor-stream-out-bytes-base-class.scrbl"]
@include-section["editor-wordbreak-map-class.scrbl"]
@include-section["image-snip-class.scrbl"]
@include-section["keymap-class.scrbl"]
@include-section["mult-color-intf.scrbl"]
@include-section["pasteboard-class.scrbl"]
@include-section["readable-snip-intf.scrbl"]
@include-section["snip-class.scrbl"]
@include-section["snip-admin-class.scrbl"]
@include-section["snip-class-class.scrbl"]
@include-section["snip-class-list-intf.scrbl"]
@include-section["string-snip-class.scrbl"]
@include-section["style-intf.scrbl"]
@include-section["style-delta-class.scrbl"]
@include-section["style-list-class.scrbl"]
@include-section["tab-snip-class.scrbl"]
@include-section["text-class.scrbl"]
