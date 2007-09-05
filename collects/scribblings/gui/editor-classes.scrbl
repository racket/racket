#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]
@require["mred-classes.ss"]

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

@include-class-section[add-color<%>]
@include-class-section[editor<%>]
@include-class-section[editor-admin%]
@include-class-section[editor-canvas%]
@include-class-section[editor-data%]
@include-class-section[editor-data-class%]
@include-class-section[editor-data-class-list<%>]
@include-class-section[editor-snip-editor-admin<%>]
@include-class-section[editor-snip%]
@include-class-section[editor-stream-in%]
@include-class-section[editor-stream-in-base%]
@include-class-section[editor-stream-in-bytes-base%]
@include-class-section[editor-stream-out%]
@include-class-section[editor-stream-out-base%]
@include-class-section[editor-stream-out-bytes-base%]
@include-class-section[editor-wordbreak-map%]
@include-class-section[image-snip%]
@include-class-section[keymap%]
@include-class-section[mult-color<%>]
@include-class-section[pasteboard%]
@include-class-section[readable-snip<%>]
@include-class-section[snip%]
@include-class-section[snip-admin%]
@include-class-section[snip-class%]
@include-class-section[snip-class-list<%>]
@include-class-section[string-snip%]
@include-class-section[style<%>]
@include-class-section[style-delta%]
@include-class-section[style-list%]
@include-class-section[tab-snip%]
@include-class-section[text%]
