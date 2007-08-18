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
