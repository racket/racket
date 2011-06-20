#lang scribble/doc
@(require "common.rkt" "diagrams.rkt")

@title[#:style '(toc quiet)]{Editor Classes}

Editors:

@diagram->table[editor-diagram]

Editor Snips:

@diagram->table[editor-snip-diagram]

Displays, Administrators, and Mappings:

@diagram->table[editor-admin-diagram]

Streams for Saving and Cut-and-Paste:

@diagram->table[stream-diagram]

Alphabetical:

@local-table-of-contents[]

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
@include-section["keymap-class.scrbl"]
@include-section["pasteboard-class.scrbl"]
@include-section["text-class.scrbl"]
