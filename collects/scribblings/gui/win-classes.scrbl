#lang scribble/doc
@(require "common.rkt" "diagrams.rkt")

@title[#:style '(toc quiet)]{Windowing Classes}

Windows and controls:

@diagram->table[windowing-diagram]

Menus:

@diagram->table[menu-diagram]

Events and other:

@diagram->table[event-diagram]

Alphabetical:

@local-table-of-contents[]

@include-section["area-intf.scrbl"]
@include-section["area-container-intf.scrbl"]
@include-section["area-container-window-intf.scrbl"]
@include-section["button-class.scrbl"]
@include-section["canvas-intf.scrbl"]
@include-section["canvas-class.scrbl"]
@include-section["check-box-class.scrbl"]
@include-section["checkable-menu-item-class.scrbl"]
@include-section["choice-class.scrbl"]
@include-section["clipboard-client-class.scrbl"]
@include-section["clipboard-intf.scrbl"]
@include-section["combo-field-class.scrbl"]
@include-section["control-intf.scrbl"]
@include-section["column-control-event-class.scrbl"]
@include-section["control-event-class.scrbl"]
@include-section["cursor-class.scrbl"]
@include-section["dialog-class.scrbl"]
@include-section["event-class.scrbl"]
@include-section["frame-class.scrbl"]
@include-section["gauge-class.scrbl"]
@include-section["group-box-panel-class.scrbl"]
@include-section["grow-box-spacer-pane-class.scrbl"]
@include-section["horizontal-pane-class.scrbl"]
@include-section["horizontal-panel-class.scrbl"]
@include-section["key-event-class.scrbl"]
@include-section["labelled-menu-item-intf.scrbl"]
@include-section["list-box-class.scrbl"]
@include-section["list-control-intf.scrbl"]
@include-section["menu-class.scrbl"]
@include-section["menu-bar-class.scrbl"]
@include-section["menu-item-intf.scrbl"]
@include-section["menu-item-class.scrbl"]
@include-section["menu-item-container-intf.scrbl"]
@include-section["message-class.scrbl"]
@include-section["mouse-event-class.scrbl"]
@include-section["pane-class.scrbl"]
@include-section["panel-class.scrbl"]
@include-section["popup-menu-class.scrbl"]
@include-section["printer-dc-class.scrbl"]
@include-section["radio-box-class.scrbl"]
@include-section["selectable-menu-item-intf.scrbl"]
@include-section["separator-menu-item-class.scrbl"]
@include-section["scroll-event-class.scrbl"]
@include-section["slider-class.scrbl"]
@include-section["subarea-intf.scrbl"]
@include-section["subwindow-intf.scrbl"]
@include-section["tab-panel-class.scrbl"]
@include-section["text-field-class.scrbl"]
@include-section["timer-class.scrbl"]
@include-section["top-level-window-intf.scrbl"]
@include-section["vertical-pane-class.scrbl"]
@include-section["vertical-panel-class.scrbl"]
@include-section["window-intf.scrbl"]
