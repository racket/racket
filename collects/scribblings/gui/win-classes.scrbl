#reader(lib "docreader.ss" "scribble")
@require["common.ss"]
@require["diagrams.ss"]
@require["mred-classes.ss"]

@title[#:style '(toc quiet)]{Windowing Classes}

Windows and controls:

@diagram->table[windowing-diagram]

Menus:

@diagram->table[menu-diagram]

Events and other:

@diagram->table[event-diagram]

Alphabetical:

@local-table-of-contents[]

@include-class[area<%>]
@include-class[area-container<%>]
@include-class[area-container-window<%>]
@include-class[button%]
@include-class[canvas<%>]
@include-class[canvas%]
@include-class[check-box%]
@include-class[checkable-menu-item%]
@include-class[choice%]
@include-class[clipboard<%>]
@include-class[clipboard-client%]
@include-class[combo-field%]
@include-class[control<%>]
@include-class[control-event%]
@include-class[cursor%]
@include-class[dialog%]
@include-class[event%]
@include-class[frame%]
@include-class[gauge%]
@include-class[group-box-panel%]
@include-class[grow-box-spacer-pane%]
@include-class[horizontal-pane%]
@include-class[horizontal-panel%]
@include-class[key-event%]
@include-class[labelled-menu-item<%>]
@include-class[list-box%]
@include-class[list-control<%>]
@include-class[menu%]
@include-class[menu-bar%]
@include-class[menu-item<%>]
@include-class[menu-item%]
@include-class[menu-item-container<%>]
@include-class[message%]
@include-class[mouse-event%]
@include-class[pane%]
@include-class[panel%]
@include-class[popup-menu%]
@include-class[radio-box%]
@include-class[scroll-event%]
@include-class[selectable-menu-item<%>]
@include-class[separator-menu-item%]
@include-class[slider%]
@include-class[subarea<%>]
@include-class[subwindow<%>]
@include-class[tab-panel%]
@include-class[text-field%]
@include-class[timer%]
@include-class[top-level-window<%>]
@include-class[vertical-pane%]
@include-class[vertical-panel%]
@include-class[window<%>]
