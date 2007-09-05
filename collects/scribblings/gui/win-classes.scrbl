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

@include-class-section[area<%>]
@include-class-section[area-container<%>]
@include-class-section[area-container-window<%>]
@include-class-section[button%]
@include-class-section[canvas<%>]
@include-class-section[canvas%]
@include-class-section[check-box%]
@include-class-section[checkable-menu-item%]
@include-class-section[choice%]
@include-class-section[clipboard<%>]
@include-class-section[clipboard-client%]
@include-class-section[combo-field%]
@include-class-section[control<%>]
@include-class-section[control-event%]
@include-class-section[cursor%]
@include-class-section[dialog%]
@include-class-section[event%]
@include-class-section[frame%]
@include-class-section[gauge%]
@include-class-section[group-box-panel%]
@include-class-section[grow-box-spacer-pane%]
@include-class-section[horizontal-pane%]
@include-class-section[horizontal-panel%]
@include-class-section[key-event%]
@include-class-section[labelled-menu-item<%>]
@include-class-section[list-box%]
@include-class-section[list-control<%>]
@include-class-section[menu%]
@include-class-section[menu-bar%]
@include-class-section[menu-item<%>]
@include-class-section[menu-item%]
@include-class-section[menu-item-container<%>]
@include-class-section[message%]
@include-class-section[mouse-event%]
@include-class-section[pane%]
@include-class-section[panel%]
@include-class-section[popup-menu%]
@include-class-section[radio-box%]
@include-class-section[scroll-event%]
@include-class-section[selectable-menu-item<%>]
@include-class-section[separator-menu-item%]
@include-class-section[slider%]
@include-class-section[subarea<%>]
@include-class-section[subwindow<%>]
@include-class-section[tab-panel%]
@include-class-section[text-field%]
@include-class-section[timer%]
@include-class-section[top-level-window<%>]
@include-class-section[vertical-pane%]
@include-class-section[vertical-panel%]
@include-class-section[window<%>]
