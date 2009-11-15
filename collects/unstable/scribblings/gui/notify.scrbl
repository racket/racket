#lang scribble/manual
@(require (for-label unstable/gui/notify
                     scheme/contract
                     scheme/class
                     scheme/base))

@title[#:tag "gui-notify"]{Notify-boxes}

@defmodule[unstable/gui/notify]

@defclass[notify-box% object% ()]{

A notify-box contains a mutable cell. The notify-box notifies its
listeners when the contents of the cell is changed.

@defconstructor[([value any/c])]{
  Creates a notify-box with the initial value @scheme[value].
}
@defmethod[(get) any/c]{
  Gets the value currently stored in the notify-box.
}
@defmethod[(set [v any/c]) void?]{
  Updates the value stored in the notify-box and notifies the listeners.
}
@defmethod[(listen [listener (-> any/c any)]) void?]{
  Adds a callback to be invoked on the new value when the notify-box's
  contents change.
}
@defmethod[(remove-listener [listener (-> any/c any)]) void?]{
  Removes a previously-added callback.
}
@defmethod[(remove-all-listeners) void?]{
  Removes all previously registered callbacks.
}
}

@defproc[(notify-box/pref
          [proc (case-> (-> any/c) (-> any/c void?))])
         (is-a?/c notify-box%)]{

Creates a notify-box with an initial value of @scheme[(proc)] that
invokes @scheme[proc] on the new value when the notify-box is updated.

Useful for making a notify-box tied to a preference or parameter.
}

@defproc[(notify-box/pref/readonly [proc (-> any/c)])
         (is-a?/c notify-box%)]{

Creates a notify-box with an initial value of @scheme[(proc)].

Useful for making a notify-box that takes its initial value from a
preference or parameter but does not update the preference or
parameter.
}

@defproc[(menu-option/notify-box 
          [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
          [label label-string?]
          [notify-box (is-a?/c notify-box%)])
         (is-a?/c checkable-menu-item%)]{

Creates a @scheme[checkable-menu-item%] tied to @scheme[notify-box]. The menu item is
checked whenever @scheme[(send notify-box get)] is true. Clicking the
menu item toggles the value of @scheme[notify-box] and invokes its listeners.
}

@defproc[(check-box/notify-box
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                        (is-a?/c panel%) (is-a?/c pane%))]
          [label label-string?]
          [notify-box (is-a?/c notify-box%)])
         (is-a?/c check-box%)]{

Creates a @scheme[check-box%] tied to @scheme[notify-box]. The
check-box is checked whenever @scheme[(send notify-box get)] is
true. Clicking the check box toggles the value of @scheme[notify-box]
and invokes its listeners.
}

@defproc[(choice/notify-box
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                        (is-a?/c panel%) (is-a?/c pane%))]
          [label label-string?]
          [choices (listof label-string?)]
          [notify-box (is-a?/c notify-box%)])
         (is-a?/c choice%)]{

Creates a @scheme[choice%] tied to @scheme[notify-box]. The choice
control has the value @scheme[(send notify-box get)] selected, and
selecting a different choice updates @scheme[notify-box] and invokes
its listeners.

If the value of @scheme[notify-box] is not in @scheme[choices], either
initially or upon an update, an error is raised.
}

@defproc[(menu-group/notify-box
          [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
          [labels (listof label-string?)]
          [notify-box (is-a?/c notify-box%)])
         (listof (is-a?/c checkable-menu-item%))]{

Returns a list of @scheme[checkable-menu-item%] controls tied to
@scheme[notify-box]. A menu item is checked when its label is
@scheme[(send notify-box get)]. Clicking a menu item updates
@scheme[notify-box] to its label and invokes @scheme[notify-box]'s
listeners.
}

