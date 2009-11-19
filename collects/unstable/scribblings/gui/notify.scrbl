#lang scribble/manual
@(require scribble/eval
          "../utils.ss"
          (for-label unstable/gui/notify
                     scheme/contract
                     scheme/class
                     scheme/base))

@title[#:tag "gui-notify"]{Notify-boxes}

@(define the-eval (make-base-eval))
@(the-eval '(require scheme/class unstable/private/notify))

@defmodule[unstable/gui/notify]

@unstable[@author+email["Ryan Culpepper" "ryanc@plt-scheme.org"]]

@defclass[notify-box% object% ()]{

A notify-box contains a mutable cell. The notify-box notifies its
listeners when the contents of the cell is changed.

@examples[#:eval the-eval
(define nb (new notify-box% (value 'apple)))
(send nb get)
(send nb set 'orange)
(send nb listen (lambda (v) (printf "New value: ~s\n" v)))
(send nb set 'potato)
]

@defconstructor[([value any/c])]{
  Creates a notify-box initially containing @scheme[value].
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
          [proc (case-> (-> any/c) (-> any/c void?))]
          [#:readonly? readonly? boolean? #f])
         (is-a?/c notify-box%)]{

Creates a notify-box with an initial value of @scheme[(proc)]. Unless
@scheme[readonly?] is true, @scheme[proc] is invoked on the new value
when the notify-box is updated.

Useful for tying a notify-box to a preference or parameter. Of course,
changes made directly to the underlying parameter or state are not
reflected in the notify-box.

@examples[#:eval the-eval
(define animal (make-parameter 'ant))
(define nb (notify-box/pref animal))
(send nb listen (lambda (v) (printf "New value: ~s\n" v)))
(send nb set 'bee)
(animal 'cow)
(send nb get)
(send nb set 'deer)
(animal)
]
}

@defform[(define-notify name value-expr)
         #:contracts ([value-expr (is-a?/c notify-box%)])]{

Class-body form. Declares @scheme[name] as a field and
@schemeidfont{get-@scheme[name]}, @schemeidfont{set-@scheme[name]},
and @schemeidfont{listen-@scheme[name]} as methods that delegate to
the @method[notify-box% get], @method[notify-box% set], and
@method[notify-box% listen] methods of @scheme[value].

The @scheme[value-expr] argument must evaluate to a notify-box, not
just the initial contents for a notify box.

Useful for aggregating many notify-boxes together into one
``configuration'' object.

@examples[#:eval the-eval
(define config%
  (class object%
    (define-notify food (new notify-box% (value 'apple)))
    (define-notify animal (new notify-box% (value 'ant)))
    (super-new)))
(define c (new config%))
(send c listen-food
        (lambda (v) (when (eq? v 'honey) (send c set-animal 'bear))))
(let ([food (get-field food c)])
  (send food set 'honey))
(send c get-animal)
]
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
