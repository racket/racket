#lang scribble/doc
@(require scribble/manual
          "../scribble.ss"
          (for-label scheme/gui unstable/cce/gui))

@title[#:style 'quiet #:tag "cce-gui"]{GUI Widgets}

@defmodule[unstable/cce/gui]

@section{Locked Text Fields}

These classes and mixins provide text and combo field controls that cannot be
directly edited by the user, but may be updated by other controls.

@defmixin[locked-text-field-mixin (text-field%) ()]{

This mixin updates text field classes to prevent user edits, but allow
programmatic update of the text value.  It also sets the undo history length to
a default of 0, as user undo commands are disabled and the history takes up
space.

@defconstructor[([undo-history exact-nonnegative-integer? 0])]{

The mixin adds the @scheme[undo-history] initialization argument to control the
length of the undo history.  It defaults to 0 to save space, but may be set
higher.

The mixin inherits all the initialization arguments of its parent class; it does
not override any of them.

}

@defmethod[#:mode override (set-value [str string?]) void?]{

Unlocks the text field's nested editor, calls the parent class's
@method[text-field% set-value], and then re-locks the editor.

}

}

@defclass[locked-text-field% text-field% ()]{

Equal to @scheme[(locked-text-field-mixin text-field%)].

}

@defclass[locked-combo-field% combo-field% ()]{

Equal to @scheme[(locked-text-field-mixin combo-field%)].

}

@section{Union GUIs}

@defmixin[union-container-mixin (area-container<%>) ()]{

This mixin modifies a container class to display only one of its child areas at
a time, but to leave room to switch to any of them.

@defmethod[(choose [child (is-a?/c subwindow<%>)]) void?]{

This method changes which of the container's children is displayed.  The chosen
child is shown and the previous choice is hidden.

}

}

@defclass[union-pane% pane% ()]{

Equal to @scheme[(union-container-mixin pane%)].

}

@defclass[union-panel% panel% ()]{

Equal to @scheme[(union-container-mixin panel%)].

}
