#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/show-queen))

@teachpack["show-queen"]{Queens}

@;declare-exporting[teachpack/htdp/show-queen]
@defmodule[#:require-form beginner-require htdp/show-queen]

The teachpack provides the function @racket[show-queen], which implements
a GUI for exploring the n-queens problem.

@defproc[(show-queen [board (list-of (list-of boolean?))]) true]{The
function @racket[show-queen] consumes a list of 
lists of booleans that describes a @racket[board]. Each of the inner
lists must have the same length as the outer list. The
@racket[true]s correspond to positions where queens are,
and the @racket[false]s correspond to empty squares. The
function returns nothing.

In the GUI window that @racket[show-queen] opens, the
red and orange dots show where the queens are. The green dot
shows where the mouse cursor is. Each queen that threatens
the green spot is shown in red, and the queens that do not
threaten the green spot are shown in orange.}
