#lang scribble/doc

@(require scribble/manual
          (for-label scheme 
	  	     teachpack/htdp/show-queen))

@title[#:tag "show-queen"]{8 Queens: show-queen.ss}

@declare-exporting[teachpack/htdp/show-queen]

The teachpack provides the operation @scheme[show-queen], which implements
a GUI for exploring the n-queens problem.

@defproc[(show-queen [board (list-of (list-of boolean?))]) true]{The
function @scheme[show-queen] consumes a list of 
lists of booleans that describes a @scheme[board]. Each of the inner
lists must have the same length as the outer list. The
@scheme[true]s correspond to positions where queens are,
and the @scheme[false]s correspond to empty squares. The
function returns nothing.

In the GUI window that @scheme[show-queen] opens, the
red and orange dots show where the queens are. The green dot
shows where the mouse cursor is. Each queen that threatens
the green spot is shown in red, and the queens that do not
threaten the green spot are shown in orange.}
