#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/set))

@title{Sets}

@defmodule[unstable/set]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for representing finite sets.

@deftogether[(
@defproc[(list->set [lst list?]) set?]
@defproc[(list->seteq [lst list?]) set?]
@defproc[(list->seteqv [lst list?]) set?]
)]{

Produces the appropriate type of set containing the elements of the given list.

@defexamples[
#:eval (eval/require 'racket/set 'unstable/set)
(define lst
  (list 'atom (expt 2 100) (list 'compound)
        'atom (expt 2 100) (list 'compound)))
(list->set lst)
(list->seteqv lst)
(list->seteq lst)
]

}

@defproc[(set=? [a set?] [b set?]) boolean?]{

Reports whether two sets contain the same elements.

@defexamples[
#:eval (eval/require 'racket/set 'unstable/set)
(set=? (set 1) (set 1 2 3))
(set=? (set 1 2 3) (set 1))
(set=? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(proper-subset? [a set?] [b set?]) boolean?]{

Reports whether @scheme[b] contains all of the elements of @scheme[a], and at
least one element not in @scheme[a].

@defexamples[
#:eval (eval/require 'racket/set 'unstable/set)
(proper-subset? (set 1) (set 1 2 3))
(proper-subset? (set 1 2 3) (set 1))
(proper-subset? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(set->list [s set?]) list?]{

Produces a list containing the elements of @scheme[s].

@defexamples[
#:eval (eval/require 'racket/set 'unstable/set)
(set->list (set 1 2 3))
]

}

@defproc[(set-exclusive-or [s set?] ...+) set?]{

Produces a set containing only those elements found in each @scheme[s] an odd
number of times.

@defexamples[
#:eval (eval/require 'racket/set 'unstable/set)
(set-exclusive-or (set 1) (set 1 2) (set 1 2 3))
]

}
