#lang scribble/manual
@(require scribble/eval
          "utils.ss"
          (for-label unstable/poly-c
                     scheme/contract
                     scheme/base))

@title[#:tag "poly-c"]{Polymorphic Contracts}

@(define (build-eval)
   (let* ([e (make-base-eval)])
     (e '(require unstable/poly-c scheme/contract))
     e))

@defmodule[unstable/poly-c]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]
          @author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

@defform[(poly/c (x ...) c)]{

Creates a contract for polymorphic functions that may inspect their arguments.
Each function is protected by @scheme[c], where each @scheme[x] is bound in
@scheme[c] and refers to a polymorphic type that is instantiated each time the
function is applied.

At each application of a function, the @scheme[poly/c] contract constructs a new
weak, @scheme[eq?]-based hash table for each @scheme[x].  Values flowing into
the polymorphic function (i.e. values protected by some @scheme[x] in negative
position with respect to @scheme[poly/c]) are stored in the hash table.  Values
flowing out of the polymorphic function (i.e. protected by some @scheme[x] in
positive position with respect to @scheme[poly/c]) are checked for their
presence in the hash table.  If they are present, they are returned; otherwise,
a contract violation is signalled.

@examples[#:eval (build-eval)
(define/contract (check x y) (poly/c [X] (boolean? X . -> . X))
  (if (or (not x) (equal? y 'surprise))
      'invalid
      y))
(check #t 'ok)
(check #f 'ignored)
(check #t 'surprise)
]

}

@defform[(parametric/c (x ...) c)]{

Creates a contract for parametric polymorphic functions.  Each function is
protected by @scheme[c], where each @scheme[x] is bound in @scheme[c] and refers
to a polymorphic type that is instantiated each time the function is applied.

At each application of a function, the @scheme[parametric/c] contract constructs
a new opaque wrapper for each @scheme[x]; values flowing into the polymorphic
function (i.e. values protected by some @scheme[x] in negative position with
respect to @scheme[parametric/c]) are wrapped in the corresponding opaque
wrapper.  Values flowing out of the polymorphic function (i.e. values protected
by some @scheme[x] in positive position with respect to @scheme[parametric/c])
are checked for the appropriate wrapper.  If they have it, they are unwrapped;
if they do not, a contract violation is signalled.

@examples[#:eval (build-eval)
(define/contract (check x y) (parametric/c [X] (boolean? X . -> . X))
  (if (or (not x) (equal? y 'surprise))
      'invalid
      y))
(check #t 'ok)
(check #f 'ignored)
(check #t 'surprise)
]

}

@defproc[(memory/c [positive? boolean?] [name any/c]) contract?]{

This function constructs a contract that records values flowing in one direction
in a fresh, weak hash table, and looks up values flowing in the other direction,
signalling a contract violation if those values are not in the table.

If @scheme[positive?] is true, values in positive position get stored and values
in negative position are checked.  Otherwise, the reverse happens.

}

@defproc[(opaque/c [positive? boolean?] [name any/c]) contract?]{

This function constructs a contract that wraps values flowing in one direction
in a unique, opaque wrapper, and unwraps values flowing in the other direction,
signalling a contract violation if those values are not wrapped.

If @scheme[positive?] is true, values in positive position get wrapped and
values in negative position get unwrapped.  Otherwise, the reverse happens.

}
