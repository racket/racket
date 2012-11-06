#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/eval
          unstable/scribblings/utils
          (for-label racket/base
                     racket/contract
                     racket/list))

@(define our-eval (make-base-eval))

@title{Automata: Compiling State Machines}
@unstable[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

@defmodule[unstable/automata]

This package provides macros and functions for writing state machines over @racketmodname[racket/match] patterns (as opposed to concrete characters.)

@section[#:tag "machine"]{Machines}

@defmodule[unstable/automata/machine]
@(require (for-label unstable/automata/machine))
@interaction-eval[#:eval our-eval (require unstable/automata/machine)]

Each of the subsequent macros compile to instances of the machines provided by this module. This is a documented feature of the modules, so these functions should be used to, for example, determine if the machine is currently accepting.

@defstruct*[machine ([next (any/c . -> . machine?)])]{
 An applicable structure for machines. When the structure is applied, the @racket[next] field is used as the procedure.
}

@defstruct*[(machine-accepting machine) ([next (any/c . -> . machine?)])]{
 A sub-structure of @racket[machine] that is accepting.
}

@defproc[(machine-accepts? [m machine?] [i (listof any/c)])
         boolean?]{
 Returns @racket[#t] if @racket[m] ends in an accepting state after consuming every element of @racket[i].
}

@defproc[(machine-accepts?/prefix-closed [m machine?] [i (listof any/c)])
         boolean?]{
 Returns @racket[#t] if @racket[m] stays in an accepting state during the consumption of every element of @racket[i].
}

@defthing[machine-null machine?]{
 A machine that is never accepting.
}

@defthing[machine-epsilon machine?]{
 A machine that is initially accepting and never accepting afterwards.
}

@defthing[machine-sigma* machine?]{
 A machine that is always accepting.
}

@defproc[(machine-complement [m machine?])
         machine?]{
 A machine that inverts the acception criteria of @racket[m].
}

@defproc[(machine-star [m machine?])
         machine?]{
 A machine that simulates the Kleene star of @racket[m]. @racket[m] may be invoked many times.
}

@defproc[(machine-union [m0 machine?] [m1 machine?])
         machine?]{
 A machine that simulates the union of @racket[m0] and @racket[m1].
}

@defproc[(machine-intersect [m0 machine?] [m1 machine?])
         machine?]{
 A machine that simulates the intersection of @racket[m0] and @racket[m1].
}

@defproc[(machine-seq [m0 machine?] [m1 machine?])
         machine?]{
 A machine that simulates the sequencing of @racket[m0] and @racket[m1]. @racket[m1] may be invoked many times.
}

@defproc[(machine-seq* [m0 machine?] [make-m1 (-> machine?)])
         machine?]{
 A machine that simulates the sequencing of @racket[m0] and @racket[(make-m1)].
 @racket[(make-m1)] may be invoked many times.
}


@section[#:tag "dfa"]{Deterministic Finite Automata}

@defmodule[unstable/automata/dfa]
@(require (for-label unstable/automata/dfa))
@interaction-eval[#:eval our-eval (require unstable/automata/dfa)]

This module provides a macro for deterministic finite automata.

@defform[(dfa start
              (end ...)
              [state ([evt next-state]
                      ...)]
              ...)
         #:contracts 
         ([start identifier?]
          [end identifier?]
          [state identifier?]
          [next-state identifier?])]{
 A @racket[machine] that starts in state @racket[start] where each state behaves as specified in the rules. If a @racket[state] is in @racket[(end ...)], then it is constructed with @racket[machine-accepting]. @racket[next-state] need not be a state from this DFA.
   
   @defexamples[#:eval our-eval
 (define M
   (dfa s1 (s1)
        [s1 ([0 s2]
             [(? even?) s1])]
        [s2 ([0 s1]
             [(? even?) s2])]))
 (machine-accepts? M (list 2 0 4 0 2))
 (machine-accepts? M (list 0 4 0 2 0))
 (machine-accepts? M (list 2 0 2 2 0 8))
 (machine-accepts? M (list 0 2 0 0 10 0))
 (machine-accepts? M (list))
 (machine-accepts? M (list 4 0))]
}

@section[#:tag "nfa"]{Non-Deterministic Finite Automata}

@defmodule[unstable/automata/nfa]
@(require (for-label unstable/automata/nfa))
@interaction-eval[#:eval our-eval (require unstable/automata/nfa)]

This module provides a macro for non-deterministic finite automata.

@defform[(nfa (start:id ...)
              (end:id ...)
              [state:id ([evt:expr (next-state:id ...)]
                         ...)]
              ...)
         #:contracts 
         ([start identifier?]
          [end identifier?]
          [state identifier?]
          [next-state identifier?])]{
 A @racket[machine] that starts in state @racket[(set start ...)] where each state behaves as specified in the rules. If a state is in @racket[(end ...)], then the machine is accepting. @racket[next-state] must be a state from this NFA.
   
 These machines are efficiently compiled to use the smallest possible bit-string as a set representation and unsafe numeric operations where appropriate for inspection and adjusting the sets.
   
   @defexamples[#:eval our-eval
(define M
  (nfa (s1 s3) (s1 s3)
       [s1 ([0 (s2)]
            [1 (s1)])]
       [s2 ([0 (s1)]
            [1 (s2)])]
       [s3 ([0 (s3)]
            [1 (s4)])]
       [s4 ([0 (s4)]
            [1 (s3)])]))
(machine-accepts? M (list 1 0 1 0 1))
(machine-accepts? M (list 0 1 0 1 0))
(machine-accepts? M (list 1 0 1 1 0 1))
(machine-accepts? M (list 0 1 0 0 1 0))
(machine-accepts? M (list))
(machine-accepts? M (list 1 0))]
}

@section[#:tag "nfa-ep"]{Non-Deterministic Finite Automata (with epsilon transitions)}

@defmodule[unstable/automata/nfa-ep]
@(require (for-label unstable/automata/nfa-ep))
@interaction-eval[#:eval our-eval (require unstable/automata/nfa-ep)]

This module provides a macro for non-deterministic finite automata with epsilon transitions.

@defidform[epsilon]{
 A binding for use in epsilon transitions.
}

@defform[#:literals (epsilon)
         (nfa/ep (start:id ...)
                 (end:id ...)
                 [state:id ([epsilon (epsilon-state:id ...)]
                            ...
                            [evt:expr (next-state:id ...)]
                            ...)]
                 ...)
         #:contracts 
         ([start identifier?]
          [end identifier?]
          [state identifier?]
          [epsilon-state identifier?]
          [next-state identifier?])]{
 Extends @racket[nfa] with epsilon transitions, which must be listed first for each state. 
   
   @defexamples[#:eval our-eval
(define M
  (nfa/ep (s0) (s1 s3)
          [s0 ([epsilon (s1)]
               [epsilon (s3)])]
          [s1 ([0 (s2)]
               [1 (s1)])]
          [s2 ([0 (s1)]
               [1 (s2)])]
          [s3 ([0 (s3)]
               [1 (s4)])]
          [s4 ([0 (s4)]
               [1 (s3)])]))
(machine-accepts? M (list 1 0 1 0 1))
(machine-accepts? M (list 0 1 0 1 0))
(machine-accepts? M (list 1 0 1 1 0 1))
(machine-accepts? M (list 0 1 0 0 1 0))
(machine-accepts? M (list))
(machine-accepts? M (list 1 0))]
}

@include-section["re.scrbl"]


@close-eval[our-eval]
