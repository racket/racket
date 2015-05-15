#lang scribble/doc
@(require "common.rkt"
          (for-label syntax/id-set
                     racket/set
                     (only-in racket/stream gen:stream)))

@title[#:tag "idset"]{Sets with Identifier Keys}

@defmodule[syntax/id-set]

This module provides @deftech{identifier sets}:
sets with identifier keys that use identifier-specific
comparisons instead of the usual equality operators such as
@racket[eq?] or @racket[equal?].

This module implements two kinds of identifier sets: one via
@racket[free-identifier=?] and one via @racket[bound-identifier=?].
Each are available in both mutable and immutable variants and 
implement the @racket[gen:set],
@racket[gen:stream], 
@racket[prop:sequence], and @racket[gen:equal+hash] 
generic interfaces.

Identifier sets are implemented using @tech{identifier tables},
in the same way that
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash sets}
are implemented with
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash tables}.


@section{Sets for @racket[free-identifier=?]}

A free-identifier set is a set whose keys are compared using
@racket[free-identifier=?]. Free-identifier sets implement the
@racket[gen:set] interface, so all of the
appropriate generic functions (e.g., @racket[set-add], @racket[set-map],
etc) can be used on free-identifier sets.

@deftogether[[
@defproc[(mutable-free-id-set
           [init-set generic-set? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-free-id-set?]
@defproc[(immutable-free-id-set
           [init-set generic-set? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-free-id-set?]]]{

Produces a mutable free-identifier set or immutable free-identifier
set, respectively.  The set uses @racket[free-identifier=?]
to compare keys.

The identifiers are compared at phase level @racket[phase]. The
default phase, @racket[(syntax-local-phase-level)], is generally
appropriate for identifier sets used by macros, but code that
analyzes fully-expanded programs may need to create separate
identifier sets for each phase of the module.

The optional @racket[init-set] argument provides the initial
set elements. It must be a set of identifiers. If the @racket[init-set] 
set has multiple distinct entries whose keys are @racket[free-identifier=?], 
only one of the entries appears in the new id-set, and it is not specified
which entry is picked.
}

@defproc[(free-id-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[mutable-free-id-set] or
@racket[immutable-free-id-set], @racket[#f] otherwise.
}

@defproc[(mutable-free-id-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[mutable-free-id-set], @racket[#f] otherwise.
}

@defproc[(immutable-free-id-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[immutable-free-id-set], @racket[#f] otherwise.
}

@defproc[(free-id-set-empty? [s free-id-set?]) boolean?]{
  Like @racket[set-empty?].}

@defproc[(free-id-set-count [s free-id-set?]) exact-nonnegative-integer?]{
  Like @racket[set-count].}

@defproc[(free-id-set-member? [s free-id-set?] [v identifier?]) boolean?]{
  Like @racket[set-member?].}

@defproc[(free-id-set=? [s1 free-id-set?] [s2 free-id-set?]) boolean?]{
  Like @racket[set=?].}

@defproc[(free-id-set-add [s immutable-free-id-set?] [v identifier?]) 
         immutable-free-id-set?]{
  Like @racket[set-add].}

@defproc[(free-id-set-add! [s mutable-free-id-set?] [v identifier?]) void?]{
  Like @racket[set-add!].}

@defproc[(free-id-set-remove [s immutable-free-id-set?] [v identifier?]) 
         immutable-free-id-set?]{
  Like @racket[set-remove].}

@defproc[(free-id-set-remove! [s mutable-free-id-set?] [v identifier?]) void?]{
  Like @racket[set-remove!].}

@defproc[(free-id-set-first [s free-id-set?]) identifier?]{
  Like @racket[set-first].}

@defproc[(free-id-set-rest [s immutable-free-id-set?]) immutable-free-id-set?]{
  Like @racket[set-rest].}

@defproc[(in-free-id-set [s free-id-set?]) sequence?]{
  Like @racket[in-set].}

@defproc[(free-id-set->stream [s free-id-set?]) stream?]{
  Like @racket[set->stream].}

@defproc[(free-id-set->list [s free-id-set?]) list?]{
  Like @racket[set->list].}

@defproc[(free-id-set-copy [s free-id-set?]) free-id-set?]{
  Like @racket[set-copy].}

@defproc[(free-id-set-copy-clear [s free-id-set?]) free-id-set?]{
  Like @racket[set-copy-clear].}

@defproc[(free-id-set-clear [s immutable-free-id-set?]) immutable-free-id-set?]{
  Like @racket[set-clear].}

@defproc[(free-id-set-clear! [s mutable-free-id-set?]) void?]{
  Like @racket[set-clear!].}

@defproc[(free-id-set-union [s0 immutable-free-id-set?] [s free-id-set?] ...)
         immutable-free-id-set?]{
  Like @racket[set-union].}

@defproc[(free-id-set-union! [s0 mutable-free-id-set?] [s free-id-set?] ...) 
         void?]{
  Like @racket[set-union!].}

@defproc[(free-id-set-intersect 
          [s0 immutable-free-id-set?] [s free-id-set?] ...)
         immutable-free-id-set?]{
  Like @racket[set-intersect].}

@defproc[(free-id-set-intersect! [s0 mutable-free-id-set?] [s free-id-set?] ...) 
         void?]{
  Like @racket[set-intersect!].}

@defproc[(free-id-set-subtract [s0 immutable-free-id-set?] [s free-id-set?] ...)
         immutable-free-id-set?]{
  Like @racket[set-subtract].}

@defproc[(free-id-set-subtract! [s0 mutable-free-id-set?] [s free-id-set?] ...) 
         void?]{
  Like @racket[set-subtract!].}

@defproc[(free-id-set-symmetric-difference 
          [s0 immutable-free-id-set?] [s free-id-set?] ...)
         immutable-free-id-set?]{
  Like @racket[set-symmetric-difference].}

@defproc[(free-id-set-symmetric-difference! 
          [s0 mutable-free-id-set?] [s free-id-set?] ...) void?]{
  Like @racket[set-symmetric-difference!].}

@defproc[(free-id-subset? [s1 free-id-set?] [s2 free-id-set?]) boolean?]{
  Like @racket[subset?].}

@defproc[(free-id-proper-subset? [s1 free-id-set?] [s2 free-id-set?]) boolean?]{
  Like @racket[proper-subset?].}

@defproc[(free-id-set-map [s free-id-set?] [f (-> identifier? any/c)]) list?]{
  Like @racket[set-map].}

@defproc[(free-id-set-for-each [s free-id-set?] [f (-> identifier? any/c)]) void?]{
  Like @racket[set-for-each].}



@defproc[(id-set/c 
          [elem-ctc flat-contract?]
          [#:setidtype idsettype
                       (or/c 'dont-care 'free 'bound) 'dont-care]
          [#:mutability mutability 
                        (or/c 'dont-care 'mutable 'immutable) 'immutable])
         contract?]{
Creates a contract for identifier sets. If
@racket[mutability] is @racket['immutable], the contract accepts only
immutable identifier sets; if @racket[mutability] is @racket['mutable],
the contract accepts only mutable identifier sets.
}

@defproc[(free-id-set/c 
          [elem-ctc flat-contract?]
          [#:mutability mutability 
                        (or/c 'dont-care 'mutable 'immutable) 'immutable])
         contract?]{
Creates a contract for free-identifier sets. If
@racket[mutability] is @racket['immutable], the contract accepts only
immutable identifier sets; if @racket[mutability] is @racket['mutable],
the contract accepts only mutable identifier sets.
}

@;{----------}
@section{Sets for @racket[bound-identifier=?]}

A bound-identifier set is a set whose keys are compared using
@racket[bound-identifier=?]. Bound-identifier sets implement the
@racket[gen:set] interface, so all of the
appropriate generic functions (e.g., @racket[set-add], @racket[set-map],
etc.) can be used on bound-identifier sets.

@deftogether[[
@defproc[(mutable-bound-id-set
           [init-set set? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-bound-id-set?]
@defproc[(immutable-bound-id-set
           [init-set set? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-bound-id-set?]
@defproc[(bound-id-set? [v any/c]) boolean?]
@defproc[(mutable-bound-id-set? [v any/c]) boolean?]
@defproc[(immutable-bound-id-set? [v any/c]) boolean?]
@defproc[(bound-id-set-empty? [s bound-id-set?]) boolean?]
@defproc[(bound-id-set-count [s bound-id-set?]) exact-nonnegative-integer?]
@defproc[(bound-id-set-member? [s bound-id-set?] [v identifier?]) boolean?]
@defproc[(bound-id-set=? [s1 bound-id-set?] [s2 bound-id-set?]) boolean?]
@defproc[(bound-id-set-add [s immutable-bound-id-set?] [v identifier?]) 
         immutable-bound-id-set?]
@defproc[(bound-id-set-add! [s mutable-bound-id-set?] [v identifier?]) void?]
@defproc[(bound-id-set-remove [s immutable-bound-id-set?] [v identifier?]) 
         immutable-bound-id-set?]
@defproc[(bound-id-set-remove! [s mutable-bound-id-set?] [v identifier?]) void?]
@defproc[(bound-id-set-first [s bound-id-set?]) identifier?]
@defproc[(bound-id-set-rest [s immutable-bound-id-set?]) immutable-bound-id-set?]
@defproc[(in-bound-id-set [s bound-id-set?]) sequence?]
@defproc[(bound-id-set->stream [s bound-id-set?]) stream?]
@defproc[(bound-id-set->list [s bound-id-set?]) list?]
@defproc[(bound-id-set-copy [s bound-id-set?]) bound-id-set?]
@defproc[(bound-id-set-copy-clear [s bound-id-set?]) bound-id-set?]
@defproc[(bound-id-set-clear [s immutable-bound-id-set?])
         immutable-bound-id-set?]
@defproc[(bound-id-set-clear! [s mutable-bound-id-set?]) void?]
@defproc[(bound-id-set-union [s0 immutable-bound-id-set?] [s bound-id-set?] ...)
         immutable-bound-id-set?]
@defproc[(bound-id-set-union! [s0 mutable-bound-id-set?] [s bound-id-set?] ...) 
         void?]
@defproc[(bound-id-set-intersect
          [s0 immutable-bound-id-set?] [s bound-id-set?] ...)
         immutable-bound-id-set?]
@defproc[(bound-id-set-intersect! 
          [s0 mutable-bound-id-set?] [s bound-id-set?] ...) void?]
@defproc[(bound-id-set-subtract 
          [s0 immutable-bound-id-set?] [s bound-id-set?] ...) 
         immutable-bound-id-set?]
@defproc[(bound-id-set-subtract! 
          [s0 mutable-bound-id-set?] [s bound-id-set?] ...) void?]
@defproc[(bound-id-set-symmetric-difference 
          [s0 immutable-bound-id-set?] [s bound-id-set?] ...) 
         immutable-bound-id-set?]
@defproc[(bound-id-set-symmetric-difference! 
          [s0 mutable-bound-id-set?] [s bound-id-set?] ...) void?]
@defproc[(bound-id-subset? [s1 bound-id-set?] [s2 bound-id-set?]) boolean?]
@defproc[(bound-id-proper-subset? [s1 bound-id-set?] [s2 bound-id-set?]) 
         boolean?]
@defproc[(bound-id-set-map [s bound-id-set?]) list?]
@defproc[(bound-id-set-for-each [s bound-id-set?]) void?]
@defproc[(bound-id-set/c 
          [elem-ctc flat-contract?]
          [#:mutability mutability 
                        (or/c 'dont-care 'mutable 'immutable) 'immutable])
         contract?]]]{
  Like the procedures for free-identifier sets
  (e.g., @racket[immutable-free-id-set], @racket[free-id-set-add], etc.), but
  for bound-identifier sets, which use @racket[bound-identifier=?] to
  compare keys.
}
