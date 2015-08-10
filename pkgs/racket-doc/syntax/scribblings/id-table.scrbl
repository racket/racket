#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label syntax/id-table racket/dict))

@(define id-table-eval (make-base-eval))
@(id-table-eval '(require (for-syntax racket/base syntax/id-table)))

@title[#:tag "idtable"]{Dictionaries with Identifier Keys}

@defmodule[syntax/id-table]

This module provides two implementations of @deftech{identifier tables}:
dictionaries with identifier keys that use identifier-specific
comparisons instead of @racket[eq?] or @racket[equal?]. Identifier
tables implement the @racketmodname[racket/dict] interface, and they
are available in both mutable and immutable variants.

@section{Dictionaries for @racket[free-identifier=?]}

A free-identifier table is a dictionary whose keys are compared using
@racket[free-identifier=?]. Free-identifier tables implement the
dictionary interface of @racketmodname[racket/dict], so all of the
appropriate generic functions (@racket[dict-ref], @racket[dict-map],
etc) can be used on free-identifier tables.

A caveat for using these tables is that a lookup can fail with
unexpected results if the binding of an identifier changes between
key-value insertion and the lookup.

For example, consider the following use:

@interaction[#:eval id-table-eval
(define-syntax-rule (m)
  (begin
    (begin-for-syntax
      (define table (make-free-id-table))
      (code:comment "set table entry to #t")
      (free-id-table-set! table #'x #t)
      (code:comment "sanity check, it's set to #t")
      (displayln (free-id-table-ref table #'x #f)))

    (define x 'defined-now)

    (begin-for-syntax
      (code:comment "might expect to get #t, but prints #f")
      (displayln (free-id-table-ref table #'x #f)))))

(m)]

The macro @racket[m] expands to code that initializes an identifier table
at compile-time and inserts a key-value pair for @racket[#'x] and
@racket[#t]. The @racket[#'x] identifier has no binding, however, until
the definition @racket[(define x 'defined-now)] is evaluated.

As a result, the lookup at the end of @racket[m] will return @racket[#f]
instead of @racket[#t] because the binding symbol for @racket[#'x] changes
after the initial key-value pair is put into the table. If the definition
is evaluated @emph{before} the initial insertion, both expressions will
print @racket[#t].

@deftogether[[
@defproc[(make-free-id-table 
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-free-id-table?]
@defproc[(make-immutable-free-id-table
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-free-id-table?]]]{

Produces a mutable free-identifier table or immutable free-identifier
table, respectively.  The dictionary uses @racket[free-identifier=?]
to compare keys, but also uses a hash table based on symbol equality
to make the dictionary efficient in the common case.

The identifiers are compared at phase level @racket[phase]. The
default phase, @racket[(syntax-local-phase-level)], is generally
appropriate for identifier tables used by macros, but code that
analyzes fully-expanded programs may need to create separate
identifier tables for each phase of the module.

The optional @racket[init-dict] argument provides the initial
mappings. It must be a dictionary, and its keys must all be
identifiers. If the @racket[init-dict] dictionary has multiple
distinct entries whose keys are @racket[free-identifier=?], only one
of the entries appears in the new id-table, and it is not specified
which entry is picked.
}

@defproc[(free-id-table? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[make-free-id-table] or
@racket[make-immutable-free-id-table], @racket[#f] otherwise.
}

@defproc[(mutable-free-id-table? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[make-free-id-table], @racket[#f] otherwise.
}

@defproc[(immutable-free-id-table? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[make-immutable-free-id-table], @racket[#f] otherwise.
}

@defproc[(free-id-table-ref [table free-id-table?]
                            [id identifier?]
                            [failure any/c
                             (lambda () (raise (make-exn:fail .....)))])
         any]{

Like @racket[hash-ref]. In particular, if @racket[id] is not found,
the @racket[failure] argument is applied if it is a procedure, or
simply returned otherwise.
}

@defproc[(free-id-table-set! [table mutable-free-id-table?]
                             [id identifier?]
                             [v any/c])
         void?]{

Like @racket[hash-set!].
}

@defproc[(free-id-table-set [table immutable-free-id-table?]
                            [id identifier?]
                            [v any/c])
         immutable-free-id-table?]{

Like @racket[hash-set].
}

@defproc[(free-id-table-remove! [table mutable-free-id-table?]
                                [id identifier?])
         void?]{

Like @racket[hash-remove!].
}

@defproc[(free-id-table-remove [table immutable-free-id-table?]
                               [id identifier?])
         immutable-free-id-table?]{

Like @racket[hash-remove].
}

@defproc[(free-id-table-map [table free-id-table?]
                            [proc (-> identifier? any/c any)])
         list?]{

Like @racket[hash-map].
}

@defproc[(free-id-table-for-each [table free-id-table?]
                                 [proc (-> identifier? any/c any)])
         void?]{

Like @racket[hash-for-each].
}

@defproc[(free-id-table-count [table free-id-table?])
         exact-nonnegative-integer?]{

Like @racket[hash-count].
}

@deftogether[[
@defproc[(free-id-table-iterate-first [table free-id-table?])
         id-table-iter?]
@defproc[(free-id-table-iterate-next [table free-id-table?]
                                     [position id-table-iter?])
         id-table-iter?]
@defproc[(free-id-table-iterate-key [table free-id-table?]
                                    [position id-table-iter?])
         identifier?]
@defproc[(free-id-table-iterate-value [table bound-it-table?]
                                      [position id-table-iter?])
         identifier?]]]{

Like @racket[hash-iterate-first], @racket[hash-iterate-next],
@racket[hash-iterate-key], and @racket[hash-iterate-value],
respectively.
}

@defproc[(id-table-iter? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a position in an
identifier table (free or bound, mutable or immutable), @racket[#f]
otherwise.
}

@defproc[(free-id-table/c [key-ctc flat-contract?]
                          [val-ctc chaperone-contract?]
                          [#:immutable immutable? (or/c #t #f 'dont-care) 'dont-care])
         contract?]{

Like @racket[hash/c], but for free-identifier tables. If
@racket[immutable?] is @racket[#t], the contract accepts only
immutable identifier tables; if @racket[immutable?] is @racket[#f],
the contract accepts only mutable identifier tables.
}

@;{----------}
@section{Dictionaries for @racket[bound-identifier=?]}

A bound-identifier table is a dictionary whose keys are compared using
@racket[bound-identifier=?]. Bound-identifier tables implement the
dictionary interface of @racketmodname[racket/dict], so all of the
appropriate generic functions (@racket[dict-ref], @racket[dict-map],
etc) can be used on bound-identifier tables.

@deftogether[[
@defproc[(make-bound-id-table
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-bound-id-table?]
@defproc[(make-immutable-bound-id-table 
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-bound-id-table?]
@defproc[(bound-id-table? [v any/c]) boolean?]
@defproc[(mutable-bound-id-table? [v any/c]) boolean?]
@defproc[(immutable-bound-id-table? [v any/c]) boolean?]
@defproc[(bound-id-table-ref [table bound-id-table?]
                             [id identifier?]
                             [failure any/c
                              (lambda () (raise (make-exn:fail .....)))])
         any]
@defproc[(bound-id-table-set! [table mutable-bound-id-table?]
                              [id identifier?]
                              [v any/c])
         void?]
@defproc[(bound-id-table-set [table immutable-bound-id-table?]
                             [id identifier?]
                             [v any/c])
         immutable-bound-id-table?]
@defproc[(bound-id-table-remove! [table mutable-bound-id-table?]
                                 [id identifier?])
         void?]
@defproc[(bound-id-table-remove [table immutable-bound-id-table?]
                                [id identifier?])
         immutable-bound-id-table?]
@defproc[(bound-id-table-map [table bound-id-table?]
                             [proc (-> identifier? any/c any)])
         list?]
@defproc[(bound-id-table-for-each [table bound-id-table?]
                                  [proc (-> identifier? any/c any)])
         void?]
@defproc[(bound-id-table-count [table bound-id-table?])
         exact-nonnegative-integer?]
@defproc[(bound-id-table-iterate-first [table bound-id-table?])
         id-table-position?]
@defproc[(bound-id-table-iterate-next [table bound-id-table?]
                                      [position id-table-position?])
         id-table-position?]
@defproc[(bound-id-table-iterate-key [table bound-id-table?]
                                     [position id-table-position?])
         identifier?]
@defproc[(bound-id-table-iterate-value [table bound-id-table?]
                                       [position id-table-position?])
         identifier?]
@defproc[(bound-id-table/c [key-ctc flat-contract?]
                           [val-ctc chaperone-contract?]
                           [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care])
         contract?]
]]{

Like the procedures for free-identifier tables
(@racket[make-free-id-table], @racket[free-id-table-ref], etc), but
for bound-identifier tables, which use @racket[bound-identifier=?] to
compare keys.
}

@close-eval[id-table-eval]