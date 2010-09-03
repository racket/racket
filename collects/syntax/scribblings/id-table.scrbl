#lang scribble/doc
@(require "common.ss"
          (for-label syntax/id-table)
          (for-label scheme/dict))

@title[#:tag "idtable"]{Identifier dictionaries}

@defmodule[syntax/id-table]

This module provides functionality like that of
@schememodname[syntax/boundmap] but with more operations, standard
names, implementation of the @schememodname[scheme/dict] interface,
and immutable (functionally-updating) variants.

@section{Dictionaries for @scheme[bound-identifier=?]}

Bound-identifier tables implement the dictionary interface of
@scheme[scheme/dict]. Consequently, all of the appropriate generic
functions (@scheme[dict-ref], @scheme[dict-map], etc) can be used on
free-identifier tables.

@deftogether[[
@defproc[(make-bound-id-table 
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-bound-id-table?]
@defproc[(make-immutable-bound-id-table
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-bound-id-table?]]]{

Produces a dictionary mapping syntax identifiers to arbitrary
values. The mapping uses @scheme[bound-identifier=?] to compare keys,
but also uses a hash table based on symbol equality to make the
mapping efficient in the common case. The two procedures produce
mutable and immutable dictionaries, respectively.

The identifiers are compared at phase level @scheme[phase]. The
default value is generally appropriate for identifier tables used by
macros, but code that analyzes fully-expanded programs may need to
create identifier tables at multiple different phases.

The optional @scheme[init-dict] argument provides the initial
mappings. It must be a dictionary, and its keys must all be
identifiers. If the @scheme[init-dict] dictionary has multiple
distinct entries whose keys are @scheme[bound-identifier=?], only one
of the entries appears in the new id-table, and it is not specified
which entry is picked.
}

@defproc[(bound-id-table? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] was produced by
@scheme[make-bound-id-table] or
@scheme[make-immutable-bound-id-table], @scheme[#f] otherwise.
}

@deftogether[[
@defproc[(mutable-bound-id-table? [v any/c]) boolean?]
@defproc[(immutable-bound-id-table? [v any/c]) boolean?]
]]{

Predicate for the mutable and immutable variants of bound-identifier
tables, respectively.
}

@defproc[(bound-id-table-ref [table bound-id-table?]
                             [id identifier?]
                             [failure any/c
                              (lambda () (raise (make-exn:fail .....)))])
         any]{

Like @scheme[hash-ref] for bound identifier tables. In particular, if
@scheme[id] is not found, the @scheme[failure] argument is applied if
it is a procedure, or simply returned otherwise.
}

@defproc[(bound-id-table-set! [table mutable-bound-id-table?]
                              [id identifier?]
                              [v any/c])
         void?]{

Like @scheme[hash-set!] for mutable bound-identifier tables.
}

@defproc[(bound-id-table-set [table immutable-bound-id-table?]
                             [id identifier?]
                             [v any/c])
         immutable-bound-id-table?]{

Like @scheme[hash-set] for immutable bound-identifier tables.
}

@defproc[(bound-id-table-remove! [table mutable-bound-id-table?]
                              [id identifier?])
         void?]{

Like @scheme[hash-remove!] for mutable bound-identifier tables.
}

@defproc[(bound-id-table-remove [table immutable-bound-id-table?]
                             [id identifier?]
                             [v any/c])
         immutable-bound-id-table?]{

Like @scheme[hash-remove] for immutable bound-identifier tables.
}

@defproc[(bound-id-table-map [table bound-id-table?]
                             [proc (-> identifier? any/c any)])
         list?]{

Like @scheme[hash-map] for bound-identifier tables.
}

@defproc[(bound-id-table-for-each [table bound-id-table?]
                                  [proc (-> identifier? any/c any)])
         void?]{

Like @scheme[hash-for-each] for bound-identifier tables.
}

@defproc[(bound-id-table-count [table bound-id-table?])
         exact-nonnegative-integer?]{

Like @scheme[hash-count] for bound-identifier tables.

}

@;{
@deftogether[[
@defproc[(bound-id-table-iterate-first [table bound-id-table?])
         id-table-position?]
@defproc[(bound-id-table-iterate-next [table bound-id-table?]
                                      [position id-table-position?])
         id-table-position?]
@defproc[(bound-id-table-iterate-key [table bound-id-table?]
                                     [position id-table-position?])
         identifier?]
@defproc[(bound-id-table-iterate-value [table bound-it-table?]
                                       [position id-table-position?])
         identifier?]]]{

Like the corresponding dictionary procedures from
@schememodname[scheme/dict] for for bound-identifier tables.
}
}

@;{----------}
@section{Dictionaries for @scheme[free-identifier=?]}

Free-identifier tables implement the dictionary interface of
@scheme[scheme/dict]. Consequently, all of the appropriate generic
functions (@scheme[dict-ref], @scheme[dict-map], etc) can be used on
free-identifier tables.

@deftogether[[
@defproc[(make-free-id-table
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         mutable-free-id-table?]
@defproc[(make-immutable-free-id-table 
           [init-dict dict? null]
           [#:phase phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         immutable-free-id-table?]
@defproc[(free-id-table? [v any/c]) boolean?]
@defproc[(mutable-free-id-table? [v any/c]) boolean?]
@defproc[(immutable-free-id-table? [v any/c]) boolean?]
@defproc[(free-id-table-ref [table free-id-table?]
                             [id identifier?]
                             [failure any/c
                              (lambda () (raise (make-exn:fail .....)))])
         any]
@defproc[(free-id-table-set! [table mutable-free-id-table?]
                              [id identifier?]
                              [v any/c])
         void?]
@defproc[(free-id-table-set [table immutable-free-id-table?]
                             [id identifier?]
                             [v any/c])
         immutable-free-id-table?]
@defproc[(free-id-table-remove! [table mutable-free-id-table?]
                              [id identifier?])
         void?]
@defproc[(free-id-table-remove [table immutable-free-id-table?]
                             [id identifier?]
                             [v any/c])
         immutable-free-id-table?]
@defproc[(free-id-table-map [table free-id-table?]
                             [proc (-> identifier? any/c any)])
         list?]
@defproc[(free-id-table-for-each [table free-id-table?]
                                  [proc (-> identifier? any/c any)])
         void?]
@defproc[(free-id-table-count [table free-id-table?])
         exact-nonnegative-integer?]
@;{
@defproc[(free-id-table-iterate-first [table free-id-table?])
         id-table-position?]
@defproc[(free-id-table-iterate-next [table free-id-table?]
                                      [position id-table-position?])
         id-table-position?]
@defproc[(free-id-table-iterate-key [table free-id-table?]
                                     [position id-table-position?])
         identifier?]
@defproc[(free-id-table-iterate-value [table free-it-table?]
                                       [position id-table-position?])
         identifier?]
}]]{

Like the procedures for bound-identifier tables
(@scheme[make-bound-id-table], @scheme[bound-id-table-ref], etc), but
for free-identifier tables, which use @scheme[free-identifier=?] to
compare keys.
}
