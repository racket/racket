#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/for))

@mzlib[#:mode title for]

@deprecated[@racketmodname[racket/base]]{}

The @racketmodname[mzlib/for] library re-exports from
@racketmodname[scheme/base]:

@racketblock[
for/fold for*/fold
for for*
for/list for*/list
for/lists for*/lists
for/and for*/and
for/or for*/or
for/first for*/first
for/last for*/last

for/fold/derived for*/fold/derived

in-range
in-naturals
in-list
in-vector
in-string
in-bytes
in-input-port-bytes
in-input-port-chars
in-hash-table
in-hash-table-keys
in-hash-table-values
in-hash-table-pairs

in-parallel
stop-before
stop-after
in-indexed

sequence?
sequence-generate

define-sequence-syntax
make-do-sequence
:do-in]
