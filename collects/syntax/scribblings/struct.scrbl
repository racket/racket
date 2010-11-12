#lang scribble/doc
@(require "common.ss"
          (for-label syntax/struct))

@title[#:tag "struct"]{Expanding @scheme[define-struct]-like Forms}

@defmodule[syntax/struct]

@defproc[(parse-define-struct [stx syntax?] [orig-stx syntax?]) 
         (values identifier?
                 (or/c identifier? false/c)
                 (listof identifier?)
                 syntax?)]{

Parses @scheme[stx] as a @scheme[define-struct] form, but uses
@scheme[orig-stx] to report syntax errors (under the assumption that
@scheme[orig-stx] is the same as @scheme[stx], or that they at least share
sub-forms). The result is four values: an identifier for the struct
type name, a identifier or #f for the super-name, a list of
identifiers for fields, and a syntax object for the inspector
expression.}

@defproc[(build-struct-names [name-id identifier?]
			     [field-ids (listof identifier?)]
                             [#:constructor-name ctr-name (or/c identifier? #f) #f]
			     [omit-sel? boolean?]
			     [omit-set? boolean?]
			     [src-stx (or/c syntax? false/c) #f])
          (listof identifier?)]{

Generates the names bound by @scheme[define-struct] given an
identifier for the struct type name and a list of identifiers for the
field names. The result is a list of identifiers:

@itemize[
  @item{@schemeidfont{struct:}@scheme[name-id]}
  @item{@scheme[ctr-name], or @schemeidfont{make-}@scheme[name-id] if @racket[ctr-name] is @racket[#f]}
  @item{@scheme[name-id]@schemeidfont{?}}
  @item{@scheme[name-id]@schemeidfont{-}@scheme[_field], for each
        @scheme[_field] in @scheme[field-ids].}
  @item{@schemeidfont{set-}@scheme[name-id]@schemeidfont{-}@scheme[_field]@schemeidfont{!}
        (getter and setter names alternate).}
  @item{....}]

If @scheme[omit-sel?] is true, then the selector names are omitted from the
result list. If @scheme[omit-set?] is true, then the setter names are omitted
from the result list.

The default @scheme[src-stx] is @scheme[#f]; it is used to provide a
source location to the generated identifiers.}

@defproc[(build-struct-generation [name-id identifier?]
                                  [field-ids (listof identifier?)]

                                  [#:constructor-name ctr-name (or/c identifier? #f) #f]
                                  [omit-sel? boolean?]
                                  [omit-set? boolean?]

                                  [super-type any/c #f]
                                  [prop-value-list list? empty]
                                  [immutable-k-list list? empty])
         (listof identifier?)]{

Takes the same arguments as @scheme[build-struct-names] and generates
an S-expression for code using @scheme[make-struct-type] to generate
the structure type and return values for the identifiers created by
@scheme[build-struct-names].  The optional @scheme[super-type],
@scheme[prop-value-list], and @scheme[immutable-k-list] parameters take
S-expression values that are used as the corresponding arguments to
@scheme[make-struct-type].}


@defproc[(build-struct-generation* [all-name-ids (listof identifier?)]
				   [name-id identifier?]
				   [field-ids (listof identifier?)]
                                   [#:constructor-name ctr-name (or/c identifier? #f) #f]
				   [omit-sel? boolean?]
				   [omit-set? boolean?]
				   [super-type any/c #f]
				   [prop-value-list list? empty]
				   [immutable-k-list list? empty])
	 (listof identifier?)]{

Like @scheme[build-struct-generation], but given the names produced by
@scheme[build-struct-names], instead of re-generating them.}

@defproc[(build-struct-expand-info [name-id identifier?]
                                   [field-ids (listof identifier?)]
                                   [#:omit-constructor? no-ctr? any/c #f]
                                   [#:constructor-name ctr-name (or/c identifier? #f) #f]
                                   [#:omit-struct-type? no-type? any/c #f]
                                   [omit-sel? boolean?]
                                   [omit-set? boolean?]
                                   [base-name (or/c identifier? boolean?)]
                                   [base-getters (listof (or/c identifier? false/c))]
                                   [base-setters (listof (or/c identifier? false/c))])
	 any]{

Takes mostly the same arguments as @scheme[build-struct-names], plus a parent
identifier/@scheme[#t]/@scheme[#f] and a list of accessor and mutator
identifiers (possibly ending in @scheme[#f]) for a parent type, and
generates an S-expression for expansion-time code to be used in the
binding for the structure name. 

If @racket[no-ctr?] is true, then the constructor name is omitted from
the expansion-time information. Similarly, if @racket[no-type?] is
true, then the structure-type name is omitted.

A @scheme[#t] for the @scheme[base-name] means no super-type,
@scheme[#f] means that the super-type (if any) is unknown, and an
identifier indicates the super-type identifier.}


@defproc[(struct-declaration-info? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[x] has the shape of expansion-time
information for structure type declarations, @scheme[#f] otherwise.
See @secref[#:doc refman]{structinfo}.}


@defproc[(generate-struct-declaration [orig-stx syntax?] 
                                      [name-id identifier?]
                                      [super-id-or-false (or/c identifier? false/c)]
                                      [field-id-list (listof identifier?)]
                                      [current-context any/c]
				      [make-make-struct-type procedure?]
				      [omit-sel? boolean? #f]
				      [omit-set? boolean? #f])
	 syntax?]{

This procedure implements the core of a @scheme[define-struct]
expansion.

The @scheme[generate-struct-declaration] procedure is called by a
macro expander to generate the expansion, where the @scheme[name-id],
@scheme[super-id-or-false], and @scheme[field-id-list] arguments
provide the main parameters. The @scheme[current-context] argument is
normally the result of @scheme[syntax-local-context]. The
@scheme[orig-stx] argument is used for syntax errors. The optional
@scheme[omit-sel?] and @scheme[omit-set?]  arguments default to
@scheme[#f]; a @scheme[#t] value suppresses definitions of field
selectors or mutators, respectively.

The @scheme[make-struct-type] procedure is called to generate the
expression to actually create the struct type. Its arguments are
@scheme[orig-stx], @scheme[name-id-stx], @scheme[defined-name-stxes],
and @scheme[super-info].  The first two are as provided originally to
@scheme[generate-struct-declaration], the third is the set of names
generated by @scheme[build-struct-names], and the last is super-struct
info obtained by resolving @scheme[super-id-or-false] when it is not
@scheme[#f], @scheme[#f] otherwise.

The result should be an expression whose values are the same as the
result of @scheme[make-struct-type]. Thus, the following is a basic
@scheme[make-make-struct-type]:

@SCHEMEBLOCK[
      (lambda (orig-stx name-stx defined-name-stxes super-info)
	#`(make-struct-type '#,name-stx 
			     #,(and super-info (list-ref super-info 0))
			     #,(/ (- (length defined-name-stxes) 3) 2)
			     0 #f))]

but an actual @scheme[make-make-struct-type] will likely do more.}
