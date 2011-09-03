#lang scribble/doc
@(require "common.rkt" (for-label syntax/struct))

@title[#:tag "struct"]{Expanding @racket[define-struct]-like Forms}

@defmodule[syntax/struct]

@defproc[(parse-define-struct [stx syntax?] [orig-stx syntax?]) 
         (values identifier?
                 (or/c identifier? false/c)
                 (listof identifier?)
                 syntax?)]{

Parses @racket[stx] as a @racket[define-struct] form, but uses
@racket[orig-stx] to report syntax errors (under the assumption that
@racket[orig-stx] is the same as @racket[stx], or that they at least share
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

Generates the names bound by @racket[define-struct] given an
identifier for the struct type name and a list of identifiers for the
field names. The result is a list of identifiers:

@itemize[
  @item{@racketidfont{struct:}@racket[name-id]}
  @item{@racket[ctr-name], or @racketidfont{make-}@racket[name-id] if @racket[ctr-name] is @racket[#f]}
  @item{@racket[name-id]@racketidfont{?}}
  @item{@racket[name-id]@racketidfont{-}@racket[_field], for each
        @racket[_field] in @racket[field-ids].}
  @item{@racketidfont{set-}@racket[name-id]@racketidfont{-}@racket[_field]@racketidfont{!}
        (getter and setter names alternate).}
  @item{....}]

If @racket[omit-sel?] is true, then the selector names are omitted from the
result list. If @racket[omit-set?] is true, then the setter names are omitted
from the result list.

The default @racket[src-stx] is @racket[#f]; it is used to provide a
source location to the generated identifiers.}

@defproc[(build-struct-generation [name-id identifier?]
                                  [field-ids (listof identifier?)]

                                  [#:constructor-name ctr-name (or/c identifier? #f) #f]
                                  [omit-sel? boolean?]
                                  [omit-set? boolean?]

                                  [super-type any/c #f]
                                  [prop-value-list list? '(list)]
                                  [immutable-k-list list? '(list)])
         (listof identifier?)]{

Takes the same arguments as @racket[build-struct-names] and generates
an S-expression for code using @racket[make-struct-type] to generate
the structure type and return values for the identifiers created by
@racket[build-struct-names].  The optional @racket[super-type],
@racket[prop-value-list], and @racket[immutable-k-list] parameters take
S-expressions that are used as the corresponding argument expressions to
@racket[make-struct-type].}


@defproc[(build-struct-generation* [all-name-ids (listof identifier?)]
				   [name-id identifier?]
				   [field-ids (listof identifier?)]
                                   [#:constructor-name ctr-name (or/c identifier? #f) #f]
				   [omit-sel? boolean?]
				   [omit-set? boolean?]
				   [super-type any/c #f]
				   [prop-value-list list? '(list)]
				   [immutable-k-list list? '(list)])
	 (listof identifier?)]{

Like @racket[build-struct-generation], but given the names produced by
@racket[build-struct-names], instead of re-generating them.}

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

Takes mostly the same arguments as @racket[build-struct-names], plus a parent
identifier/@racket[#t]/@racket[#f] and a list of accessor and mutator
identifiers (possibly ending in @racket[#f]) for a parent type, and
generates an S-expression for expansion-time code to be used in the
binding for the structure name. 

If @racket[no-ctr?] is true, then the constructor name is omitted from
the expansion-time information. Similarly, if @racket[no-type?] is
true, then the structure-type name is omitted.

A @racket[#t] for the @racket[base-name] means no super-type,
@racket[#f] means that the super-type (if any) is unknown, and an
identifier indicates the super-type identifier.}


@defproc[(struct-declaration-info? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[x] has the shape of expansion-time
information for structure type declarations, @racket[#f] otherwise.
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

This procedure implements the core of a @racket[define-struct]
expansion.

The @racket[generate-struct-declaration] procedure is called by a
macro expander to generate the expansion, where the @racket[name-id],
@racket[super-id-or-false], and @racket[field-id-list] arguments
provide the main parameters. The @racket[current-context] argument is
normally the result of @racket[syntax-local-context]. The
@racket[orig-stx] argument is used for syntax errors. The optional
@racket[omit-sel?] and @racket[omit-set?]  arguments default to
@racket[#f]; a @racket[#t] value suppresses definitions of field
selectors or mutators, respectively.

The @racket[make-struct-type] procedure is called to generate the
expression to actually create the struct type. Its arguments are
@racket[orig-stx], @racket[name-id-stx], @racket[defined-name-stxes],
and @racket[super-info].  The first two are as provided originally to
@racket[generate-struct-declaration], the third is the set of names
generated by @racket[build-struct-names], and the last is super-struct
info obtained by resolving @racket[super-id-or-false] when it is not
@racket[#f], @racket[#f] otherwise.

The result should be an expression whose values are the same as the
result of @racket[make-struct-type]. Thus, the following is a basic
@racket[make-make-struct-type]:

@RACKETBLOCK[
      (lambda (orig-stx name-stx defined-name-stxes super-info)
        #`(make-struct-type '#,name-stx
                             #,(and super-info (list-ref super-info 0))
                             #,(/ (- (length defined-name-stxes) 3) 2)
                             0 #f))]

but an actual @racket[make-make-struct-type] will likely do more.}
