#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label scribble/srcdoc scribble/extract racket/contract))

@title[#:tag "srcdoc"]{In-Source Documentation}

The @racketmodname[scribble/srcdoc] and
@racketmodname[scribble/extract] libraries support writing
documentation within the documentation code along with an export
contract, similar to using @as-index{JavaDoc}. With this approach, a
single contract specification is used both for the run-time contract
and the documentation of an exported binding.

The @racketmodname[scribble/srcdoc] library provides forms for
exporting a binding with associated documentation. The
@racket[scribble/extract] library is used to pull
@racket[scribble/srcdoc]-based documentation into a Scribble document
(perhaps for multiple libraries).

Although documentation is written with a library's implementation when
using @racketmodname[scribble/srcdoc], the documentation creates no
run-time overhead for the library. Similarly, typesetting the
documentation does not require running the library. The two phases
(run time versus documentation time) are kept separate in much the
same way that the module system keeps expansion-time code separate
from run-time code.

For an example use, see the @filepath{file} collection's
@filepath{gif.rkt} source file and the corresponding extraction in
@filepath{scribblings/gif.scrbl}. As that example illustrates,
prefixing the module declaration with

@verbatim[#:indent 2]{
  #reader scribble/reader
}

enables the @"@"-reader, which is handy for writing documentation
expressions.

@; ----------------------------------------

@section{Source Annotations for Documentation}

@defmodule[scribble/srcdoc]

@defform[(provide/doc spec ...)]{

Like @racket[provide] or @racket[provide/contract], but each
@racket[spec] uses a @deftech{documentation transformer} to describe
the exported identifier and its contract.

The currently supported @tech{documentation transformers} are
@racket[proc-doc], @racket[proc-doc/names], @racket[parameter-doc],
and @racket[thing-doc].}


@defform[(require/doc require-spec ...)]{

Like @racket[require], but for bindings that are needed at
documentation time (and documentation-expansion time, etc.) instead of
run time (and expansion time, etc.). A @racket[require-doc] form has
no effect on a normal use of the library; it affects only
documentation extraction.

Typically, a library that uses @racketmodname[scribble/srcdoc]
includes at least @racket[(require/doc scribble/base scribble/manual)]
to get core Racket forms and basic Scribble functions to use in
documentation expressions.}

@defform*/subs[#:literals (-> ->* case->)
               [(proc-doc/names id contract ((arg-id ...) ((arg-id default-expr) ...))
                                desc-expr)]
               ([contract (-> arg ... result)
                          (->* (mandatory ...) (optional ...) result)
                          (case-> (-> arg ... result) ...)]
                [mandatory contract-expr
                           (code:line keyword contract-expr)]
                [optional contract-expr
                          (code:line keyword contract-expr)])]{

When used in @racket[provide/doc], exports @racket[id] with the
contract described by @racket[contract]
just like using @racket[provide/contract].

The @racket[arg-id]s specify the names of arguments, which are not
normally written as part of a contract. They are combined with the
contract expression to generate the description of the binding in the
documentation via @racket[defproc]. The @racket[(arg-id default-expr)]
pairs specify the names and default values of the optional arguments.

The @racket[desc-expr] is a documentation-time expression that
produces prose to describe the exported binding---that is, the last
part of the generated @racket[defproc], so the description can refer
to the @racket[arg-id]s using @racket[racket].

The normal @racket[require]s of the enclosing library are effectively
converted into @racket[for-label] @racket[require]s when generating
documentation, so that identifiers in the @racket[contract]s are
linked to their corresponding documentation. Similarly, any binding
that is available in the run-time phase of of the enclosing library
can be referenced in documentation prose using the @racket[racket]
form.}

@defform/subs[#:literals (-> ->i ->d values)
              (proc-doc id contract desc-expr)
              ([contract (-> result)
                         (->i (arg ...) () (values ress ...))
                         (->i (arg ...) () #:pre (pre-id ...) condition (values ress ...))
                         (->i (arg ...) () res)
                         (->i (arg ...) () #:pre (pre-id ...) condition [name res])
                         (->i (arg ...) () #:rest rest res)

                         (->d (arg ...) () (values [id result] ...))
                         (->d (arg ...) () #:pre-cond expr (values [id result] ...))
                         (->d (arg ...) () [id result])
                         (->d (arg ...) () #:pre-cond expr [id result])
                         (->d (arg ...) () #:rest id rest [id result])])]{

Like @racket[proc-doc], but supporting contract forms that embed
argument names. Only a subset of @racket[->i] and @racket[->d] forms are
currently supported.}

@defform[(thing-doc id contract-expr dec-expr)]{

Like @racket[proc-doc], but for an export of an arbitrary value.}

@defform[#:literals (parameter/c)
         (parameter-doc id (parameter/c contract-expr) arg-id desc-expr)]{

Like @racket[proc-doc], but for exporting a parameter.}

@; ----------------------------------------

@section{Extracting Documentation from Source}

@defmodule[scribble/extract]

@defform[(include-extracted module-path)]{

Expands to a sequence of documentation forms extracted from
@racket[module-path], which is expected to be a module that uses
@racketmodname[scribble/srcdoc].}

@defform[(provide-extracted module-path)]{

Similar to @racket[include-extracted], but the documentation is
packaged and exported as @racket[exported], instead of left
inline.

Use this form in combination with
@racket[include-previously-extracted] when documentation from a single
source is to be split and typeset among multiple documentation
locations. The @racket[provide-extracted] form extracts the
documentation once, and then @racket[include-previously-extracted]
form extracts documentation for specific bindings as needed.}

@defform[(include-previously-extracted module-path regexp)]{

Similar to @racket[include-extracted], but instead of referring to the
source that contains its own documentation, @racket[module-path]
refers to a module that uses @racket[provide-extracted]. The
@racket[include-previously-extracted] form expands to documentation
forms for all identifiers whose string forms match @racket[regexp].}
