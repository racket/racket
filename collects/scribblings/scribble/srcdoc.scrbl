#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/srcdoc
                     scribble/extract
                     scheme/contract))

@title[#:tag "srcdoc"]{In-Source Documentation}

The @schememodname[scribble/srcdoc] and
@schememodname[scribble/extract] libraries support writing
documentation withing the documentation code along with an export
contract, similar to using @as-index{JavaDoc}. With this approach, a
single contract specification is used both for the run-time contract
and the documentation of an exported binding.

The @schememodname[scribble/srcdoc] library provides forms for
exporting a binding with associated documentation. The
@scheme[scribble/extract] library is used to pull
@scheme[scribble/srcdoc]-based documentation into a Scribble document
(perhaps for multiple libraries).

Although documentation is written with a library's implementation when
using @schememodname[scribble/srcdoc], the documentation creates no
run-time overhead for the library. Similarly, typesetting the
documentation does not require running the library. The two phases
(run time versus documentation time) are kept separate in much the
same way that the module system keeps expansion-time code separate
from run-time code.

For an example use, see the @filepath{file} collection's
@filepath{gif.ss} source file and the corresponding extraction in
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

Like @scheme[provide] or @scheme[provide/contract], but each
@scheme[spec] uses a @deftech{documentation transformer} to describe
the exported identifier and its contract.

The currently supported @tech{documentation transformers} are
@scheme[proc-doc], @scheme[proc-doc/names], @scheme[parameter-doc],
and @scheme[thing-doc].}


@defform[(require/doc require-spec ...)]{

Like @scheme[require], but for bindings that are needed at
documentation time (and documentation-expansion time, etc.) instead of
run time (and expansion time, etc.). A @scheme[require-doc] form has
no effect on a normal use of the library; it affects only
documentation extraction.

Typically, a library that uses @schememodname[scribble/srcdoc]
includes at least @scheme[(require/doc scribble/base scribble/manual)]
to get core Scheme forms and basic Scribble functions to use in
documentation expressions.}

@defform*/subs[#:literals (-> ->* case->)
               [(proc-doc/names id contract ((arg-id ...) ((arg-id default-expr) ...)) desc-expr)
                (proc-doc/names id case-contract ((arg-id ...) ((arg-id default-expr) ...)) desc-expr)]
               ([contract (-> arg ... result)
                          (->* (mandatory ...) (optional ...) result)]
                [mandatory contract-expr
                           (code:line keyword contract-expr)]
                [optional contract-expr
                          (code:line keyword contract-expr)]
                [case-contract (case-> (-> arg ... result) ...)])]{
                          
When used in @scheme[provide/doc], exports @scheme[id] with the
contract described by @scheme[contract] or @scheme[case-contract],
just like using @scheme[provide/contract].

The @scheme[arg-id]s specify the names of arguments, which are not
normally written as part of a contract. They are combined with the
contract expression to generate the description of the binding in the
documentation via @scheme[defproc]. The @scheme[(arg-id default-expr)]
pairs specify the names and default values of the optional arguments.

The @scheme[desc-expr] is a documentation-time expression that
produces prose to describe the exported binding---that is, the last
part of the generated @scheme[defproc], so the description can refer
to the @scheme[arg-id]s using @scheme[scheme].

The normal @scheme[require]s of the enclosing library are effectively
converted into @scheme[for-label] @scheme[require]s when generating
documentation, so that identifiers in the @scheme[contract]s are
linked to their corresponding documentation. Similarly, any binding
that is available in the run-time phase of of the enclosing library
can be referenced in documentation prose using the @scheme[scheme]
form.}

@defform/subs[#:literals (-> ->d values)
              (proc-doc id contract desc-expr)
              ([contract (-> result)
                         (->d (arg ...) () (values [id result] ...))
                         (->d (arg ...) () #:pre-cond expression (values [id result] ...))
                         (->d (arg ...) () [id result])
                         (->d (arg ...) () #:pre-cond expression [id result])
                         (->d (arg ...) () #:rest id rest [id result])])]{

Like @scheme[proc-doc], but supporting contract forms that embed
argument names. Only a subset of @scheme[->d] forms are currently
supported.}
                          
@defform[(thing-doc id contract-expr dec-expr)]{

Like @scheme[proc-doc], but for an export of an arbitrary value.}

@defform[#:literals (parameter/c)
         (parameter-doc id (parameter/c contract-expr) arg-id desc-expr)]{

Like @scheme[proc-doc], but for exporting a parameter.}

@; ----------------------------------------

@section{Extracting Documentation from Source}

@defmodule[scribble/extract]

@defform[(include-extracted module-path)]{

Expands to a sequence of documentation forms extracted from
@scheme[module-path], which is expected to be a module that uses
@schememodname[scribble/srcdoc].}

@defform[(provide-extracted module-path)]{

Similar to @scheme[include-extracted], but the documentation is
packaged and exported as @scheme[exported], instead of left
inline.

Use this form in combination with
@scheme[include-previously-extracted] when documentation from a single
source is to be split and typeset among multiple documentation
locations. The @scheme[provide-extracted] form extracts the
documentation once, and then @scheme[include-previously-extracted]
form extracts documentation for specific bindings as needed.}

@defform[(include-previously-extracted module-path regexp)]{

Similar to @scheme[include-extracted], but instead of referring to the
source that contains its own documentation, @scheme[module-path]
refers to a module that uses @scheme[provide-extracted]. The
@scheme[include-previously-extracted] form expands to documentation
forms for all identifiers whose string forms match @scheme[regexp].}
