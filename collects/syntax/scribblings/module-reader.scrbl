#lang scribble/doc
@(require "common.ss")

@(define-syntax-rule (go)
  (begin
    (require (for-label syntax/module-reader))
@begin{
@title[#:tag "module-reader"]{Module Reader}

@defmodule[syntax/module-reader]

The @schememodname[syntax/module-reader] language provides support
for defining @hash-lang[] readers.

@defform[(#%module-begin module-path)]{

Causes a module written in the @schememodname[syntax/module-reader]
language to define and provide @schemeidfont{read} and
@schemeidfont{read-syntax} functions, making the module an
implementation of a reader. In particular, the exported reader
functions read all S-expressions until an end-of-file, and it packages
them into a new module in the @scheme[module-path] language.

That is, a module @scheme[_something]@scheme[/lang/reader] implemented
as

@schemeblock[
(module reader module-syntax/module-reader
  module-path)
]

creates a reader that converts @scheme[#, @hash-lang[] _something]
into

@schemeblock[
(module _name-id module-path
  ....)
]

where @scheme[_name-id] is derived from the name of the port used by
the reader.

For example, @scheme[scheme/base/lang/reader] is implemented as

@schemeblock[
(module reader module-syntax/module-reader
  scheme/base)
]}

@defproc[(wrap-read-all [mod-path module-path?]
                        [in input-port?]
                        [read (input-port . -> . any/c)]
                        [mod-path-stx syntax?]
                        [src (or/c syntax? #f)]
                        [line number?]
                        [col number?]
                        [pos number?])
         any/c]{

Repeatedly calls @scheme[read] on @scheme[in] until an end of file,
collecting the results in order into @scheme[_lst], and derives a
@scheme[_name-id] from @scheme[(object-name in)].  The last five 
arguments are used to construct the syntax object for the language
position of the module.  The result is roughly

@schemeblock[
`(module ,_name-id ,mod-path ,@_lst)
]}

}))
@(go)
