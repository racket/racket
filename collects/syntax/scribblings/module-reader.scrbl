#lang scribble/doc
@(require "common.ss")

@(require (for-label syntax/module-reader
                     (only-in scribble/reader read-syntax-inside read-inside)))

@title[#:tag "module-reader"]{Module Reader}

@defmodule[syntax/module-reader]

The @schememodname[syntax/module-reader] language provides support for
defining @hash-lang[] readers.  In its simplest form, the only thing
that is needed in the body of a @schememodname[syntax/module-reader]
is the name of the module that will be used in the language position
of read modules; using keywords, the resulting readers can be
customized in a number of ways.

@defform*/subs[[(#%module-begin module-path)
                (#%module-begin module-path reader-option ... body ....)]
               ([reader-option (code:line #:read        read-expr)
                               (code:line #:read-syntax read-syntax-expr)
                               (code:line #:wrapper1    wrapper1-expr)
                               (code:line #:wrapper2    wrapper2-expr)
                               (code:line #:whole-body-readers? whole?-expr)])]{

Causes a module written in the @schememodname[syntax/module-reader]
language to define and provide @schemeidfont{read} and
@schemeidfont{read-syntax} functions, making the module an
implementation of a reader. In particular, the exported reader
functions read all S-expressions until an end-of-file, and package
them into a new module in the @scheme[module-path] language.

That is, a module @scheme[_something]@scheme[/lang/reader] implemented
as

@schemeblock[
(module reader syntax/module-reader
  module-path)
]

creates a reader that converts @scheme[#,(hash-lang)_something]
into

@schemeblock[
(module _name-id module-path
  ....)
]

where @scheme[_name-id] is derived from the name of the port used by
the reader.

For example, @scheme[scheme/base/lang/reader] is implemented as

@schemeblock[
(module reader syntax/module-reader
  scheme/base)
]

The reader functions can be customized in a number of ways, using
keyword markers in the syntax of the reader module.  A @scheme[#:read]
and @scheme[#:read-syntax] keywords can be used to specify functions
other than @scheme[read] and @scheme[read-syntax] to perform the
reading.  For example, you can implement a
@secref[#:doc '(lib "scribblings/honu/honu.scrbl")]{Honu} reader
using:

@schemeblock[
(module reader syntax/module-reader
  honu
  #:read read-honu
  #:read-syntax read-honu-syntax)
]

You can also use the (optional) module body to provide more
definitions that might be needed to implement your reader functions.
For example, here is a case-insensitive reader for the
@scheme[scheme/base] language:

@schemeblock[
(module insensitive syntax/module-reader
  scheme/base
  #:read (wrap read) #:read-syntax (wrap read-syntax)
  (define ((wrap reader) . args)
    (parameterize ([read-case-sensitive #f]) (apply reader args))))
]

In many cases, however, the standard @scheme[read] and
@scheme[read-syntax] are fine, as long as you can customize the
dynamic context they're invoked at.  For this, @scheme[#:wrapper1] can
specify a function that can control the dynamic context in which the
reader functions are called.  It should evaluate to a function that
consumes a thunk and invokes it in the right context.  Here is an
alternative definition of the case-insensitive language using
@scheme[#:wrapper1]:

@schemeblock[
(module insensitive syntax/module-reader
  scheme/base
  #:wrapper1 (lambda (t)
               (parameterize ([read-case-sensitive #f])
                 (t))))
]

Note that using a @tech[#:doc refman]{readtable}, you can implement
languages that go beyond plain S-expressions.

In addition to this wrapper, there is also @scheme[#:wrapper2] that
has more control over the resulting reader functions.  If specified,
this wrapper is handed the input port and a (one-argumet) reader
function that expects the input port as an argument.  This allows this
wrapper to hand a different port value to the reader function, for
example, it can divert the read to use different file (if given a port
that corresponds to a file).  Here is the case-insensitive implemented
using this option:

@schemeblock[
(module insensitive syntax/module-reader
  scheme/base
  #:wrapper2 (lambda (in r)
               (parameterize ([read-case-sensitive #f])
                 (r in))))
]

In some cases, the reader functions read the whole file, so there is
no need to iterate them (e.g., @scheme[read-inside] and
@scheme[read-syntax-inside]).  In these cases you can specify
@scheme[#:whole-body-readers?] as @scheme[#t] --- the readers are
expected to return a list of expressions in this case.

Finally, note that the two wrappers can return a different value than
the wrapped function.  This introduces two more customization points
for the resulting readers:
@itemize{
  @item{The thunk that is passed to a @scheme[#:wrapper1] function
    reads the file contents and returns a list of read expressions
    (either syntax values or S-expressions).  For example, the
    following reader defines a ``language'' that ignores the contents
    of the file, and simply reads files as if they were empty:
    @schemeblock[
    (module ignored syntax/module-reader
      scheme/base
      #:wrapper1 (lambda (t) (t) '()))
    ]
    Note that it is still performing the read, otherwise the module
    loader will complain about extra expressions.}
  @item{The reader function that is passed to a @scheme[#:wrapper2]
    function returns the final reault of the reader (a module
    expression).  You can return a different value, for example,
    making it use a different language module.}}
In some rare cases, it is more convenient to know whether a reader is
invoked for a @scheme[read] or for a @scheme[read-syntax].  To
accommodate these cases, both wrappers can accept an additional
argument, and in this case, they will be handed a boolean value that
indicates whether the reader is expected to read syntax (@scheme[#t])
or not (@scheme[#f]).  For example, here is a reader that uses the
scribble syntax, and the first datum in the file determines the actual
language (which means that the library specification is effectively
ignored):
@schemeblock[
(module scribbled syntax/module-reader
  -ignored-
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([lang (read in)]
           [mod  (parameterize ([current-readtable (make-at-readtable)])
                   (rd in))]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (syntax-case mod ()
                [(module name lang* . body)
                 (with-syntax ([lang (datum->syntax
                                      #'lang* lang #'lang*)])
                   (syntax/loc mod (module name lang . body)))])])
      (if stx? r (syntax->datum r))))
  (require scribble/reader))
]
}

@defproc[(wrap-read-all [mod-path module-path?]
                        [in input-port?]
                        [read (input-port . -> . any/c)]
                        [mod-path-stx syntax?]
                        [src (or/c syntax? #f)]
                        [line number?]
                        [col number?]
                        [pos number?])
         any/c]{

[Note: this function is deprecated;
@schememodname[syntax/module-reader] can be adapted using the various
keywords to arbitrary readers, please use it instead.]

Repeatedly calls @scheme[read] on @scheme[in] until an end of file,
collecting the results in order into @scheme[_lst], and derives a
@scheme[_name-id] from @scheme[(object-name in)].  The last five 
arguments are used to construct the syntax object for the language
position of the module.  The result is roughly

@schemeblock[
`(module ,_name-id ,mod-path ,@_lst)
]}
