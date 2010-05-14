#lang scribble/doc
@(require "common.ss")

@(require (for-label syntax/module-reader
                     (only-in scribble/reader
                              read-syntax-inside read-inside)))

@(begin
  (define-syntax-rule (define-mb name)
    (begin
     (require (for-label scheme/base))
     (define name @scheme[#%module-begin])))
  (define-mb scheme-#%module-begin))

@(define guide-doc '(lib "scribblings/guide/guide.scrbl"))

@title[#:tag "module-reader"]{Module Reader}

@margin-note{See also @secref[#:doc guide-doc "hash-languages"] in
             @other-manual[guide-doc].}

@defmodule[syntax/module-reader]

The @schememodname[syntax/module-reader] library provides support for
defining @hash-lang[] readers. It is normally used as a module
language, though it may also be @scheme[require]d to get
@scheme[make-meta-reader]. It provides all of the bindings of
@scheme[scheme/base] other than @|scheme-#%module-begin|.

@defform*/subs[
  [(#%module-begin module-path)
   (#%module-begin module-path reader-option ... form ....)
   (#%module-begin             reader-option ... form ....)]
  ([reader-option (code:line #:read        read-expr)
                  (code:line #:read-syntax read-syntax-expr)
                  (code:line #:whole-body-readers? whole?-expr)
                  (code:line #:wrapper1    wrapper1-expr)
                  (code:line #:wrapper2    wrapper2-expr)
                  (code:line #:language    lang-expr)
                  (code:line #:info        info-expr)
                  (code:line #:language-info language-info-expr)])
  #:contracts ([read-expr (input-port? . -> . any/c)]
               [read-syntax-expr (any/c input-port? . -> . any/c)]
               [whole-expr any/c]
               [wrapper1-expr (or/c ((-> any/c) . -> . any/c)
                                    ((-> any/c) boolean? . -> . any/c))]
               [wrapper2-expr (or/c (input-port? (input-port? . -> . any/c) 
                                     . -> . any/c)
                                    (input-port? (input-port? . -> . any/c) 
                                     boolean? . -> . any/c))]
               [info-expr (symbol? any/c (symbol? any/c . -> . any/c) . -> . any/c)]
               [module-info-expr (or/c (vector/c module-path? symbol? any/c) #f)]
               [lang-expr (or/c module-path?
                                (and/c syntax? (compose module-path? syntax->datum))
                                procedure?)])]{

In its simplest form, the body of a module written with
@schememodname[syntax/module-reader] contains just a module path,
which is used in the language position of read modules. For example, a
module @scheme[_something]@scheme[/lang/reader] implemented as

@schemeblock[
(module reader @#,schememodname[syntax/module-reader]
  module-path)
]

creates a reader such that a module source

@schememod[
@#,scheme[_something]
....
]

is read as

@schemeblock[
  (module _name-id module-path
    (#%module-begin ....))
]

Keyword-based @scheme[reader-option]s allow further customization, as
listed below. Additional @scheme[form]s are as in the body of
@scheme[scheme/base] module; they can import bindings and define
identifiers used by the @scheme[reader-option]s.

@itemlist[

 @item{@scheme[#:read] and @scheme[#:read-syntax] (both or neither
       must be supplied) specify alternate readers for parsing the
       module body---replacements @scheme[read] and
       @scheme[read-syntax], respectively. Normally, the replacements
       for @scheme[read] and @scheme[read-syntax] are applied
       repeatedly to the module source until @scheme[eof] is produced,
       but see also @scheme[#:whole-body-readers?].

       For example, a language built on the @secref[#:doc '(lib
       "scribblings/honu/honu.scrbl")]{Honu} reader could be
       implemented with:

        @schemeblock[
          (module reader syntax/module-reader
            module-path
            #:read read-honu
            #:read-syntax read-honu-syntax)
        ]

        See also @scheme[#:wrapper1] and @scheme[#:wrapper2], which
        support simple parameterization of readers rather than
        wholesale replacement.}

 @item{@scheme[#:whole-body-readers?] specified as true indicates that
       the @scheme[#:read] and @scheme[#:read-syntax] functions each produce a
       list of S-expressions or syntax objects for the module content,
       so that each is applied just once to the input stream.

       If the resulting list contains a single form that starts with
       the symbol @scheme['#%module-begin] (or a syntax object whose
       datum is that symbol), then the first item is used as the
       module body; otherwise, a @scheme['#%module-begin] (symbol or
       identifier) is added to the beginning of the list to form the
       module body.}

 @item{@scheme[#:wrapper1] specifies a function that controls the
       dynamic context in which the @scheme[read] and
       @scheme[read-syntax] functions are called. A
       @scheme[#:wrapper1]-specified function must accept a thunk, and
       it normally calls the thunk to produce a result while
       @scheme[parameterizing] the call. Optionally, a
       @scheme[#:wrapper1]-specified function can accept a boolean
       that indicates whether it is used in @scheme[read]
       (@scheme[#f]) or @scheme[read-syntax] (@scheme[#t]) mode.

       For example, a language like @scheme[scheme/base] but with
       case-insensitive reading of symbols and identifiers can be
       implemented as

        @schemeblock[
          (module reader syntax/module-reader
            scheme/base
            #:wrapper1 (lambda (t)
                         (parameterize ([read-case-sensitive #f])
                           (t))))
        ]

       Using a @tech[#:doc refman]{readtable}, you can implement
       languages that are extensions of plain S-expressions.}

 @item{@scheme[#:wrapper2] is like @scheme[#:wrapper1], but a
       @scheme[#:wrapper2]-specified function receives the input port
       to be read, and the function that it receives accepts an input
       port (usually, but not necessarily the same input port). A
       @scheme[#:wrapper2]-specified function can optionally accept an
       boolean that indicates whether it is used in @scheme[read]
       (@scheme[#f]) or @scheme[read-syntax] (@scheme[#t]) mode.}

 @item{@scheme[#:info] specifies an implementation of reflective
       information that is used by external tools to manipulate the
       @emph{source} of modules in the language @scheme[_something]. For
       example, DrRacket uses information from @scheme[#:info] to
       determine the style of syntax coloring that it should use for
       editing a module's source.

       The @scheme[#:info] specification should be a function of three
       arguments: a symbol indicating the kind of information
       requested (as defined by external tools), a default value that
       normally should be returned if the symbol is not recognized,
       and a default-filtering function that takes the first two
       arguments and returns a result.

       The expression after @scheme[#:info] is placed into a context
       where @scheme[language-module] and @scheme[language-data] are
       bound. The @scheme[language-module] identifier is bound to the
       @scheme[module-path] that is used for the read module's
       language as written directly or as determined through
       @scheme[#:language]. The @scheme[language-data] identifier is
       bound to the second result from @scheme[#:language], or
       @scheme[#f] by default.

       The default-filtering function passed to the @scheme[#:info]
       function is intended to provide support for information that
       @schememodname[syntax/module-reader] can provide automatically.
       Currently, it recognizes only the @scheme['module-language]
       key, for which it returns @scheme[language-module]; it returns
       the given default value for any other key.

       In the case of the DrRacket syntax-coloring example, DrRacket
       supplies @scheme['color-lexer] as the symbol argument, and it
       supplies @scheme[#f] as the default. The default-filtering
       argument (i.e., the third argument to the @scheme[#:info]
       function) currently just returns the default for
       @scheme['color-lexer].}

 @item{@scheme[#:language-info] specifies an implementation of
       reflective information that is used by external tools to
       manipulate the module in the language @scheme[_something] in
       its @emph{expanded}, @emph{compiled}, or @emph{declared} form
       (as opposed to source). For example, when Racket starts a
       program, it uses information attached to the main module to
       initialize the run-time environment.

       Since the expanded/compiled/declared form exists at a different time
       than when the source is read, a @scheme[#:language-info]
       specification is a vector that indicates an implementation of
       the reflective information, instead of a direct implementation
       as a function like @scheme[#:info]. The first element of the
       vector is a module path, the second is a symbol corresponding
       to a function exported from the module, and the last element is
       a value to be passed to the function. The last value in the
       vector must be one that can be written with @scheme[write] and
       read back with @scheme[read]. When the exported function
       indicated by the first two vector elements is called with the
       value from the last vector element, the result should be a
       function or two arguments: a symbol and a default value. The
       symbol and default value are used as for the @scheme[#:info]
       function (but without an extra default-filtering function).

       The value specified by @scheme[#:language-info] is attached to
       the @scheme[module] form that is parsed from source through the
       @scheme['module-language] syntax property. See @scheme[module]
       for more information.

       The expression after @scheme[#:language-info] is placed into a
       context where @scheme[language-module] are
       @scheme[language-data] are bound, the same as for
       @scheme[#:info].

       In the case of the Racket run-time configuration example,
       Racket uses the @scheme[#:language-info] vector to obtain a
       function, and then it passes @scheme['configure-runtime] to the
       function to obtain information about configuring the runtime
       environment. See also @secref[#:doc refman "configure-runtime"].}

 @item{@scheme[#:language] allows the language of the read
       @scheme[module] to be computed dynamically and based on the
       program source, instead of using a constant
       @scheme[module-path]. (Either @scheme[#:language] or
       @scheme[module-path] must be provided, but not both.)

       This value of the @scheme[#:language] option can be either a
       module path (possibly as a syntax object) that is used as a
       module language, or it can be a procedure. If it is a procedure
       it can accept either

       @itemlist[
         @item{0 arguments;}
         @item{1 argument: an input port; or}
         @item{5 arguments: an input port, a syntax object whose datum
               is a module path for the enclosing module as it was
               referenced through @hash-lang[] or
               @schememetafont{#reader}, a starting line number
               (positive exact integer) or @scheme[#f], a column
               number (non-negative exact integer) or @scheme[#f], and
               a position number (positive exact integer) or
               @scheme[#f].}
        ] 

       The result can be either

       @itemlist[
          @item{a single value, which is a module path or a syntax
                object whose datum is a module path, to be used
                like @scheme[module-path]; or}
          @item{two values, where the first is like a single-value
                result and the second can be any value.}
       ]

       The second result, which defaults to @scheme[#f] if only a
       single result is produced, is made available to the
       @scheme[#:info] and @scheme[#:module-info] functions through
       the @scheme[language-data] binding. For example, it can be a
       specification derived from the input stream that changes the
       module's reflective information (such as the syntax-coloring
       mode or the output-printing styles).}

]

As another example, the following reader defines a ``language'' that
ignores the contents of the file, and simply reads files as if they
were empty:

    @schemeblock[
      (module ignored syntax/module-reader
        scheme/base
        #:wrapper1 (lambda (t) (t) '()))]

Note that the wrapper still performs the read, otherwise the module
loader would complain about extra expressions.

As a more useful example, the following module language is similar to
@schememodname[at-exp], where the first datum in the file determines
the actual language (which means that the library specification is
effectively ignored):

@schemeblock[
  (module reader syntax/module-reader
    -ignored-
    #:wrapper2
    (lambda (in rd stx?)
      (let* ([lang (read in)]
             [mod  (parameterize ([current-readtable
                                   (make-at-readtable)])
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

The ability to change the language position in the resulting module
expression can be useful in cases such as the above, where the base
language module is chosen based on the input.  To make this more
convenient, you can omit the @scheme[module-path] and instead specify
it via a @scheme[#:language] expression.  This expression can evaluate
to a datum or syntax object that is used as a language, or it can
evaluate to a thunk.  In the latter case, the thunk is invoked to
obtain such a datum before reading the module body begins, in a
dynamic extent where @scheme[current-input-port] is the source
input. A syntax object is converted using @scheme[syntax->datum] when
a datum is needed (for @scheme[read] instead of @scheme[read-syntax]).
Using @scheme[#:language], the last example above can be written more
concisely:

@schemeblock[
  (module reader syntax/module-reader
    #:language read
    #:wrapper2 (lambda (in rd stx?)
                 (parameterize ([current-readtable
                                 (make-at-readtable)])
                   (rd in)))
    (require scribble/reader))
]

For such cases, however, the alternative reader constructor
@scheme[make-meta-reader] implements a might tightly controlled
reading of the module language.}


@defproc[(make-meta-reader [self-sym symbol?]
                           [path-desc-str string?]
                           [#:read-spec read-spec (input-port? . -> . any/c) (lambda (in) ....)]
                           [module-path-parser (any/c . -> . (or/c module-path? #f))]
                           [convert-read (procedure? . -> . procedure?)]
                           [convert-read-syntax (procedure? . -> . procedure?)]
                           [convert-get-info  (procedure? . -> . procedure?)])
         (values procedure? procedure? procedure?)]{

Generates procedures suitable for export as @schemeidfont{read} (see
@scheme[read] and @hash-lang[]), @schemeidfont{read-syntax} (see
@scheme[read-syntax] and @hash-lang[]), and @schemeidfont{get-info}
(see @scheme[read-language] and @hash-lang[]), respectively, where the
procedures chains to another language that is specified in an input
stream.

@margin-note{The @schememodname[at-exp], @schememodname[reader], and
  @schememodname[planet] languages are implemented using this
  function.}

The generated functions expect a target language description in the
input stream that is provided to @scheme[read-spec]. The default
@scheme[read-spec] extracts a non-empty sequence of bytes after one or
more space and tab bytes, stopping at the first whitespace byte or
end-of-file (whichever is first), and it produces either such a byte
string or @scheme[#f].  If @scheme[read-spec] produces @scheme[#f], a
reader exception is raised, and @scheme[path-desc-str] is used as a
description of the expected language form in the error message.

@margin-note{The @schememodname[reader] language supplies
  @scheme[read] for @scheme[read-spec]. The @schememodname[at-exp] and
  @schememodname[planet] languages use the default
  @scheme[read-spec].}

The result of @scheme[read-spec] is converted to a module path using
@scheme[module-path-parser]. If @scheme[module-path-parser] produces
@scheme[#f], a reader exception is raised in the same way as when
@scheme[read-spec] produces a @scheme[#f]. The @schememodname[planet]
languages supply a @scheme[module-path-parser] that converts a byte
string to a module path.

If loading the module produced by @scheme[module-path-parser]
succeeds, then the loaded module's @schemeidfont{read},
@schemeidfont{read-syntax}, or @schemeidfont{get-info} export is
passed to @scheme[convert-read], @scheme[convert-read-syntax], or
@scheme[convert-get-info], respectively.

@margin-note{The @schememodname[at-exp] language supplies
  @scheme[convert-read] and @scheme[convert-read-syntax] to add
  @"@"-expression support to the current readtable before chaining to
  the given procedures.}

The procedures generated by @scheme[make-meta-reader] are not meant
for use with the @schememodname[syntax/module-reader] language; they
are meant to be exported directly.}


@defproc[(wrap-read-all [mod-path module-path?]
                        [in input-port?]
                        [read (input-port . -> . any/c)]
                        [mod-path-stx syntax?]
                        [src (or/c syntax? #f)]
                        [line number?]
                        [col number?]
                        [pos number?])
         any/c]{

@emph{This function is deprecated; the
@schememodname[syntax/module-reader] language can be adapted using the
various keywords to arbitrary readers; please use it instead.}

Repeatedly calls @scheme[read] on @scheme[in] until an end of file,
collecting the results in order into @scheme[_lst], and derives a
@scheme[_name-id] from @scheme[(object-name in)].  The last five
arguments are used to construct the syntax object for the language
position of the module.  The result is roughly

@schemeblock[
  `(module ,_name-id ,mod-path ,@_lst)
]}
