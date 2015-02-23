#lang scribble/doc
@(require "common.rkt")

@(require (for-label syntax/module-reader
                     (only-in scribble/reader
                              read-syntax-inside read-inside)))

@(begin
   (define-syntax-rule (define-mb name)
     (begin
       (require (for-label racket/base))
       (define name @racket[#%module-begin])))
   (define-mb scheme-#%module-begin))

@(define guide-doc '(lib "scribblings/guide/guide.scrbl"))
@(define ref-doc '(lib "scribblings/reference/reference.scrbl"))

@title[#:tag "module-reader"]{Module Reader}

@margin-note{See also @secref[#:doc guide-doc "hash-languages"] in
             @other-manual[guide-doc].}

@defmodule[syntax/module-reader]

The @racketmodname[syntax/module-reader] library provides support for
defining @hash-lang[] readers. It is normally used as a module
language, though it may also be @racket[require]d to get
@racket[make-meta-reader]. It provides all of the bindings of
@racket[racket/base] other than @|scheme-#%module-begin|.

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
               [whole?-expr any/c]
               [wrapper1-expr (or/c ((-> any/c) . -> . any/c)
                                    ((-> any/c) boolean? . -> . any/c))]
               [wrapper2-expr (or/c (input-port? (input-port? . -> . any/c) 
                                     . -> . any/c)
                                    (input-port? (input-port? . -> . any/c) 
                                     boolean? . -> . any/c))]
               [info-expr (symbol? any/c (symbol? any/c . -> . any/c) . -> . any/c)]
               [language-info-expr (or/c (vector/c module-path? symbol? any/c) #f)]
               [lang-expr (or/c module-path?
                                (and/c syntax? (compose module-path? syntax->datum))
                                procedure?)])]{

In its simplest form, the body of a module written with
@racketmodname[syntax/module-reader] contains just a module path,
which is used in the language position of read modules. For example, a
module @racket[_something]@racket[/lang/reader] implemented as

@racketblock[
(module reader @#,racketmodname[syntax/module-reader]
  module-path)
]

creates a reader such that a module source

@racketmod[
@#,racket[_something]
....
]

is read as

@racketblock[
  (module _name-id module-path
    (#%module-begin ....))
]

where @racket[_name-id] is derived from the source input port's name
in the same way as for @racket[@#,hash-lang[] @#,racketmodname[s-exp]].

Keyword-based @racket[reader-option]s allow further customization, as
listed below. Additional @racket[form]s are as in the body of
@racket[racket/base] module; they can import bindings and define
identifiers used by the @racket[reader-option]s.

@itemlist[

 @item{@racket[#:read] and @racket[#:read-syntax] (both or neither
       must be supplied) specify alternate readers for parsing the
       module body---replacements @racket[read] and
       @racket[read-syntax], respectively. Normally, the replacements
       for @racket[read] and @racket[read-syntax] are applied
       repeatedly to the module source until @racket[eof] is produced,
       but see also @racket[#:whole-body-readers?]. 

       Unless @racket[#:whole-body-readers?] specifies a true value,
       the repeated use of @racket[read] or @racket[read-syntax] is
       @racket[parameterize]d to set @racket[read-accept-lang] to
       @racket[#f], which disables nested uses of @hash-lang[].

        See also @racket[#:wrapper1] and @racket[#:wrapper2], which
        support simple parameterization of readers rather than
        wholesale replacement.}

 @item{@racket[#:whole-body-readers?] specified as true indicates that
       the @racket[#:read] and @racket[#:read-syntax] functions each produce a
       list of S-expressions or syntax objects for the module content,
       so that each is applied just once to the input stream.

       If the resulting list contains a single form that starts with
       the symbol @racket['#%module-begin] (or a syntax object whose
       datum is that symbol), then the first item is used as the
       module body; otherwise, a @racket['#%module-begin] (symbol or
       identifier) is added to the beginning of the list to form the
       module body.}

 @item{@racket[#:wrapper1] specifies a function that controls the
       dynamic context in which the @racket[read] and
       @racket[read-syntax] functions are called. A
       @racket[#:wrapper1]-specified function must accept a thunk, and
       it normally calls the thunk to produce a result while
       @racket[parameterizing] the call. Optionally, a
       @racket[#:wrapper1]-specified function can accept a boolean
       that indicates whether it is used in @racket[read]
       (@racket[#f]) or @racket[read-syntax] (@racket[#t]) mode.

       For example, a language like @racket[racket/base] but with
       case-insensitive reading of symbols and identifiers can be
       implemented as

        @racketblock[
          (module reader syntax/module-reader
            racket/base
            #:wrapper1 (lambda (t)
                         (parameterize ([read-case-sensitive #f])
                           (t))))
        ]

       Using a @tech[#:doc refman]{readtable}, you can implement
       languages that are extensions of plain S-expressions.}

 @item{@racket[#:wrapper2] is like @racket[#:wrapper1], but a
       @racket[#:wrapper2]-specified function receives the input port
       to be read, and the function that it receives accepts an input
       port (usually, but not necessarily the same input port). A
       @racket[#:wrapper2]-specified function can optionally accept an
       boolean that indicates whether it is used in @racket[read]
       (@racket[#f]) or @racket[read-syntax] (@racket[#t]) mode.}

 @item{@racket[#:info] specifies an implementation of reflective
       information that is used by external tools to manipulate the
       @emph{source} of modules in the language @racket[_something]. For
       example, DrRacket uses information from @racket[#:info] to
       determine the style of syntax coloring that it should use for
       editing a module's source.

       The @racket[#:info] specification should be a function of three
       arguments: a symbol indicating the kind of information
       requested (as defined by external tools), a default value that
       normally should be returned if the symbol is not recognized,
       and a default-filtering function that takes the first two
       arguments and returns a result.

       The expression after @racket[#:info] is placed into a context
       where @racket[language-module] and @racket[language-data] are
       bound. The @racket[language-module] identifier is bound to the
       @racket[module-path] that is used for the read module's
       language as written directly or as determined through
       @racket[#:language]. The @racket[language-data] identifier is
       bound to the second result from @racket[#:language], or
       @racket[#f] by default.

       The default-filtering function passed to the @racket[#:info]
       function is intended to provide support for information that
       @racketmodname[syntax/module-reader] can provide automatically.
       Currently, it recognizes only the @racket['module-language]
       key, for which it returns @racket[language-module]; it returns
       the given default value for any other key.

       In the case of the DrRacket syntax-coloring example, DrRacket
       supplies @racket['color-lexer] as the symbol argument, and it
       supplies @racket[#f] as the default. The default-filtering
       argument (i.e., the third argument to the @racket[#:info]
       function) currently just returns the default for
       @racket['color-lexer].}

 @item{@racket[#:language-info] specifies an implementation of
       reflective information that is used by external tools to
       manipulate the module in the language @racket[_something] in
       its @emph{expanded}, @emph{compiled}, or @emph{declared} form
       (as opposed to source). For example, when Racket starts a
       program, it uses information attached to the main module to
       initialize the run-time environment.

       @tech[#:doc ref-doc]{Submodules} are normally a better way to
       implement reflective information, instead of
       @racket[#:language-info]. For example, when Racket starts a
       program, it also checks for a @racket[configure-runtime]
       submodule of the main module to initialize the run-time
       environment. The @racket[#:language-info] mechanism pre-dates
       submodules.

       Since the expanded/compiled/declared form exists at a different time
       than when the source is read, a @racket[#:language-info]
       specification is a vector that indicates an implementation of
       the reflective information, instead of a direct implementation
       as a function like @racket[#:info]. The first element of the
       vector is a module path, the second is a symbol corresponding
       to a function exported from the module, and the last element is
       a value to be passed to the function. The last value in the
       vector must be one that can be written with @racket[write] and
       read back with @racket[read]. When the exported function
       indicated by the first two vector elements is called with the
       value from the last vector element, the result should be a
       function or two arguments: a symbol and a default value. The
       symbol and default value are used as for the @racket[#:info]
       function (but without an extra default-filtering function).

       The value specified by @racket[#:language-info] is attached to
       the @racket[module] form that is parsed from source through the
       @racket['module-language] syntax property. See @racket[module]
       for more information.

       The expression after @racket[#:language-info] is placed into a
       context where @racket[language-module] are
       @racket[language-data] are bound, the same as for
       @racket[#:info].

       In the case of the Racket run-time configuration example,
       Racket uses the @racket[#:language-info] vector to obtain a
       function, and then it passes @racket['configure-runtime] to the
       function to obtain information about configuring the runtime
       environment. See also @secref[#:doc refman "configure-runtime"].}

 @item{@racket[#:language] allows the language of the read
       @racket[module] to be computed dynamically and based on the
       program source, instead of using a constant
       @racket[module-path]. (Either @racket[#:language] or
       @racket[module-path] must be provided, but not both.)

       This value of the @racket[#:language] option can be either a
       module path (possibly as a syntax object) that is used as a
       module language, or it can be a procedure. If it is a procedure
       it can accept either

       @itemlist[
         @item{0 arguments;}
         @item{1 argument: an input port; or}
         @item{5 arguments: an input port, a syntax object whose datum
               is a module path for the enclosing module as it was
               referenced through @hash-lang[] or
               @racketmetafont{#reader}, a starting line number
               (positive exact integer) or @racket[#f], a column
               number (non-negative exact integer) or @racket[#f], and
               a position number (positive exact integer) or
               @racket[#f].}
        ]

       The result can be either

       @itemlist[
          @item{a single value, which is a module path or a syntax
                object whose datum is a module path, to be used
                like @racket[module-path]; or}
          @item{two values, where the first is like a single-value
                result and the second can be any value.}
       ]

       The second result, which defaults to @racket[#f] if only a
       single result is produced, is made available to the
       @racket[#:info] and @racket[#:module-info] functions through
       the @racket[language-data] binding. For example, it can be a
       specification derived from the input stream that changes the
       module's reflective information (such as the syntax-coloring
       mode or the output-printing styles).}

]

As another example, the following reader defines a ``language'' that
ignores the contents of the file, and simply reads files as if they
were empty:

    @racketblock[
      (module ignored syntax/module-reader
        racket/base
        #:wrapper1 (lambda (t) (t) '()))]

Note that the wrapper still performs the read, otherwise the module
loader would complain about extra expressions.

As a more useful example, the following module language is similar to
@racketmodname[at-exp], where the first datum in the file determines
the actual language (which means that the library specification is
effectively ignored):

@racketblock[
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
convenient, you can omit the @racket[module-path] and instead specify
it via a @racket[#:language] expression.  This expression can evaluate
to a datum or syntax object that is used as a language, or it can
evaluate to a thunk.  In the latter case, the thunk is invoked to
obtain such a datum before reading the module body begins, in a
dynamic extent where @racket[current-input-port] is the source
input. A syntax object is converted using @racket[syntax->datum] when
a datum is needed (for @racket[read] instead of @racket[read-syntax]).
Using @racket[#:language], the last example above can be written more
concisely:

@racketblock[
  (module reader syntax/module-reader
    #:language read
    #:wrapper2 (lambda (in rd stx?)
                 (parameterize ([current-readtable
                                 (make-at-readtable)])
                   (rd in)))
    (require scribble/reader))
]

For such cases, however, the alternative reader constructor
@racket[make-meta-reader] implements a might tightly controlled
reading of the module language.}


@defproc[(make-meta-reader [self-sym symbol?]
                           [path-desc-str string?]
                           [#:read-spec read-spec (input-port? . -> . any/c) (lambda (in) ....)]
                           [module-path-parser (any/c . -> . (or/c module-path? #f 
                                                                   (vectorof module-path?)))]
                           [convert-read (procedure? . -> . procedure?)]
                           [convert-read-syntax (procedure? . -> . procedure?)]
                           [convert-get-info  (procedure? . -> . procedure?)])
         (values procedure? procedure? procedure?)]{

Generates procedures suitable for export as @racketidfont{read} (see
@racket[read] and @hash-lang[]), @racketidfont{read-syntax} (see
@racket[read-syntax] and @hash-lang[]), and @racketidfont{get-info}
(see @racket[read-language] and @hash-lang[]), respectively, where the
procedures chains to another language that is specified in an input
stream.

@margin-note{The @racketmodname[at-exp], @racketmodname[reader], and
  @racketmodname[planet] languages are implemented using this
  function.}

The generated functions expect a target language description in the
input stream that is provided to @racket[read-spec]. The default
@racket[read-spec] extracts a non-empty sequence of bytes after one or
more space and tab bytes, stopping at the first whitespace byte or
end-of-file (whichever is first), and it produces either such a byte
string or @racket[#f].  If @racket[read-spec] produces @racket[#f], a
reader exception is raised, and @racket[path-desc-str] is used as a
description of the expected language form in the error message.

@margin-note{The @racketmodname[reader] language supplies
  @racket[read] for @racket[read-spec]. The @racketmodname[at-exp] and
  @racketmodname[planet] languages use the default
  @racket[read-spec].}

The result of @racket[read-spec] is converted to a module path using
@racket[module-path-parser]. If @racket[module-path-parser] produces
a vector of module paths, they are tried in order using 
@racket[module-declared?]. If @racket[module-path-parser] produces
@racket[#f], a reader exception is raised in the same way as when
@racket[read-spec] produces a @racket[#f]. The @racketmodname[planet]
languages supply a @racket[module-path-parser] that converts a byte
string to a module path.

If loading the module produced by @racket[module-path-parser]
succeeds, then the loaded module's @racketidfont{read},
@racketidfont{read-syntax}, or @racketidfont{get-info} export is
passed to @racket[convert-read], @racket[convert-read-syntax], or
@racket[convert-get-info], respectively.

@margin-note{The @racketmodname[at-exp] language supplies
  @racket[convert-read] and @racket[convert-read-syntax] to add
  @"@"-expression support to the current readtable before chaining to
  the given procedures.}

The procedures generated by @racket[make-meta-reader] are not meant
for use with the @racketmodname[syntax/module-reader] language; they
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
@racketmodname[syntax/module-reader] language can be adapted using the
various keywords to arbitrary readers; please use it instead.}

Repeatedly calls @racket[read] on @racket[in] until an end of file,
collecting the results in order into @racket[_lst], and derives a
@racket[_name-id] from @racket[(object-name in)] in the same way as
@racket[@#,hash-lang[] @#,racketmodname[s-exp]].  The last five arguments are
used to construct the syntax object for the language position of the
module.  The result is roughly

@racketblock[
  `(module ,_name-id ,mod-path ,@_lst)
]}
