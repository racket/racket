#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/eval
          "utils.ss"
          (for-syntax scheme/base)
          (for-label (only-in scribble/reader
                              use-at-readtable)))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax scheme/base)))

@title[#:tag "reader-internals"]{@"@" Reader Internals}

@;--------------------------------------------------------------------
@section{Using the @"@" Reader}

You can use the reader via Scheme's @schemefont{#reader} form:

@schemeblock[
 @#,schemefont|{
     #reader scribble/reader @foo{This is free-form text!}
}|]

or use the @scheme[at-exp] meta-language as described in
@secref["at-exp-lang"].

Note that the Scribble reader reads @"@"-forms as S-expressions.  This
means that it is up to you to give meanings for these expressions in
the usual way: use Scheme functions, define your functions, or require
functions.  For example, typing the above into @exec{mzscheme} is likely
going to produce a ``reference to undefined identifier'' error, unless
@scheme[foo] is defined. You can use @scheme[string-append] instead,
or you can define @scheme[foo] as a function (with variable arity).

A common use of the Scribble @"@"-reader is when using Scribble as a
documentation system for producing manuals.  In this case, the manual
text is likely to start with

@schememod[scribble/doc]

which installs the @"@" reader starting in ``text mode,'' wraps the
file content afterward into a Scheme module where many useful Scheme
and documentation related functions are available, and parses the body
into a document using @schememodname[scribble/decode].  See
@secref["docreader"] for more information.

Another way to use the reader is to use the @scheme[use-at-readtable]
function to switch the current readtable to a readtable that parses
@"@"-forms.  You can do this in a single command line:

@commandline{mzscheme -ile scribble/reader "(use-at-readtable)"}

@;--------------------------------------------------------------------
@section{Syntax Properties}

The Scribble reader attaches properties to syntax objects.  These
properties might be useful in some rare situations.

Forms that Scribble reads are marked with a @scheme['scribble]
property, and a value of a list of three elements: the first is
@scheme['form], the second is the number of items that were read from
the datum part, and the third is the number of items in the body part
(strings, sub-forms, and escapes).  In both cases, a @scheme[0] means
an empty datum/body part, and @scheme[#f] means that the corresponding
part was omitted.  If the form has neither parts, the property is not
attached to the result.  This property can be used to give different
meanings to expressions from the datum and the body parts, for
example, implicitly quoted keywords:

@; FIXME: a bit of code duplication here
@def+int[
  #:eval read-eval
  (define-syntax (foo stx)
    (let ([p (syntax-property stx 'scribble)])
      (printf ">>> ~s\n" (syntax->datum stx))
      (syntax-case stx ()
        [(_ x ...)
         (and (pair? p) (eq? (car p) 'form) (even? (cadr p)))
         (let loop ([n (/ (cadr p) 2)]
                    [as '()]
                    [xs (syntax->list #'(x ...))])
           (if (zero? n)
             (with-syntax ([attrs (reverse as)]
                           [(x ...) xs])
               #'(list 'foo `attrs x ...))
             (loop (sub1 n)
                   (cons (with-syntax ([key (car xs)]
                                       [val (cadr xs)])
                           #'(key ,val))
                         as)
                   (cddr xs))))])))
  (eval:alts
   (code:line
    @#,tt["@foo[x 1 y (* 2 3)]{blah}"])
    ;; Unfortunately, expressions are preserved by `def+int'
    ;; using `quote', not `quote-syntax' (which would create all sorts
    ;; or binding trouble), so we manually re-attach the property:
    (eval (syntax-property #'@foo[x 1 y (* 2 3)]{blah}
                           'scribble '(form 4 1))))
]

In addition, the Scribble parser uses syntax properties to mark syntax
items that are not physically in the original source --- indentation
spaces and newlines.  Both of these will have a @scheme['scribble]
property; an indentation string of spaces will have
@scheme['indentation] as the value of the property, and a newline will
have a @scheme['(newline S)] value where @scheme[S] is the original
newline string including spaces that precede and follow it (which
includes the indentation for the following item).  This can be used to
implement a verbatim environment: drop indentation strings, and use
the original source strings instead of the single-newline string.  Here
is an example of this.

@; FIXME: a bit of code duplication here
@def+int[
  #:eval read-eval
  (define-syntax (verb stx)
    (syntax-case stx ()
      [(_ cmd item ...)
       #`(cmd
          #,@(let loop ([items (syntax->list #'(item ...))])
               (if (null? items)
                 '()
                 (let* ([fst  (car items)]
                        [prop (syntax-property fst 'scribble)]
                        [rst  (loop (cdr items))])
                   (cond [(eq? prop 'indentation) rst]
                         [(not (and (pair? prop)
                                    (eq? (car prop) 'newline)))
                          (cons fst rst)]
                         [else (cons (datum->syntax-object
                                      fst (cadr prop) fst)
                                     rst)])))))]))
  (eval:alts
   (code:line
    @#,tt["@verb[string-append]{"]
    @#,tt["  foo"]
    @#,tt["    bar"]
    @#,tt["}"])
   @verb[string-append]{
     foo
       bar
   })
]

@;--------------------------------------------------------------------
@section[#:tag "at-exp-lang"]{Adding @"@"-expressions to a Language}

@defmodulelang[at-exp]{The @schememodname[at-exp] language installs
@"@"-reader support in the readtable, and then chains to the reader of
another language that is specified immediately after
@schememodname[at-exp].}

For example, @scheme[@#,hash-lang[] at-exp scheme/base] adds @"@"-reader
support to @scheme[scheme/base], so that

@schememod[
at-exp scheme/base

(define (greet who) @#,elem{@tt["@"]@scheme[string-append]@schemeparenfont["{"]@schemevalfont{Hello, }@tt["@|"]@scheme[who]@tt["|"]@schemevalfont{.}@schemeparenfont["}"]})
(greet "friend")]

reports @scheme["Hello, friend."].

@;--------------------------------------------------------------------
@section{Interface}

@defmodule[scribble/reader]{The @schememodname[scribble/reader] module
provides direct Scribble reader functionality for advanced needs.}

@; The `with-scribble-read' trick below shadows `read' and
@;  `read-syntax' with for-label bindings from the Scribble reader

@(define-syntax with-scribble-read
   (syntax-rules ()
     [(_)
      (...
       (begin
         (require (for-label scribble/reader))

@; *** Start reader-import section ***
@defproc[(read [in input-port? (current-input-port)]) any]{}
@defproc[(read-syntax [source-name any/c (object-name in)]
                      [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{
These procedures implement the Scribble reader.  They do so by
constructing a reader table based on the current one, and using that
for reading.
}

@defproc[(read-inside [in input-port? (current-input-port)]) any]{}
@defproc[(read-syntax-inside [source-name any/c (object-name in)]
                             [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{
These @schemeid[-inside] variants parse as if starting inside a
@litchar["@{"]...@litchar["}"], and they return a (syntactic) list.
Useful for implementing languages that are textual by default (see
@filepath{docreader.ss} for example).
}

@defproc[(make-at-readtable
          [#:readtable readtable readtable? (current-readtable)]
          [#:command-char command-char character? #\@]
          [#:datum-readtable datum-readtable
                             (or/c readtable? boolean?
                                              (readtable? . -> . readtable?))
                             #t]
          [#:syntax-post-processor syntax-post-proc
                                   (syntax? . -> . syntax?)
                                   values])
          readtable?]{

Constructs an @"@"-readtable.  The keyword arguments can customize the
resulting reader in several ways:

@itemize[

@item{@scheme[readtable] --- a readtable to base the @"@"-readtable
  on.}

@item{@scheme[command-char] --- the character used for @"@"-forms.}

@item{@scheme[datum-readtable] --- determines the readtable used for
  reading the datum part.  A @scheme[#t] values uses the
  @"@"-readtable, otherwise it can be a readtable, or a
  readtable-to-readtable function that will construct one from the
  @"@"-readtable.  The idea is that you may want to have completely
  different uses for the datum part, for example, introducing a
  convenient @litchar{key=val} syntax for attributes.}

@item{@scheme[syntax-post-proc] --- function that is applied on
  each resulting syntax value after it has been parsed (but before it
  is wrapped quoting punctuations).  You can use this to further
  control uses of @"@"-forms, for example, making the command be the
  head of a list:

  @schemeblock[
    (use-at-readtable
      #:syntax-post-processor
      (lambda (stx)
        (syntax-case stx ()
          [(cmd rest ...) #'(list 'cmd rest ...)]
          [_else (error "@ forms must have a body")])))
  ]}

]}

@defproc[(make-at-reader [#:syntax? syntax? #t] [#:inside? inside? #f] ...)
          procedure?]{
Constructs a variant of a @"@"-readtable.  The arguments are the same
as in @scheme[make-at-readtable], with two more that determine the
kind of reader function that will be created: @scheme[syntax?] chooses
between a @scheme[read]- or @scheme[read-syntax]-like function, and
@scheme[inside?] chooses a plain reader or an @schemeid[-inside]
variant.

The resulting function has a different contract and action based on
these inputs.  The expected inputs are as in @scheme[read] or
@scheme[read-syntax] depending on @scheme[syntax?]; the function will
read a single expression or, if @scheme[inside?] is true, the whole
input; it will return a syntactic list of expressions rather than a
single one in this case.

Note that @scheme[syntax?] defaults to @scheme[#t], as this is the
more expected common case when you're dealing with concrete-syntax
reading.

Note that if @scheme[syntax?] is true, the @scheme[read]-like function
is constructed by simply converting a syntax result back into a datum.}

@defproc[(use-at-readtable ...) void?]{

Passes all arguments to @scheme[make-at-readtable], and installs the
resulting readtable using @scheme[current-readtable]. It also enables
line counting for the current input-port via @scheme[port-count-lines!].

This is mostly useful for playing with the Scribble syntax on the REPL.}

@; *** End reader-import section ***
))]))
@with-scribble-read[]

@; --------------------------------------------------
@(close-eval read-eval)

