#lang scribble/doc
@(require scribble/manual scribble/bnf scribble/eval "utils.rkt"
          (for-syntax racket/base)
          (for-label (only-in scribble/reader
                              use-at-readtable)))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax racket/base)))

@title[#:tag "reader-internals"]{@"@" Reader Internals}

@;--------------------------------------------------------------------
@section{Using the @"@" Reader}

You can use the reader via Racket's @racketfont{#reader} form:

@racketblock[
 @#,racketfont|{
     #reader scribble/reader @foo{This is free-form text!}
}|]

or use the @racket[at-exp] meta-language as described in
@secref["at-exp-lang"].

Note that the Scribble reader reads @tech{@"@"-forms} as S-expressions.  This
means that it is up to you to give meanings for these expressions in
the usual way: use Racket functions, define your functions, or require
functions.  For example, typing the above into @exec{racket} is likely
going to produce a ``reference to undefined identifier'' error, unless
@racket[foo] is defined. You can use @racket[string-append] instead,
or you can define @racket[foo] as a function (with variable arity).

A common use of the Scribble @"@"-reader is when using Scribble as a
documentation system for producing manuals.  In this case, the manual
text is likely to start with

@racketmod[scribble/doc]

which installs the @"@" reader starting in ``text mode,'' wraps the
file content afterward into a Racket module where many useful Racket
and documentation related functions are available, and parses the body
into a document using @racketmodname[scribble/decode].  See
@secref["docreader"] for more information.

Another way to use the reader is to use the @racket[use-at-readtable]
function to switch the current readtable to a readtable that parses
@tech{@"@"-forms}.  You can do this in a single command line:

@commandline{racket -ile scribble/reader "(use-at-readtable)"}

@;--------------------------------------------------------------------
@section{Syntax Properties}

The Scribble reader attaches properties to syntax objects.  These
properties might be useful in some rare situations.

Forms that Scribble reads are marked with a @racket['scribble]
property, and a value of a list of three elements: the first is
@racket['form], the second is the number of items that were read from
the datum part, and the third is the number of items in the body part
(strings, sub-forms, and escapes).  In both cases, a @racket[0] means
an empty datum/body part, and @racket[#f] means that the corresponding
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
spaces and newlines.  Both of these will have a @racket['scribble]
property; an indentation string of spaces will have
@racket['indentation] as the value of the property, and a newline will
have a @racket['(newline S)] value where @racket[S] is the original
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

@defmodulelang[at-exp]{The @racketmodname[at-exp] language installs
@seclink["reader"]{@"@"-reader} support in the readtable, and then chains to the reader of
another language that is specified immediately after
@racketmodname[at-exp].}

For example, @racket[@#,hash-lang[] at-exp racket/base] adds @"@"-reader
support to @racket[racket/base], so that

@racketmod[
at-exp racket/base

(define (greet who) @#,elem{@tt["@"]@racket[string-append]@racketparenfont["{"]@racketvalfont{Hello, }@tt["@|"]@racket[who]@tt["|"]@racketvalfont{.}@racketparenfont["}"]})
(greet "friend")]

reports @racket["Hello, friend."].

@;--------------------------------------------------------------------
@section{Interface}

@defmodule[scribble/reader]{The @racketmodname[scribble/reader] module
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
                             [in input-port? (current-input-port)]
                             [#:command-char command-char char? #\@])
         (or/c syntax? eof-object?)]{
These @racketid[-inside] variants parse as if starting inside a
@litchar["@{"]...@litchar["}"], and they return a (syntactic) list.
The @racket[command-char] is used to customize the readtable.
Useful for implementing languages that are textual by default (see
@filepath{docreader.rkt} for example).
}

@defproc[(make-at-readtable
          [#:readtable readtable readtable? (current-readtable)]
          [#:command-char command-char char? #\@]
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

@item{@racket[readtable] --- a readtable to base the @"@"-readtable
  on.}

@item{@racket[command-char] --- the character used for @tech{@"@"-forms}.}

@item{@racket[datum-readtable] --- determines the readtable used for
  reading the datum part.  A @racket[#t] values uses the
  @"@"-readtable, otherwise it can be a readtable, or a
  readtable-to-readtable function that will construct one from the
  @"@"-readtable.  The idea is that you may want to have completely
  different uses for the datum part, for example, introducing a
  convenient @litchar{key=val} syntax for attributes.}

@item{@racket[syntax-post-proc] --- function that is applied on
  each resulting syntax value after it has been parsed (but before it
  is wrapped quoting punctuations).  You can use this to further
  control uses of @tech{@"@"-forms}, for example, making the command be the
  head of a list:

  @racketblock[
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
as in @racket[make-at-readtable], with two more that determine the
kind of reader function that will be created: @racket[syntax?] chooses
between a @racket[read]- or @racket[read-syntax]-like function, and
@racket[inside?] chooses a plain reader or an @racketid[-inside]
variant.

The resulting function has a different contract and action based on
these inputs.  The expected inputs are as in @racket[read] or
@racket[read-syntax] depending on @racket[syntax?]; the function will
read a single expression or, if @racket[inside?] is true, the whole
input; it will return a syntactic list of expressions rather than a
single one in this case.

Note that @racket[syntax?] defaults to @racket[#t], as this is the
more expected common case when you're dealing with concrete-syntax
reading.

Note that if @racket[syntax?] is true, the @racket[read]-like function
is constructed by simply converting a syntax result back into a datum.}

@defproc[(use-at-readtable ...) void?]{

Passes all arguments to @racket[make-at-readtable], and installs the
resulting readtable using @racket[current-readtable]. It also enables
line counting for the current input-port via @racket[port-count-lines!].

This is mostly useful for playing with the Scribble syntax on the REPL.}

@; *** End reader-import section ***
))]))
@with-scribble-read[]

@; --------------------------------------------------
@(close-eval read-eval)

