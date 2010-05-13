#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/eval
          (for-label racket/match
                     syntax/readerr)
          "guide-utils.ss"
          (for-syntax racket/base
                      syntax/strip-context))

@(define-syntax (racketmodfile stx)
   (syntax-case stx ()
    [(_ file)
     (with-syntax ([(content ...)
                    (call-with-input-file* (syntax-e #'file)
                     (lambda (in)
                       (read-bytes 6 in)
                       (port-count-lines! in)
                       (let loop ()
                         (let ([v (read-syntax (object-name in) in)])
                           (if (eof-object? v) 
                               null 
                               (cons (replace-context #'file v) 
                                     (loop)))))))])
        #'(racketmod content ...))]))

@title[#:tag "hash-reader"]{Reader Extensions}

The @tech{reader} layer of the Racket language supports a
@litchar{#reader} syntax for allowing an external processor to parse
raw bytes into forms to be consumed by the @tech{expander} layer.
The syntax of @litchar{#reader} is

@racketblock[@#,(BNF-seq @litchar{#reader} @nonterm{module-path} @nonterm{reader-specific})]

where @nonterm{module-path} names a module that provides
@racketidfont{read} and @racketidfont{read-syntax} functions. The
@nonterm{module-path} itself is written with the reader syntax put in
place by its context. The @nonterm{reader-specific} part is a sequence
of characters that is parsed as determined by the @racketidfont{read}
and @racketidfont{read-syntax} functions that are exported by the
module named through @nonterm{module-path}.

For example, suppose that file @filepath{five.rkt} contains

@racketmod[
racket/base

(provide read read-syntax)

(define (read in) (list (read-string 5 in)))
(define (read-syntax src in) (list (read-string 5 in)))
]

Then, the program

@racketmod[
racket/base

'(1 @#,(elem @racketmetafont{#reader} @racket["five.rkt"] @tt{23456} @racket[7]) 8)
]

is equivalent to

@racketmod[
racket/base

'(1 ("23456") 7 8)
]

because the @racketidfont{read} and @racketidfont{read-syntax}
functions of @filepath{five.rkt} both read five characters from the
input stream and put them into a string and then a list. The reader
functions from @filepath{five.rkt} are not obliged to follow Racket
lexical conventions and treat the continuous sequence @litchar{234567}
as a single number. Since only the @litchar{23456} part is consumed by
@racketidfont{read} or @racketidfont{read-syntax}, the @litchar{7}
remains to be parsed in the usual racket way. Similarly, the reader
functions from @filepath{five.rkt} are not obliged to treat spaces as
whitespace, and

@racketmod[
racket/base

'(1 @#,(elem @racketmetafont{#reader} @racket["five.rkt"] @hspace[1] @tt{2345} @racket[67]) 8)
]

is equivalent to

@racketmod[
racket/base

'(1 (" 2345") 67 8)
]

since the first character immediately after @racket["five.rkt"] is a
space.

A @litchar{#reader} form can be used in the @tech{REPL}, too:

@interaction[
(eval:alts @#,(elem @racketmetafont{#reader}@racket["five.rkt"]@tt{abcde}) #reader"five.rkt"abcde)
]

@; ----------------------------------------------------------------------

@section{Source Locations}

The difference between @racketidfont{read} and
@racketidfont{read-syntax} is that @racketidfont{read} is meant to be
used for data like the Racket @racket[read] function, while
@racketidfont{read-syntax} is meant to be used to parse programs. More
precisely, the @racketidfont{read} function will be used when the
enclosing stream is being parsed by the Racket @racket[read], and
@racketidfont{read-syntax} is used when the enclosing stream is being
parsed by the Racket @racket[read-syntax] function. Nothing requires
@racketidfont{read} and @racketidfont{read-syntax} to parse input in
the same way, though they normally should.

Although the @racketidfont{read-syntax} function can return the same
kind of value as @racketidfont{read}, it should normally return a
@tech{syntax object} that connects the parsed expression with source
locations. Unlike the @filepath{five.rkt} example, the
@racketidfont{read-syntax} function is typically implemented directly,
and then @racketidfont{read} can use @racketidfont{read-syntax} and
strip away source information.

The following @filepath{arith.rkt} module implements that reader to
parse simple infix arithmetic expressions into Racket forms. For
example, @litchar{1*2+3} parses into the Racket form @racket[(+ (* 1
2) 3)]. Single-letter variables can appear in the expression. The
implementation uses @racket[port-next-location] to obtain the current
source location, and it uses @racket[datum->syntax] to turn raw values
into @tech{syntax objects}.

@racketmodfile["arith.rkt"]

If the @filepath{arith.rkt} reader is used in an expression position,
then its parse result will be treated as a Racket expression. If it is
used in a quoted form, however, then it just produces a number or a
list:

@interaction[
(eval:alts @#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) #reader"arith.rkt" 1*2+3 )
(eval:alts '@#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) '#reader"arith.rkt" 1*2+3 )
]

The @filepath{arith.rkt} reader could also be used in positions that
make no sense. Since the @racketidfont{read-syntax} implementation
tracks source locations, syntax errors can at least refer to parts of
the input in terms of their original locations (at the beginning of
the error message):

@interaction[
(eval:alts (let @#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) 8)
           (eval (parameterize ([read-accept-reader #t])
                   (read-syntax 'repl (let ([p @open-input-string{(let #reader"arith.rkt" 1*2+3 8)}])
                                        (port-count-lines! p)
                                        p)))))
]

@; ----------------------------------------------------------------------

@section{Readtables}

A reader extension's ability to parse input characters in an arbitrary
way can be powerful, but many cases of lexical extension call for a
less general but more composable approach. In much the same way that
the @tech{expander} level of Racket syntax can be extended through
@tech{macros}, the @tech{reader} level of Racket syntax can be more
composably extended through a @deftech{readtable}.

The Racket reader is a recursive-descent parser, and the
@tech{readtable} maps characters to parsing handlers. For example, the
default readtable maps @litchar{(} to a handler that recursively
parses subforms until it finds a @litchar{)}. The
@racket[current-readtable] @tech{parameter} determines the
@tech{readtable} that is used by @racket[read] or
@racket[read-syntax]. Rather than parsing raw characters directly, a
reader extension can install an extended @tech{readtable} and then
chain to @racket[read] or @racket[read-syntax].

@guideother{See @secref["parameterize"] for an introduction to
@tech{parameters}.}

The @racket[make-readtable] function constructs a new @tech{readtable}
as an extension of an existing one. It accepts a sequence of
specifications in terms of a character, a type of mapping for the
character, and (for certain types of mappings) a parsing
procedure. For example, to extend the readtable so that @litchar{$}
can be used to start and end infix expressions, implement a
@racket[parse-dollar] function and use:

@racketblock[
(make-readtable (current-readtable)
                #\$ 'terminating-macro read-dollar)
]

The protocol for @racket[read-dollar] requires the function to accept
different numbers of arguments depending on whether it is being used
in @racket[read] or @racket[read-syntax] mode. In @racket[read] mode,
the parser function is given two arguments: the character that
triggered the parser function and the input port that is being
read. In @racket[read-syntax] mode, the function must accept four
additional arguments that provide the source location of the
character.

The following @filepath{dollar.rkt} module defines a
@racket[parse-dollar] function in terms of the @racketidfont{read} and
@racketidfont{read-syntax} functions provided by @filepath{arith.rkt},
and it puts it together with new @racketidfont{read} and
@racketidfont{read-syntax} functions that install the readtable can
chain to Racket's @racket[read] or @racket[read-syntax]:

@racketmodfile["dollar.rkt"]

With this reader extension, a single @racketmetafont{#reader} can be
used at the beginning of an expression to enable multiple uses of
@litchar{$} to switch to infix arithmetic:

@interaction[
(eval:alts @#,(elem @racketmetafont{#reader}@racket["dollar.rkt"]@hspace[1]
                    @racket[(let ([a @#,tt{$1*2+3$}][b @#,tt{$5/6$}]) $a+b$)])
           #reader"dollar.rkt" (let ([a $1*2+3$][b $5/6$]) $a+b$))
]
