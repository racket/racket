#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          honu/core/read
          (for-label honu/core/read))

@(define lcomma (litchar ", "))

@title{Honu}

@defterm{Honu} is a language with Java-like syntax built on top of Racket.
Honu's main goal is to support syntactic abstraction mechanisms similar to
Racket. Currently, Honu is a prototype and may change without notice.

@table-of-contents[]

@; ----------------------------------------------------------------------

@defmodulelang[honu]

@section{Get started}
To use Honu in a module, write the following line at the top of the file.

@racketmod[honu]

You can use Honu at the REPL on the command line by invoking racket like so
@verbatim{
racket -Iq honu
}

@section{Reader}

@subsection{Tokens}
The Honu reader, @racket[honu-read], will tokenize the input stream according to
the following regular expressions.

@itemize[
  @item{Identifiers are [a-zA-Z_?][a-zA-Z_?0-9]*}
  @item{Strings are "[^"]*"}
  @item{Numbers are \d+(\.\d+)?}
  @item{And the following tokens + = * / - ^ || | && <= >= <- < > !
  :: := : ; ` ' . ,  ( ) { } [ ]}
]

@subsection{Structure}

After tokenization a Honu program will be converted into a tree with minimal
structure. Enclosing tokens will be grouped into a single object represented as
an s-expression. Enclosing tokens are pairs of (), {}, and [].

Consider the following stream of tokens

@codeblock|{
x ( 5 + 2 )
}|

This will be converted into
@codeblock|{
(x (#%parens 5 + 2))
}|

{} will be converted to (#%braces ...) and [] will be conveted to (#%brackets
...)

@defproc[(honu-read (port port?)) any]{
  Read an s-expression from the given port.
}

@defproc[(honu-read-syntax (name any) (port port?)) any]{
  Read a syntax object from the given port.
}

@defproc[(honu-lexer (port port?)) (list position-token?)]{
  Tokenize a port into a stream of honu tokens.
}

@section{Parsing}

Honu is parsed using an algorithm based primarily on operator precedence. The
main focus of the operator precedence algorithm is to support infix operators.
In short, the algorithm operates in the following way

@itemlist[
@item{1. parse an @tech{expression}}
@item{2. check for a binary operator. if one is found then continue to step 3
otherwise return the expression from step 1 immediately.}
@item{3. parse another @tech{expression}}
@item{4. check for a binary operator. if one is found then check if its precedence is
higher than the operator found in step 2, and if so then continue parsing from
step 3. if the precedence is lower or an operator is not found then build an
infix expression from the left hand expression from step 1, the binary operator
in step 2, and the right hand expression in step 3.}
]

Parsing will maintain the following registers
@itemlist[
  @item{@bold{left} - a function that takes the right hand side of an expression and
  returns the infix expression by combining the left hand side and the
  operator.}
  @item{@bold{current} - the current right hand side}
  @item{@bold{precedence} - represents the current precedence level}
  @item{@bold{stream} - stream of tokens to parse}
]

This algorithm is illustrated with the following example. Consider the raw
stream of tokens

@codeblock|{ 1 + 2 * 3 - 9 }|

@tabular[
  @list[
    @list["left" (hspace 1) "current" (hspace 1) "precedence" (hspace 1) "stream"]
    @list[@racket[(lambda (x) x)] (hspace 1)
          @racket[#f] (hspace 1)
          @racket[0] (hspace 1)
          @codeblock|{1 + 2 * 3 - 9}|]
    @list[@racket[(lambda (x) x)] (hspace 1)
          @racket[1] (hspace 1)
          @racket[0] (hspace 1)
          @codeblock|{+ 2 * 3 - 9}|]
    @list[@racket[(lambda (x) #'(+ 1 x))] (hspace 1)
          @racket[#f] (hspace 1)
          @racket[1] (hspace 1)
          @codeblock|{2 * 3 - 9}|]
    @list[@racket[(lambda (x) #'(+ 1 x))] (hspace 1)
          @racket[2] (hspace 1)
          @racket[1] (hspace 1)
          @codeblock|{* 3 - 9}|]
    @list[@racket[(lambda (x) (left #'(* 2 x)))] (hspace 1)
          @racket[2] (hspace 1)
          @racket[2] (hspace 1)
          @codeblock|{3 - 9}|]
    @list[@racket[(lambda (x) (left #'(* 2 x)))] (hspace 1)
          @racket[3] (hspace 1)
          @racket[2] (hspace 1)
          @codeblock|{- 9}|]
    @list[@racket[(lambda (x) #'(- (+ 1 (* 2 3)) x))] (hspace 1)
          @racket[#f] (hspace 1)
          @racket[1] (hspace 1)
          @codeblock|{9}|]
    @list[@racket[(lambda (x) #'(- (+ 1 (* 2 3)) x))] (hspace 1)
          @racket[9] (hspace 1)
          @racket[1] (hspace 1)
          @codeblock|{}|]
  ]
]

When the stream of tokens is empty the @bold{current} register is passed as an
argument to the @bold{left} function which ultimately produces the expression
@codeblock|{(- (+ 1 (* 2 3)) 9)}|

In this example @racket[+] and @racket[-] both have a precedence of 1 while
@racket[*] has a precedence of 2. Currently, precedences can be any number that
can be compared with @racket[<=].

The example takes some liberties with respect to how the actual implementation
works. In particular the binary operators are syntax transformers that accept
the left and right hand expressions as parameters and return new syntax objects.
Also when the @racket[*] operator is parsed the @bold{left} function for
@racket[+] is nested inside the new function for @racket[*].

An @deftech{expression} can be one of the following
@itemlist[
  @item{@bold{datum} - number, string, or symbol. @codeblock|{5}|}
  @item{@bold{macro} - a symbol bound to a syntax transformer.
  @codeblock|{cond x = 5: true, else: false}|}
  @item{@bold{stop} - a symbol which immediately ends the current expression.
  these are currently , ; :}
  @item{@bold{lambda expression} - an identifier followed by @racket[(id ...)]
  followed by a block of code in braces. @codeblock|{add(x, y){ x + y }}|}
  @item{@bold{function application} - an expression followed by @racket[(arg
  ...)]. @codeblock|{f(2, 2)}|}
  @item{@bold{list comprehension} - @codeblock|{[x + 1: x <- [1, 2, 3]]}|}
  @item{@bold{block of code} - a series of expressions wrapped in braces.}
  @item{@bold{expression grouping} - any expression inside a set of parenthesis
  @codeblock|{(1 + 1) * 2}|}
]

@section{Macros}
@section{Language}
@section{Examples}
