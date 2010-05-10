#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label scheme))

@(define lcomma (litchar ", "))

@title{@bold{Honu}}

@defterm{Honu} is a family of languages built on top of Racket. Honu
syntax resembles Java. Like Racket, however, Honu has no fixed syntax,
because Honu supports extensibility through macros and a base syntax
of @as-index{H-expressions}, which are analogous to S-expressions.

The Honu language currently exists only as a undocumented
prototype. Racket's parsing and printing of H-expressions is
independent of the Honu language, however, so it is documented here.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{H-expressions}

The Racket reader incorporates an H-expression reader, and Racket's
printer also supports printing values in Honu syntax. The reader can
be put into H-expression mode either by including @litchar{#hx} in the
input stream, or by calling @scheme[read-honu] or
@scheme[read-honu-syntax] instead of @scheme[read] or
@scheme[read-syntax]. Similarly, @scheme[print] (or, more precisely,
the default print handler) produces Honu output when the
@scheme[print-honu] parameter is set to @scheme[#t].

When the reader encounters @litchar{#hx}, it reads a single
H-expression, and it produces an S-expression that encodes the
H-expression. Except for atomic H-expressions, evaluating this
S-expression as Racket is unlikely to succeed. In other words,
H-expressions are not intended as a replacement for S-expressions to
represent Racket code.

Honu syntax is normally used via @litchar{#lang honu}, which reads
H-expressions repeatedly until an end-of-file is encountered, and
processes the result as a module in the Honu language.

Ignoring whitespace, an H-expression is either

@itemize[

 @item{a number (see @secref["honu:numbers"]);}

 @item{an identifier (see @secref["honu:identifiers"]);}

 @item{a string (see @secref["honu:strings"]);}

 @item{a character (see @secref["honu:chars"]);}

 @item{a sequence of H-expressions between parentheses (see @secref["honu:parens"]);}

 @item{a sequence of H-expressions between square brackets (see @secref["honu:parens"]);}

 @item{a sequence of H-expressions between curly braces (see @secref["honu:parens"]);}

 @item{a comment followed by an H-expression (see @secref["honu:comments"]);}

 @item{@litchar{#;} followed by two H-expressions (see @secref["honu:comments"]);}

 @item{@litchar{#hx} followed by an H-expression;}

 @item{@litchar{#sx} followed by an S-expression (see @secref[#:doc
'(lib "scribblings/reference/reference.scrbl") "reader"]).}

]

Within a sequence of H-expressions, a sub-sequence between angle
brackets is represented specially (see @secref["honu:parens"]).

Whitespace for H-expressions is as in Racket: any character for which
@scheme[char-whitespace?] returns true counts as a whitespace.

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:numbers"]{Numbers}

The syntax for Honu numbers is the same as for Java. The S-expression
encoding of a particular H-expression number is the obvious Racket
number.

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:identifiers"]{Identifiers}

The syntax for Honu identifiers is the union of Java identifiers plus
@litchar{;}, @litchar{,}, and a set of operator identifiers. An
@defterm{operator identifier} is any combination of the following
characters:

@t{
  @hspace[2] @litchar{+} @litchar{-} @litchar{=} @litchar{?} 
  @litchar{:} @litchar{<} @litchar{>} @litchar{.} @litchar{!} @litchar{%}
  @litchar{^} @litchar{&} @litchar{*} @litchar{/} @litchar{~} @litchar{|}
}

The S-expression encoding of an H-expression identifier is the obvious
Racket symbol.

Input is parsed to form maximally long identifiers. For example, the
input @litchar{int->int;} is parsed as four H-expressions represented
by symbols: @scheme['int], @scheme['->], @scheme['int], and
@scheme['|;|].

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:strings"]{Strings}

The syntax for an H-expression string is exactly the same as for an
S-expression string, and an H-expression string is represented by the
obvious Racket string.

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:chars"]{Characters}

The syntax for an H-expression character is the same as for an
H-expression string that has a single content character, except that a
@litchar{'} surrounds the character instead of @litchar{"}. The
S-expression representation of an H-expression character is the
obvious Racket character.

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:parens"]{Parentheses, Brackets, and Braces}

A H-expression between @litchar{(} and @litchar{)}, @litchar{[} and
@litchar{]}, or @litchar["{"] and @litchar["}"] is represented by a
Racket list. The first element of the list is @scheme['#%parens] for a
@litchar{(}...@litchar{)} sequence, @scheme['#%brackets] for a
@litchar{[}...@litchar{]} sequence, or @scheme['#%braces] for a
@litchar["{"]...@litchar["}"] sequence. The remaining elements are the
Racket representations for the grouped H-expressions in order.

In an H-expression sequence, when a @litchar{<} is followed by a
@litchar{>}, and when nothing between the @litchar{<} and @litchar{>}
is an immediate symbol containing a @litchar{=}, @litchar{&}, or
@litchar{|}, then the sub-sequence is represented by a Racket list
that starts with @scheme['#%angles] and continues with the elements of
the sub-sequence between the @litchar{<} and @litchar{>}
(exclusive). This representation is applied recursively, so that angle
brackets can be nested.

An angle-bracketed sequence by itself is not a single H-expression,
since the @litchar{<} by itself is a single H-expression; the
angle-bracket conversion is performed only when representing sequences
of H-expressions.

Symbols with a @litchar{=}, @litchar{&}, or @litchar{|} prevent
angle-bracket formation because they correspond to operators that
normally have lower or equal precedence compared to less-than and
greater-than operators.

@; ----------------------------------------------------------------------

@subsection[#:tag "honu:comments"]{Comments}

An H-expression comment starts with either @litchar{//} or
@litchar{/*}. In the former case, the comment runs until a linefeed or
return. In the second case, the comment runs until @litchar{*/}, but
@litchar{/*}...@litchar{*/} comments can be nested. Comments are
treated like whitespace.

A @litchar{#;} starts an H-expression comment, as in S-expressions. It
is followed by an H-expression to be treated as whitespace. Note that
@litchar{#;} is equivalent to @litchar{#sx#;#hx}.

@; ----------------------------------------------------------------------

@subsection{Honu Output Printing}

Some Racket values have a standard H-expression representation. For
values with no H-expression representation but with a
@scheme[read]able S-expression form, the Racket printer produces an
S-expression prefixed with @litchar{#sx}. For values with neither an
H-expression form nor a @scheme[read]able S-expression form, then
printer produces output of the form @litchar{#<}...@litchar{>}, as in
Racket mode. The @scheme[print-honu] parameter controls whether
Racket's printer produces Racket or Honu output.

The values with H-expression forms are as follows:

@itemize[

 @item{Every real number has an H-expression form, although the
       representation for an exact, non-integer rational number is
       actually three H-expressions, where the middle H-expression is
       @scheme[/].}

 @item{Every character string is represented the same in H-expression
       form as its S-expression form.}

 @item{Every character is represented like a single-character string,
       but (1) using a @litchar{'} as the delimiter instead of
       @litchar{"}, and (2) protecting a @litchar{'} character content
       with a @litchar{\} instead of protecting @litchar{"} character
       content.}

 @item{A list is represented with the H-expression sequence
       @litchar{list(}@nonterm{v}@|lcomma|...@litchar{)},
       where each @nonterm{v} is the representation of each element of
       the list.}

 @item{A pair that is not a list is represented with the H-expression
       sequence
       @litchar{cons(}@nonterm{v1}@|lcomma|@nonterm{v2}@litchar{)},
       where @nonterm{v1} and @nonterm{v2} are the representations of
       the pair elements.}

 @item{A vector's representation depends on the value of the
       @scheme[print-vector-length] parameter. If it is @scheme[#f],
       the vector is represented with the H-expression sequence
       @litchar{vectorN(}@nonterm{v}@|lcomma|...@litchar{)}, where
       each @nonterm{v} is the representation of each element of the
       vector. If @scheme[print-vector-length] is set to @scheme[#t],
       the vector is represented with the H-expression sequence
       @litchar{vectorN(}@nonterm{n}@|lcomma|@nonterm{v}@|lcomma|...@litchar{)},
       where @nonterm{n} is the length of the vector and each
       @nonterm{v} is the representation of each element of the
       vector, and multiple instances of the same value at the end of
       the vector are represented by a single @nonterm{v}.}

 @item{The empty list is represented as the H-expression
       @litchar{null}.}

 @item{True is represented as the H-expression @litchar{true}.}

 @item{False is represented as the H-expression @litchar{false}.}

]
