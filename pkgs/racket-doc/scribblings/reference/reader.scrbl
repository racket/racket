#lang scribble/doc
@(require "mz.rkt" scribble/bnf "reader-example.rkt")
@(begin
   (define (ilitchar s)
     (litchar s))
   (define (nunterm s)
     (nonterm s (subscript "n")))
   (define (sub n) (subscript n))
   (define (nonalpha)
     @elem{; the next character must not be @racketlink[char-alphabetic?]{alphabetic}.})
   (define (graph-tag) @kleenerange[1 8]{@nonterm{digit@sub{10}}})
   (define (graph-defn) @elem{@litchar{#}@graph-tag[]@litchar{=}})
   (define (graph-ref) @elem{@litchar{#}@graph-tag[]@litchar{#}}))

@title[#:tag "reader" #:style 'quiet]{The Reader}

Racket's reader is a recursive-descent parser that can be configured
through a @seclink["readtables"]{readtable} and various other
@tech{parameters}. This section describes the reader's parsing when
using the default readtable.

Reading from a stream produces one @deftech{datum}. If the result
datum is a compound value, then reading the datum typically requires
the reader to call itself recursively to read the component data.

The reader can be invoked in either of two modes: @racket[read] mode,
or @racket[read-syntax] mode. In @racket[read-syntax] mode, the result
is always a @techlink{syntax object} that includes
source-location and (initially empty) lexical information wrapped
around the sort of datum that @racket[read] mode would produce. In the
case of @tech{pairs}, @tech{vectors}, and @tech{box}es, the content is also
wrapped recursively as a syntax object. Unless specified otherwise,
this section describes the reader's behavior in @racket[read] mode,
and @racket[read-syntax] mode does the same modulo wrapping of the final
result.

Reading is defined in terms of Unicode characters; see
@secref["ports"] for information on how a byte stream is converted
to a character stream.

Symbols, keywords, strings, byte strings, regexps, characters, and
numbers produced by the reader in @racket[read-syntax] mode are
@deftech{interned}, which means that such values in the result of
@racket[read-syntax] are always @racket[eq?] when they are
@racket[equal?] (whether from the same call or different calls to
@racket[read-syntax]). Symbols and keywords are @tech{interned} in
both @racket[read] and @racket[read-syntax] mode. Sending an
@tech{interned} value across a @tech{place channel} does not
necessarily produce an @tech{interned} value at the receiving
@tech{place}. See also @racket[datum-intern-literal] and
@racket[datum->syntax].

@;------------------------------------------------------------------------
@section[#:tag "default-readtable-dispatch"]{Delimiters and Dispatch}

Along with @racketlink[char-whitespace?]{whitespace}, the following
characters are @defterm{delimiters}:

@t{
  @hspace[2] @ilitchar{(} @ilitchar{)} @ilitchar{[} @ilitchar{]}
  @ilitchar["{"] @ilitchar["}"]
  @ilitchar{"} @ilitchar{,} @ilitchar{'} @ilitchar{`}
  @ilitchar{;}
}

A delimited sequence that starts with any other character is typically
parsed as either a symbol, number, or @tech{extflonum}, but a few non-delimiter
characters play special roles:

@itemize[

 @item{@litchar{#} has a special meaning as an initial character in a
       delimited sequence; its meaning depends on the characters that
       follow; see below.}

 @item{@as-index{@litchar{|}} starts a subsequence of characters to
       be included verbatim in the delimited sequence (i.e., they are
       never treated as delimiters, and they are not case-folded when
       case-insensitivity is enabled); the subsequence is terminated
       by another @litchar{|}, and neither the initial nor
       terminating @litchar{|} is part of the subsequence.}

 @item{@as-index{@litchar{\}} outside of a @litchar{|} pair causes
       the following character to be included verbatim in a delimited
       sequence.}

]

More precisely, after skipping whitespace, the reader dispatches based
on the next character or characters in the input stream as follows:

@dispatch-table[

  @dispatch[@litchar{(}]{starts a @tech{pair} or @tech{list}; see @secref["parse-pair"]}
  @dispatch[@litchar{[}]{starts a @tech{pair} or @tech{list}; see @secref["parse-pair"]}
  @dispatch[@litchar["{"]]{starts a @tech{pair} or @tech{list}; see @secref["parse-pair"]}

  @dispatch[@litchar{)}]{matches @litchar{(} or raises @Exn[exn:fail:read]}
  @dispatch[@litchar{]}]{matches @litchar{[} or raises @Exn[exn:fail:read]}
  @dispatch[@litchar["}"]]{matches @litchar["{"] or raises @Exn[exn:fail:read]}

  @dispatch[@litchar{"}]{starts a @tech{string}; see @secref["parse-string"]}
  @dispatch[@litchar{'}]{starts a quote; see @secref["parse-quote"]}
  @dispatch[@litchar{`}]{starts a quasiquote; see @secref["parse-quote"]}
  @dispatch[@litchar{,}]{starts a [splicing] unquote; see @secref["parse-quote"]}

  @dispatch[@litchar{;}]{starts a line comment; see @secref["parse-comment"]}

  @dispatch[@cilitchar{#t}]{true; see @secref["parse-boolean"]}
  @dispatch[@cilitchar{#f}]{false; see @secref["parse-boolean"]}

  @dispatch[@litchar{#(}]{starts a @tech{vector}; see @secref["parse-vector"]}
  @dispatch[@litchar{#[}]{starts a @tech{vector}; see @secref["parse-vector"]}
  @dispatch[@litchar["#{"]]{starts a @tech{vector}; see @secref["parse-vector"]}

  @dispatch[@litchar{#fl(}]{starts a @tech{flvector}; see @secref["parse-vector"]}
  @dispatch[@litchar{#fl[}]{starts a @tech{flvector}; see @secref["parse-vector"]}
  @dispatch[@litchar["#fl{"]]{starts a @tech{flvector}; see @secref["parse-vector"]}

  @dispatch[@litchar{#fx(}]{starts a @tech{fxvector}; see @secref["parse-vector"]}
  @dispatch[@litchar{#fx[}]{starts a @tech{fxvector}; see @secref["parse-vector"]}
  @dispatch[@litchar["#fx{"]]{starts a @tech{fxvector}; see @secref["parse-vector"]}

  @dispatch[@litchar{#s(}]{starts a @tech{structure} literal; see @secref["parse-structure"]}
  @dispatch[@litchar{#s[}]{starts a @tech{structure} literal; see @secref["parse-structure"]}
  @dispatch[@litchar["#s{"]]{starts a @tech{structure} literal; see @secref["parse-structure"]}

  @dispatch[@litchar{#\}]{starts a @tech{character}; see @secref["parse-character"]}

  @dispatch[@litchar{#"}]{starts a @tech{byte string}; see @secref["parse-string"]}
  @dispatch[@litchar{#%}]{starts a @tech{symbol}; see @secref["parse-symbol"]}
  @dispatch[@litchar{#:}]{starts a @tech{keyword}; see @secref["parse-keyword"]}
  @dispatch[@litchar{#&}]{starts a @tech{box}; see @secref["parse-box"]}

  @dispatch[@litchar{#|}]{starts a block comment; see @secref["parse-comment"]}
  @dispatch[@litchar{#;}]{starts an S-expression comment; see @secref["parse-comment"]}
  @dispatch[@litchar{#'}]{starts a syntax quote; see @secref["parse-quote"]}
  @dispatch[@litchar{#! }]{starts a line comment; see @secref["parse-comment"]}
  @dispatch[@litchar{#!/}]{starts a line comment; see @secref["parse-comment"]}
  @dispatch[@litchar{#!}]{may start a reader extension; see @secref["parse-reader"]}
  @dispatch[@litchar{#`}]{starts a syntax quasiquote; see @secref["parse-quote"]}
  @dispatch[@litchar{#,}]{starts a syntax [splicing] unquote; see @secref["parse-quote"]}
  @dispatch[@litchar{#~}]{starts compiled code; see @secref["print-compiled"]}

  @dispatch[@cilitchar{#i}]{starts a @tech{number}; see @secref["parse-number"]}
  @dispatch[@cilitchar{#e}]{starts a @tech{number}; see @secref["parse-number"]}
  @dispatch[@cilitchar{#x}]{starts a @tech{number} or @tech{extflonum}; see @secref["parse-number"]}
  @dispatch[@cilitchar{#o}]{starts a @tech{number} or @tech{extflonum}; see @secref["parse-number"]}
  @dispatch[@cilitchar{#d}]{starts a @tech{number} or @tech{extflonum}; see @secref["parse-number"]}
  @dispatch[@cilitchar{#b}]{starts a @tech{number} or @tech{extflonum}; see @secref["parse-number"]}

  @dispatch[@cilitchar["#<<"]]{starts a @tech{string}; see @secref["parse-string"]}

  @dispatch[@litchar{#rx}]{starts a @tech{regular expression}; see @secref["parse-regexp"]}
  @dispatch[@litchar{#px}]{starts a @tech{regular expression}; see @secref["parse-regexp"]}

  @dispatch[@cilitchar{#ci}]{switches case sensitivity; see @secref["parse-symbol"]}
  @dispatch[@cilitchar{#cs}]{switches case sensitivity; see @secref["parse-symbol"]}

  @dispatch[@litchar{#hash}]{starts a @tech{hash table}; see @secref["parse-hashtable"]}

  @dispatch[@litchar{#reader}]{starts a reader extension use; see @secref["parse-reader"]}
  @dispatch[@litchar{#lang}]{starts a reader extension use; see @secref["parse-reader"]}

  @dispatch[@elem{@litchar{#}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{(}}]{starts a vector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{[}}]{starts a vector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#}@kleeneplus{@nonterm{digit@sub{10}}}@litchar["{"]}]{starts a vector; see @secref["parse-vector"]}

  @dispatch[@elem{@litchar{#fl}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{(}}]{starts a flvector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#fl}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{[}}]{starts a flvector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#fl}@kleeneplus{@nonterm{digit@sub{10}}}@litchar["{"]}]{starts a flvector; see @secref["parse-vector"]}

  @dispatch[@elem{@litchar{#fx}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{(}}]{starts a fxvector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#fx}@kleeneplus{@nonterm{digit@sub{10}}}@litchar{[}}]{starts a fxvector; see @secref["parse-vector"]}
  @dispatch[@elem{@litchar{#fx}@kleeneplus{@nonterm{digit@sub{10}}}@litchar["{"]}]{starts a fxvector; see @secref["parse-vector"]}

  @dispatch[@graph-defn[]]{binds a graph tag; see @secref["parse-graph"]}
  @dispatch[@graph-ref[]]{uses a graph tag; see @secref["parse-graph"]}

  @dispatch[@italic{otherwise}]{starts a @tech{symbol}; see @secref["parse-symbol"]}

]


@section[#:tag "parse-symbol"]{Reading Symbols}

@guideintro["symbols"]{the syntax of symbols}

A sequence that does not start with a delimiter or @litchar{#} is
parsed as either a @tech{symbol}, a @tech{number} (see
@secref["parse-number"]), or a @tech{extflonum}
(see @secref["parse-extflonum"]), 
except that @litchar{.} by itself is never
parsed as a symbol or number (unless the @racket[read-accept-dot]
parameter is set to @racket[#f]). A @as-index{@litchar{#%}} also
starts a symbol. The resulting symbol is @tech{interned}. A successful
number or extflonum parse takes precedence over a symbol parse.

@index["case-sensitivity"]{@index["case-insensitive"]{When}} the
@racket[read-case-sensitive] @tech{parameter} is set to @racket[#f],
characters in the sequence that are not quoted by @litchar{|} or
@litchar{\} are first case-normalized. If the reader encounters
@as-index{@litchar{#ci}}, @litchar{#CI}, @litchar{#Ci}, or
@litchar{#cI}, then it recursively reads the following datum in
case-insensitive mode. If the reader encounters
@as-index{@litchar{#cs}}, @litchar{#CS}, @litchar{#Cs}, or
@litchar{#cS}, then it recursively reads the following datum in
case-sensitive mode.

@reader-examples[#:symbols? #f
"Apple"
"Ap#ple"
"Ap ple"
"Ap| |ple"
"Ap\\ ple"
"#ci Apple"
"#ci |A|pple"
"#ci \\Apple"
"#ci#cs Apple"
"#%Apple"
]

@section[#:tag "parse-number"]{Reading Numbers}

@guideintro["numbers"]{the syntax of numbers}

@section-index["numbers" "parsing"]

A sequence that does not start with a delimiter is parsed as a @tech{number}
when it matches the following grammar case-insensitively for
@nonterm{number@sub{10}} (decimal), where @metavar{n} is a
meta-meta-variable in the grammar. The resulting number is @tech{interned} in 
@racket[read-syntax] mode.

A number is optionally prefixed by an exactness specifier,
@as-index{@litchar{#e}} (exact) or @as-index{@litchar{#i}} (inexact),
which specifies its parsing as an exact or inexact number; see
@secref["numbers"] for information on number exactness. As the
non-terminal names suggest, a number that has no exactness specifier
and matches only @nunterm{inexact-number} is normally parsed as an
inexact number, otherwise it is parsed as an exact number. If the
@racket[read-decimal-as-inexact] @tech{parameter} is set to @racket[#f], then
all numbers without an exactness specifier are instead parsed as
exact.

If the reader encounters @as-index{@litchar{#b}} (binary),
@as-index{@litchar{#o}} (octal), @as-index{@litchar{#d}} (decimal), or
@as-index{@litchar{#x}} (hexadecimal), it must be followed by a
sequence that is terminated by a delimiter or end-of-file, and that
is either an @tech{extflonum} (see @secref["parse-extflonum"]) or
matches the @nonterm{general-number@sub{2}},
@nonterm{general-number@sub{8}}, @nonterm{general-number@sub{10}}, or
@nonterm{general-number@sub{16}} grammar, respectively.

A @litchar{#e} or @litchar{#i} followed immediately by @litchar{#b},
@litchar{#o}, @litchar{#d}, or @litchar{#x} is treated the same as the
reverse order: @litchar{#b}, @litchar{#o}, @litchar{#d}, or
@litchar{#x} followed by @litchar{#e} or @litchar{#i}.

An @nunterm{exponent-mark} in an inexact number serves both to specify
an exponent and to specify a numerical precision. If single-precision
IEEE floating point is supported (see @secref["numbers"]), the marks
@litchar{f} and @litchar{s} specify single-precision. Otherwise, or
with any other mark, double-precision IEEE floating point is used.
In addition, single- and double-precision specials are distinct;
specials with the @litchar{.0} suffix, like @racket[-nan.0] are
double-precision, whereas specials with the @litchar{.f} suffix are
single-precision.

A @litchar{#} in an @nunterm{inexact} number is the same as
@litchar{0}, but @litchar{#} can be used to suggest
that the digit's actual value is unknown.

@BNF[(list @nunterm{number} @BNF-alt[@nunterm{exact}
                                     @nunterm{inexact}])
     (list @nunterm{exact} @BNF-alt[@nunterm{exact-rational}
                                    @nunterm{exact-complex}])
     (list @nunterm{exact-rational} @BNF-seq[@optional{@nonterm{sign}} @nunterm{unsigned-rational}])
     (list @nunterm{unsigned-rational} @nunterm{unsigned-integer}
                                       @BNF-seq[@nunterm{unsigned-integer} @litchar{/} @nunterm{unsigned-integer}])
     (list @nunterm{exact-integer} @BNF-seq[@optional{@nonterm{sign}} @nunterm{unsigned-integer}])
     (list @nunterm{unsigned-integer} @kleeneplus{@nunterm{digit}})
     (list @nunterm{exact-complex} @BNF-seq[@nunterm{exact-rational} @nonterm{sign} @nunterm{unsigned-rational} @litchar{i}])
     (list @nunterm{inexact} @BNF-alt[@nunterm{inexact-real}
                                      @nunterm{inexact-complex}])
     (list @nunterm{inexact-real} @BNF-seq[@optional{@nonterm{sign}} @nunterm{inexact-normal}]
                                  @BNF-seq[@nonterm{sign} @nunterm{inexact-special}])
     (list @nunterm{inexact-unsigned} @BNF-alt[@nunterm{inexact-normal}
                                               @nunterm{inexact-special}])
     (list @nunterm{inexact-normal} @BNF-seq[@nunterm{inexact-simple} @optional{@nunterm{exp-mark} @nunterm{exact-integer}}])
     (list @nunterm{inexact-simple} @BNF-seq[@nunterm{digits#} @optional{@litchar{.}} @kleenestar{@litchar{#}}]
                                    @BNF-seq[@optional{@nunterm{unsigned-integer}} @litchar{.} @nunterm{digits#}]
                                    @BNF-seq[@nunterm{digits#} @litchar{/} @nunterm{digits#}])
     (list @nunterm{inexact-special} @BNF-alt[@litchar{inf.0} @litchar{nan.0} @litchar{inf.f} @litchar{nan.f}])
     (list @nunterm{digits#} @BNF-seq[@kleeneplus{@nunterm{digit}} @kleenestar{@litchar{#}}])
     (list @nunterm{inexact-complex} @BNF-seq[@optional{@nunterm{inexact-real}} @nonterm{sign} @nunterm{inexact-unsigned} @litchar{i}]
                                     @BNF-seq[@nunterm{inexact-real} @litchar["@"] @nunterm{inexact-real}])


     (list @nonterm{sign} @BNF-alt[@litchar{+}
                                   @litchar{-}])
     (list @nonterm{digit@sub{16}} @BNF-alt[@nonterm{digit@sub{10}} @litchar{a} @litchar{b} @litchar{c} @litchar{d}
                                            @litchar{e} @litchar{f}])
     (list @nonterm{digit@sub{10}} @BNF-alt[@nonterm{digit@sub{8}} @litchar{8} @litchar{9}])
     (list @nonterm{digit@sub{8}} @BNF-alt[@nonterm{digit@sub{2}} @litchar{2} @litchar{3}
                                           @litchar{4} @litchar{5} @litchar{6} @litchar{7}])
     (list @nonterm{digit@sub{2}} @BNF-alt[@litchar{0} @litchar{1}])
     (list @nonterm{exp-mark@sub{16}} @BNF-alt[@litchar{s} @litchar{l}])
     (list @nonterm{exp-mark@sub{10}} @BNF-alt[@nonterm{exp-mark@sub{16}} @litchar{d} @litchar{e} @litchar{f}])
     (list @nonterm{exp-mark@sub{8}} @nonterm{exp-mark@sub{10}})
     (list @nonterm{exp-mark@sub{2}} @nonterm{exp-mark@sub{10}})
     (list @nunterm{general-number} @BNF-seq[@optional{@nonterm{exactness}} @nunterm{number}])
     (list @nonterm{exactness} @BNF-alt[@litchar{#e} @litchar{#i}])
     ]

@reader-examples[
"-1"
"1/2"
"1.0"
"1+2i"
"1/2+3/4i"
"1.0+3.0e7i"
"2e5"
"#i5"
"#e2e5"
"#x2e5"
"#b101"
]

@section[#:tag "parse-extflonum"]{Reading Extflonums}

An @tech{extflonum} has the same syntax as an @nunterm{inexact-real}
that includes an @nunterm{exp-mark}, but with @litchar{t} or
@litchar{T} in place of the @nunterm{exp-mark}. In addition,
@litchar{+inf.t}, @litchar{-inf.t}, @litchar{+nan.t}, @litchar{-nan.t}
are @tech{extflonums}. A @litchar{#b}
(binary), @litchar{#o} (octal), @litchar{#d} (decimal), or
@litchar{#x} (hexadecimal) radix specification can prefix an
extflonum, but @litchar{#i} or @litchar{#e} cannot, and a
extflonum cannot be used to form a @tech{complex number}.  The
@racket[read-decimal-as-inexact] @tech{parameter} has no effect on
extflonum reading.

@section[#:tag "parse-boolean"]{Reading Booleans}

A @as-index{@litchar{#true}}, @as-index{@litchar{#t}},
@as-index{@litchar{#T}} followed by a delimiter is the input syntax
for the @tech{boolean} constant ``true,'' and @as-index{@litchar{#false}},
@as-index{@litchar{#f}}, or @as-index{@litchar{#F}} followed by a
delimiter is the complete input syntax for the @tech{boolean} constant
``false.''


@section[#:tag "parse-pair"]{Reading Pairs and Lists}

When the reader encounters a @as-index{@litchar{(}},
@as-index{@litchar{[}}, or @as-index{@litchar["{"]}, it starts
parsing a @tech{pair} or @tech{list}; see @secref["pairs"] for information on pairs
and lists.

To parse the pair or list, the reader recursively reads data
until a matching @as-index{@litchar{)}}, @as-index{@litchar{]}}, or
@as-index{@litchar["}"]} (respectively) is found, and it specially handles
a delimited @litchar{.}.  Pairs @litchar{()}, @litchar{[]}, and
@litchar{{}} are treated the same way, so the remainder of this
section simply uses ``parentheses'' to mean any of these pair.

If the reader finds no delimited @as-index{@litchar{.}} among the elements
between parentheses, then it produces a list containing the results of
the recursive reads.

If the reader finds two data between the matching parentheses
that are separated by a delimited @litchar{.}, then it creates a
pair. More generally, if it finds two or more data where the
last datum is preceded by a delimited @litchar{.}, then it constructs
nested pairs: the next-to-last element is paired with the last, then
the third-to-last datum is paired with that pair, and so on.

If the reader finds three or more data between the matching
parentheses, and if a pair of delimited @litchar{.}s surrounds any
other than the first and last elements, the result is a list
containing the element surrounded by @litchar{.}s as the first
element, followed by the others in the read order. This convention
supports a kind of @as-index{infix} notation at the reader level.

In @racket[read-syntax] mode, the recursive reads for the pair/list
elements are themselves in @racket[read-syntax] mode, so that the
result is a list or pair of syntax objects that is itself wrapped as a
syntax object. If the reader constructs nested pairs because the input
included a single delimited @litchar{.}, then only the innermost pair
and outermost pair are wrapped as syntax objects. Whether wrapping a
pair or list, if the pair or list was formed with @litchar{[} and
@litchar{]}, then a @indexed-racket['paren-shape] property is attached
to the result with the value @racket[#\[]; if the list or pair was
formed with @litchar["{"] and @litchar["}"], then a
@racket['paren-shape] property is attached to the result with the
value @racket[#\{].

If a delimited @litchar{.} appears in any other configuration, then
the @exnraise[exn:fail:read]. Similarly, if the reader encounters a
@litchar{)}, @litchar{]}, or @litchar["}"] that does not end a list
being parsed, then the @exnraise[exn:fail:read].

@reader-examples[
"()"
"(1 2 3)"
"{1 2 3}"
"[1 2 3]"
"(1 (2) 3)"
"(1 . 3)"
"(1 . (3))"
"(1 . 2 . 3)"
]

If the @racket[read-square-bracket-as-paren] @tech{parameter} is set to
@racket[#f], then when the reader encounters @litchar{[} and
@litchar{]}, the @exnraise{exn:fail:read}. Similarly, if the
@racket[read-curly-brace-as-paren] @tech{parameter} is set to @racket[#f],
then when the reader encounters @litchar["{"] and @litchar["}"], the
@exnraise{exn:fail:read}.

If the @racket[read-accept-dot] @tech{parameter} is set to
@racket[#f], then a delimited @litchar{.} triggers an
@racket[exn:fail:read] exception. If the
@racket[read-accept-infix-dot] @tech{parameter} is set to @racket[#f],
then multiple delimited @litchar{.}s trigger an @racket[exn:fail:read]
exception, instead of the infix conversion.

@section[#:tag "parse-string"]{Reading Strings}

@guideintro["strings"]{the syntax of strings}

@section-index["strings" "parsing"]

When the reader encounters @as-index{@litchar{"}}, it begins parsing
characters to form a @tech{string}. The string continues until it is
terminated by another @litchar{"} (that is not escaped by
@litchar{\}). The resulting string is @tech{interned} in 
@racket[read-syntax] mode.

Within a string sequence, the following escape sequences are
 recognized:

@itemize[

 @item{@as-index{@litchar{\a}}: alarm (ASCII 7)}
 @item{@as-index{@litchar{\b}}: backspace (ASCII 8)}
 @item{@as-index{@litchar{\t}}: tab (ASCII 9)}
 @item{@as-index{@litchar{\n}}: linefeed (ASCII 10)}
 @item{@as-index{@litchar{\v}}: vertical tab (ASCII 11)}
 @item{@as-index{@litchar{\f}}: formfeed (ASCII 12)}
 @item{@as-index{@litchar{\r}}: return (ASCII 13)}
 @item{@as-index{@litchar{\e}}: escape (ASCII 27)}

 @item{@as-index{@litchar{\"}}: double-quotes (without terminating the string)}
 @item{@as-index{@litchar{\'}}: quote (i.e., the backslash has no effect)}
 @item{@as-index{@litchar{\\}}: backslash (i.e., the second is not an escaping backslash)}

 @item{@as-index{@litchar{\}@kleenerange[1 3]{@nonterm{digit@sub{8}}}}:
       Unicode for the octal number specified by @kleenerange[1
       3]{digit@sub{8}} (i.e., 1 to 3 @nonterm{digit@sub{8}}s), where
       each @nonterm{digit@sub{8}} is @litchar{0}, @litchar{1},
       @litchar{2}, @litchar{3}, @litchar{4}, @litchar{5},
       @litchar{6}, or @litchar{7}. A longer form takes precedence
       over a shorter form, and the resulting octal number must be
       between 0 and 255 decimal, otherwise the
       @exnraise[exn:fail:read].}

 @item{@as-index{@litchar{\x}@kleenerange[1
       2]{@nonterm{digit@sub{16}}}}: Unicode for the hexadecimal
       number specified by @kleenerange[1 2]{@nonterm{digit@sub{16}}},
       where each @nonterm{digit@sub{16}} is @litchar{0}, @litchar{1},
       @litchar{2}, @litchar{3}, @litchar{4}, @litchar{5},
       @litchar{6}, @litchar{7}, @litchar{8}, @litchar{9},
       @litchar{a}, @litchar{b}, @litchar{c}, @litchar{d},
       @litchar{e}, or @litchar{f} (case-insensitive). The longer form
       takes precedence over the shorter form.}

 @item{@as-index{@litchar{\u}@kleenerange[1
       4]{@nonterm{digit@sub{16}}}}: like @litchar{\x}, but with up to
       four hexadecimal digits (longer sequences take precedence).
       The resulting hexadecimal number must be a valid argument to
       @racket[integer->char], otherwise the
       @exnraise[exn:fail:read]---unless the encoding continues with
       another @litchar{\u} to form a surrogate-style encoding.}

 @item{@as-index{@litchar{\u}@kleenerange[4
       4]{@nonterm{digit@sub{16}}}@litchar{\u}@kleenerange[4
       4]{@nonterm{digit@sub{16}}}}: like @litchar{\u}, but for two
       hexadecimal numbers, where the first is in the range
       @code{#xD800} to @code{#xDBFF} and the second is in the
       range @code{#xDC00} to @code{#xDFFF}; the resulting
       character is the one represented by the numbers as a UTF-16
       surrogate pair.}

 @item{@as-index{@litchar{\U}@kleenerange[1
       8]{@nonterm{digit@sub{16}}}}: like @litchar{\x}, but with up
       to eight hexadecimal digits (longer sequences take precedence).
       The resulting hexadecimal number must be a valid argument to
       @racket[integer->char], otherwise the
       @exnraise[exn:fail:read].}

 @item{@as-index{@litchar{\}@nonterm{newline}}: elided, where
       @nonterm{newline} is either a linefeed, carriage return, or
       carriage return--linefeed combination. This convention allows
       single-line strings to span multiple lines in the source.}

]

If the reader encounters any other use of a backslash in a string
constant, the @exnraise[exn:fail:read].

@guideintro["bytestrings"]{the syntax of byte strings}

@section-index["byte strings" "parsing"]
@section-index["heredoc"]

A string constant preceded by @litchar{#} is parsed as a
@tech{byte string}. (That is, @as-index{@litchar{#"}} starts a byte-string
literal.) See @secref["bytestrings"] for information on byte
strings. The resulting byte string is @tech{interned} in 
@racket[read-syntax] mode.
Byte-string constants support the same escape sequences as
character strings, except @litchar{\u} and @litchar{\U}. Otherwise, each
character within the byte-string quotes must have a Unicode code-point number
in the range 0 to 255, which is used as the corresponding byte's value; if
a character is not in that range, the @exnraise[exn:fail:read].

When the reader encounters @as-index{@litchar{#<<}}, it starts parsing a
@pidefterm{here string}. The characters following @litchar{#<<} until
a newline character define a terminator for the string. The content of
the string includes all characters between the @litchar{#<<} line and
a line whose only content is the specified terminator. More precisely,
the content of the string starts after a newline following
@litchar{#<<}, and it ends before a newline that is followed by the
terminator, where the terminator is itself followed by either a
newline or end-of-file. No escape sequences are recognized between the
starting and terminating lines; all characters are included in the
string (and terminator) literally. A return character is not treated
as a line separator in this context. If no characters appear between
@litchar{#<<} and a newline or end-of-file, or if an end-of-file is
encountered before a terminating line, the @exnraise[exn:fail:read].

@reader-examples[
"\"Apple\""
"\"\\x41pple\""
"\"\\\"Apple\\\"\""
"\"\\\\\""
"#\"Apple\""
]

@section[#:tag "parse-quote"]{Reading Quotes}

When the reader encounters @as-index{@litchar{'}}, it recursively
reads one datum and forms a new list containing the @tech{symbol}
@racket['quote] and the following datum. This convention is mainly
useful for reading Racket code, where @racket['s] can be used as a
shorthand for @racket[(code:quote s)].

Several other sequences are recognized and transformed in a similar
way. Longer prefixes take precedence over short ones:

@read-quote-table[(list @litchar{'} @racket[quote])
                  (list @as-index{@litchar{`}} @racket[quasiquote])
                  (list @as-index{@litchar{,}} @racket[unquote])
                  (list @as-index{@litchar[",@"]} @racket[unquote-splicing])
                  (list @as-index{@litchar{#'}} @racket[syntax])
                  (list @as-index{@litchar{#`}} @racket[quasisyntax])
                  (list @as-index{@litchar{#,}} @racket[unsyntax])
                  (list @as-index{@litchar["#,@"]} @racket[unsyntax-splicing])]

@reader-examples[
"'apple"
"`(1 ,2)"
]

The @litchar{`}, @litchar{,}, and @litchar[",@"] forms are disabled when
the @racket[read-accept-quasiquote] @tech{parameter} is set to
@racket[#f], in which case the @exnraise[exn:fail:read] instead.

@section[#:tag "parse-comment"]{Reading Comments}

A @as-index{@litchar{;}} starts a line comment. When the reader
encounters @litchar{;}, it skips past all characters until the
next linefeed (ASCII 10), carriage return (ASCII 13), next-line
(Unicode @racket[#x0085]), line-separator (Unicode @racket[#x2028]),
or paragraph-separator (Unicode @racket[#x2029]) character.

A @as-index{@litchar{#|}} starts a nestable block comment.  When the
reader encounters @litchar{#|}, it skips past all characters
until a closing @litchar{|#}. Pairs of matching @litchar{#|} and
@litchar{|#} can be nested.

A @as-index{@litchar{#;}} starts an S-expression comment. When the
reader encounters @litchar{#;}, it recursively reads one datum, and
then discards it (continuing on to the next datum for the read
result).

A @as-index{@litchar{#! }} (which is @litchar{#!} followed by a space)
or @as-index{@litchar{#!/}} starts a line comment that can be
continued to the next line by ending a line with @litchar{\}. This
form of comment normally appears at the beginning of a Unix script
file.

@reader-examples[
"; comment"
"#| a |# 1"
"#| #| a |# 1 |# 2"
"#;1 2"
"#!/bin/sh"
"#! /bin/sh"
]

@section[#:tag "parse-vector"]{Reading Vectors}

When the reader encounters a @litchar{#(}, @litchar{#[}, or
@litchar["#{"], it starts parsing a @tech{vector}; see @secref["vectors"] for
information on vectors. A @litchar{#fl} in place of @litchar{#}
starts an @tech{flvector}, but is not allowed in @racket[read-syntax] mode; 
see @secref["flvectors"] for information on flvectors.
A @litchar{#fx} in place of @litchar{#}
starts an @tech{fxvector}, but is not allowed in @racket[read-syntax] mode; 
see @secref["fxvectors"] for information on fxvectors.
The @litchar{#[}, @litchar["#{"], @litchar{#fl[}, @litchar["#fl{"], 
@litchar{#fx[}, and @litchar["#fx{"] forms can be disabled through 
the @racket[read-square-bracket-as-paren] and
@racket[read-curly-brace-as-paren] @tech{parameters}.

The elements of the vector are recursively read until a matching
@litchar{)}, @litchar{]}, or @litchar["}"] is found, just as for
lists (see @secref["parse-pair"]). A delimited @litchar{.} is not
allowed among the vector elements. In the case of @tech{flvectors},
the recursive read for element is implicitly prefixed with @litchar{#i}
and must produce a @tech{flonum}. In the case of @tech{flvectors},
the recursive read for element is implicitly prefixed with @litchar{#e}
and must produce a @tech{fixnum}.

An optional vector length can be specified between @litchar{#}, @litchar{#fl}, @litchar{#fx}  and
@litchar{(}, @litchar{[}, or @litchar["{"]. The size is specified
using a sequence of decimal digits, and the number of elements
provided for the vector must be no more than the specified size. If
fewer elements are provided, the last provided element is used for the
remaining vector slots; if no elements are provided, then @racket[0]
is used for all slots.

In @racket[read-syntax] mode, each recursive read for vector
elements is also in @racket[read-syntax] mode, so that the wrapped
vector's elements are also wrapped as syntax objects, and the vector is
immutable.

@reader-examples[
"#(1 apple 3)"
"#3(\"apple\" \"banana\")"
"#3()"
]


@section[#:tag "parse-structure"]{Reading Structures}

When the reader encounters a @litchar{#s(}, @litchar{#s[}, or
@litchar["#s{"], it starts parsing an instance of a @tech{prefab}
@tech{structure type}; see @secref["structures"] for information on
@tech{structure types}.  The @litchar{#s[} and @litchar["#s{"] forms
can be disabled through the @racket[read-square-bracket-as-paren] and
@racket[read-curly-brace-as-paren] @tech{parameters}.

The elements of the structure are recursively read until a matching
@litchar{)}, @litchar{]}, or @litchar["}"] is found, just as for lists
(see @secref["parse-pair"]). A single delimited @litchar{.} is not
allowed among the elements, but two @litchar{.}s can be used as in a
list for an infix conversion.

The first element is used as the structure descriptor, and it must
have the form (when quoted) of a possible argument to
@racket[make-prefab-struct]; in the simplest case, it can be a
symbol. The remaining elements correspond to field values within the
structure.

In @racket[read-syntax] mode, the structure type must not have any
mutable fields. The structure's elements are read in
@racket[read-syntax] mode, so that the wrapped structure's elements
are also wrapped as syntax objects.

If the first structure element is not a valid @tech{prefab} structure
type key, or if the number of provided fields is inconsistent with the
indicated @tech{prefab} structure type, the @exnraise[exn:fail:read].


@section[#:tag "parse-hashtable"]{Reading Hash Tables}

A @as-index{@litchar{#hash}} starts an immutable @tech{hash-table} constant
with key matching based on @racket[equal?]. The characters after
@litchar{hash} must parse as a list of pairs (see
@secref["parse-pair"]) with a specific use of delimited @litchar{.}:
it must appear between the elements of each pair in the list and
nowhere in the sequence of list elements. The first element of each
pair is used as the key for a table entry, and the second element of
each pair is the associated value.

A @as-index{@litchar{#hasheq}} starts a hash table like
@litchar{#hash}, except that it constructs a hash table based on
@racket[eq?] instead of @racket[equal?].

A @as-index{@litchar{#hasheqv}} starts a hash table like
@litchar{#hash}, except that it constructs a hash table based on
@racket[eqv?] instead of @racket[equal?].

In all cases, the table is constructed by adding each mapping to the
 hash table from left to right, so later mappings can hide earlier
 mappings if the keys are equivalent.

@reader-examples[
#:example-note @elem{, where @racket[make-...] stands for @racket[make-immutable-hash]}
"#hash()"
"#hasheq()"
"#hash((\"a\" . 5))"
"#hasheq((a . 5) (b . 7))"
"#hasheq((a . 5) (a . 7))"
]

@section[#:tag "parse-box"]{Reading Boxes}

When the reader encounters a @as-index{@litchar{#&}}, it starts
parsing a @tech{box}; see @secref["boxes"] for information on boxes. The
content of the box is determined by recursively reading the next
datum.

In @racket[read-syntax] mode, the recursive read for the box content
is also in @racket[read-syntax] mode, so that the wrapped box's
content is also wrapped as a syntax object, and the box is immutable.

@reader-examples[
"#&17"
]

@section[#:tag "parse-character"]{Reading Characters}

@guideintro["characters"]{the syntax of characters}

A @as-index{@litchar{#\}} starts a @tech{character} constant, which has
one of the following forms:

@itemize[

 @item{ @litchar{#\nul} or @litchar{#\null}: NUL (ASCII 0)@nonalpha[]}
 @item{ @litchar{#\backspace}: backspace  (ASCII 8)@nonalpha[]}
 @item{ @litchar{#\tab}: tab (ASCII 9)@nonalpha[]}
 @item{ @litchar{#\newline} or @litchar{#\linefeed}: linefeed (ASCII 10)@nonalpha[]}
 @item{ @litchar{#\vtab}: vertical tab (ASCII 11)@nonalpha[]}
 @item{ @litchar{#\page}: page break (ASCII 12)@nonalpha[]}
 @item{ @litchar{#\return}: carriage return (ASCII 13)@nonalpha[]}
 @item{ @litchar{#\space}: space (ASCII 32)@nonalpha[]}
 @item{ @litchar{#\rubout}: delete (ASCII 127)@nonalpha[]}

 @item{@litchar{#\}@kleenerange[3 3]{@nonterm{digit@sub{8}}}:
       Unicode for the octal number specified by three octal digits---as in string escapes (see
       @secref["parse-string"]), but constrained to exactly three digits.}

@;{
 Not implemented:
 @item{@litchar{#\x}@kleenerange[1 2]{@nonterm{digit@sub{16}}}:
       Unicode for the hexadecimal number specified by @kleenerange[1
       2]{@nonterm{digit@sub{16}}}, as in string escapes (see
       @secref["parse-string"]).}
}

 @item{@litchar{#\u}@kleenerange[1 4]{@nonterm{digit@sub{16}}}:
       Unicode for the hexadecimal number specified by @kleenerange[1
       4]{@nonterm{digit@sub{16}}}, as in string escapes (see
       @secref["parse-string"]).}

 @item{@litchar{#\U}@kleenerange[1 6]{@nonterm{digit@sub{16}}}:
       like @litchar{#\u}, but with up to six hexadecimal digits.}

 @item{@litchar{#\}@nonterm{c}: the character @nonterm{c}, as long
       as @litchar{#\}@nonterm{c} and the characters following it
       do not match any of the previous cases, and as long as the
       character after @nonterm{c} is not
       @racketlink[char-alphabetic?]{alphabetic}.}

]

@reader-examples[
"#\\newline"
"#\\n"
"#\\u3BB"
"#\\\u3BB"
]

@section[#:tag "parse-keyword"]{Reading Keywords}

A @as-index{@litchar{#:}} starts a @tech{keyword}. The parsing of a keyword
after the @litchar{#:} is the same as for a symbol, including
case-folding in case-insensitive mode, except that the part after
@litchar{#:} is never parsed as a number. The resulting keyword is 
@tech{interned}. 

@reader-examples[
"#:Apple"
"#:1"
]

@section[#:tag "parse-regexp"]{Reading Regular Expressions}

A @as-index{@litchar{#rx}} or @as-index{@litchar{#px}} starts a
@tech{regular expression}. The characters immediately after @litchar{#rx} or
@litchar{#px} must parse as a string or byte string (see
@secref["parse-string"]). A @litchar{#rx} prefix starts a regular
expression as would be constructed by @racket[regexp], @litchar{#px}
as constructed by @racket[pregexp], @litchar{#rx#} as constructed by
@racket[byte-regexp], and @litchar{#px#} as constructed by
@racket[byte-pregexp]. The resulting regular expression is @tech{interned} in 
@racket[read-syntax] mode. 

@reader-examples[
"#rx\".*\""
"#px\"[\\\\s]*\""
"#rx#\".*\""
"#px#\"[\\\\s]*\""
]

@section[#:tag "parse-graph"]{Reading Graph Structure}

@section-index["#0="]
@section-index["#0#"]

A @graph-defn[] tags the following datum for reference via
@graph-ref[], which allows the reader to produce a datum that
has graph structure.

For a specific @graph-tag[] in a single read result, each @graph-ref[]
reference is replaced by the datum read for the corresponding
@graph-defn[]; the definition @graph-defn[] also produces just the
datum after it. A @graph-defn[] definition can appear at most once,
and a @graph-defn[] definition must appear before a @graph-ref[]
reference appears, otherwise the @exnraise[exn:fail:read]. If the
@racket[read-accept-graph] parameter is set to @racket[#f], then
@graph-defn[] or @graph-ref[] triggers a @racket[exn:fail:read]
exception.

Although a comment parsed via @litchar{#;} discards the datum
afterward, @graph-defn[] definitions in the discarded datum
still can be referenced by other parts of the reader input, as long as
both the comment and the reference are grouped together by some other
form (i.e., some recursive read); a top-level @litchar{#;} comment
neither defines nor uses graph tags for other top-level forms.

@reader-examples[
"(#1=100 #1# #1#)"
"#0=(1 . #0#)"
]

@local-table-of-contents[]

@section[#:tag "parse-reader"]{Reading via an Extension}

@guideintro["hash-reader"]{reader extension}

When the reader encounters @as-index{@litchar{#reader}}, it loads
an external reader procedure and applies it to the current input
stream.

The reader recursively reads the next datum after @litchar{#reader},
and passes it to the procedure that is the value of the
@racket[current-reader-guard] @tech{parameter}; the result is used as a
module path. The module path is passed to @racket[dynamic-require]
with either @racket['read] or @racket['read-syntax] (depending on
whether the reader is in @racket[read] or @racket[read-syntax]
mode). The module is loaded in a @tech{root namespace} of the
@tech{current namespace}.

The arity of the resulting procedure determines whether it accepts
extra source-location information: a @racketidfont{read} procedure
accepts either one argument (an input port) or five, and a
@racketidfont{read-syntax} procedure accepts either two arguments (a
name value and an input port) or six. In either case, the four
optional arguments are the reader's module path (as a syntax object in
@racket[read-syntax] mode) followed by the line (positive exact
integer or @racket[#f]), column (non-negative exact integer or
@racket[#f]), and position (positive exact integer or @racket[#f]) of
the start of the @litchar{#reader} form. The input port is the one
whose stream contained @litchar{#reader}, where the stream position is
immediately after the recursively read module path.

The procedure should produce a datum result.  If the result is a
syntax object in @racket[read] mode, then it is converted to a datum
using @racket[syntax->datum]; if the result is not a syntax object in
@racket[read-syntax] mode, then it is converted to one using
@racket[datum->syntax]. See also @secref["reader-procs"] for
information on the procedure's results.

If the @racket[read-accept-reader] @tech{parameter} is set to
@racket[#f], then if the reader encounters @litchar{#reader}, the
@exnraise[exn:fail:read].

@guideintro["hash-lang"]{@racketmodfont["#lang"]}

The @as-index{@litchar{#lang}} reader form is similar to
@litchar{#reader}, but more constrained: the @litchar{#lang} must be
followed by a single space (ASCII 32), and then a non-empty sequence
of alphanumeric ASCII, @litchar{+}, @litchar{-}, @litchar{_}, and/or
@litchar{/} characters terminated by
@racketlink[char-whitespace?]{whitespace} or an end-of-file.  The
sequence must not start or end with @litchar{/}. A sequence
@litchar{#lang }@nonterm{name} is equivalent to either
@litchar{#reader (submod }@nonterm{name}@litchar{ reader)} or
@litchar{#reader }@nonterm{name}@litchar{/lang/reader}, where the
former is tried first guarded by a @racket[module-declared?] 
check (but after filtering by
@racket[current-reader-guard], so both are passed to the
value of @racket[current-reader-guard] if the latter is used). Note
that the terminating whitespace (if any) is not consumed before the
external reading procedure is called.

@guideintro["hash-languages"]{the creation languages for @hash-lang[]}

Finally, @as-index{@litchar{#!}} is an alias for @litchar{#lang}
followed by a space when @litchar{#!} is followed by alphanumeric
ASCII, @litchar{+}, @litchar{-}, or @litchar{_}. Use of this alias
is discouraged except as needed to construct programs that conform to
certain grammars, such as that of R@superscript{6}RS
@cite["Sperber07"].

@margin-note{The @racketmodname[syntax/module-reader] library provides a
             domain-specific language for writing language readers.}

By convention, @litchar{#lang} normally appears at the beginning of a
file, possibly after comment forms, to specify the syntax of a module.

If the @racket[read-accept-reader] or @racket[read-accept-lang]
@tech{parameter} is set to @racket[#f], then if the reader encounters
@litchar{#lang} or equivalent @litchar{#!}, the @exnraise[exn:fail:read].

@subsection{S-Expression Reader Language}

@defmodulelang[s-exp]

@guideintro["s-exp"]{the @racketmodname[s-exp] meta-language}

The @racket[s-exp] ``language'' is a kind of meta-language. It
@racket[read]s the S-expression that follows @litchar{#lang s-exp} and
uses it as the language of a @racket[module] form. It also reads all
remaining S-expressions until an end-of-file, using them for the body
of the generated @racket[module].

That is,

@racketmod[
s-exp _module-path
_form ...
]

is equivalent to

@racketblock[
(module _name-id _module-path
  _form ...)
]

where @racket[_name-id] is derived from the source input port's name:
if the port name is a filename path, the filename without its
directory path and extension is used for @racket[_name-id], otherwise
@racket[_name-id] is @racket[anonymous-module].

@subsection{Chaining Reader Language}

@defmodulelang[reader]

@guideintro["hash-lang reader"]{the @racketmodname[reader] meta-language}

The @racket[reader] ``language'' is a kind of meta-language. It
@racket[read]s the S-expression that follows @litchar{#lang reader}
and uses it as a module path (relative to the module being read) that
effectively takes the place of @racketmodname[reader]. In other words,
the @racketmodname[reader] meta-language generalizes the syntax of the
module specified after @hash-lang[] to be a module path, and without
the implicit addition of @litchar{/lang/reader} to the path.
