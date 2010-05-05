#lang scribble/doc
@(require scribble/bnf
          "mz.ss")

@title[#:tag "printing" #:style 'quiet]{The Printer}

The default printer generally prints core datatypes in such a way that
using @scheme[read] on the output produces a value that is
@scheme[equal?] to the printed value---when the printed is used in
@scheme[write]. When the printer is used in @scheme[display] mode, the
printing of strings, byte strings, characters, and symbols changes to
render the character/byte content directly to the output port. The
printer's @scheme[print] mode is similar to @scheme[write], but it is
sensitive to the @scheme[print-as-quasiquote] parameter for printing
values in a way that @scheme[read] plus @scheme[eval] on the output
can be @scheme[equal?] to the printed value.

When the @scheme[print-graph] parameter is set to @scheme[#t], then
the printer first scans an object to detect cycles. The scan traverses
the components of pairs, mutable pairs, vectors, boxes (when
@scheme[print-box] is @scheme[#t]), hash tables (when
@scheme[print-hash-table] is @scheme[#t]), and fields of structures
exposed by @scheme[struct->vector] (when @scheme[print-struct] is
@scheme[#t]). If @scheme[print-graph] is @scheme[#t], then this
information is used to display sharing by printing graph definitions
and references (see @secref["parse-graph"]). If a cycle is detected in
the initial scan, then @scheme[print-graph] is effectively set to
@scheme[#t] automatically.

With the exception of displaying byte strings, printing is defined in
terms of Unicode characters; see @secref["ports"] for information
on how a character stream is written to an port's underlying byte
stream.

@section[#:tag "print-symbol"]{Printing Symbols}

Symbols containing spaces or special characters @scheme[write] using
escaping @litchar{\} and quoting @litchar{|}s. When the
@scheme[read-case-sensitive] parameter is set to @scheme[#f], then
symbols containing uppercase characters also use escaping escaping
@litchar{\} and quoting @litchar{|}s. In addition, symbols are
quoted with @litchar{|}s or leading @litchar{\} when they would
otherwise print the same as a numerical constant or as a delimited
@litchar{.} (when @scheme[read-accept-dot] is @scheme[#t]).

When @scheme[read-accept-bar-quote] is @scheme[#t], @litchar{|}s are
used in printing when one @litchar{|} at the beginning and one
@litchar{|} at the end suffices to correctly print the
symbol. Otherwise, @litchar{\}s are always used to escape special
characters, instead of quoting them with @litchar{|}s.

When @scheme[read-accept-bar-quote] is @scheme[#f], then @litchar{|}
is not treated as a special character. The following are always
special characters:

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar{[} @litchar{]}
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{\}
}

In addition, @litchar{#} is a special character when it appears at the
beginning of the symbol, and when it is not followed by @litchar{%}.

Symbols @scheme[display] without escaping or quoting special
characters. That is, the display form of a symbol is the same as the
display form of @scheme[symbol->string] applied to the symbol.

Symbols @scheme[print] the same as they @scheme[write], unless
@scheme[print-as-quasiquote] is set to @scheme[#t] and the current
@scheme[quasiquote] depth is @scheme[0]. In that case, the symbol's
@scheme[print]ed form is prefixed with @litchar{'}. If the current
@scheme[quasiquote] depth is @scheme[1], and if the symbol is
@scheme['unquote] or @scheme[quasiquote], then the @scheme[print]ed
form is prefixed with @litchar{,'}.

@section{Printing Numbers}

A number prints the same way in @scheme[write], @scheme[display], and
@scheme[print] modes.

A @tech{complex number} that is not a @tech{real number} always prints
as @nonterm{m}@litchar{+}@nonterm{n}@litchar{i}, where @nonterm{m} and
@nonterm{n} are the printed forms of its real and imaginary parts,
respectively.

An inexact real number prints with either a @litchar{.} decimal
point, an @litchar{e} exponent marker, or both.  The form is selected
so that the output is as short as possible, with the constraint that
reading the printed form back in produces an @scheme[equal?] number.

An exact @scheme[0] prints as @litchar{0}.

A positive, exact integer prints as a sequence of decimal digits that
does not start with @scheme[0].

A positive, exact, real, non-integer number prints as
@nonterm{m}@litchar{/}@nonterm{n}, where @nonterm{m} and @nonterm{n}
are the printed forms of the number's numerators and denominator (as
determined by @scheme[numerator] and @scheme[denominator]).

A negative @tech{exact number} prints with a @litchar{-} prefix on the
printed form of its exact negation.

@section{Printing Booleans}

The constant @scheme[#t] prints as @litchar{#t}, and the constant
@scheme[#f] prints as @litchar{#f} in all modes (@scheme[display],
@scheme[write], and @scheme[print]).

@section[#:tag "print-pairs"]{Printing Pairs and Lists}

In @scheme[write] and @scheme[display] modes, an empty list prints as
@litchar{()}. A pair normally prints starting with @litchar{(}
followed by the printed form of its @scheme[car]. The rest of the
printed form depends on the @scheme[cdr]:

@itemize[

 @item{If the @scheme[cdr] is a pair or the empty list, then the
       printed form of the pair completes with the printed form of the
       @scheme[cdr], except that the leading @litchar{(} in the
       @scheme[cdr]'s printed form is omitted.}

 @item{Otherwise, the printed for of the pair continues with a space,
       @litchar{.}, another space, the printed form of the
       @scheme[cdr], and a @litchar{)}.}

]

If @scheme[print-reader-abbreviations] is set to @scheme[#t], then
pair printing is adjusted in the case of a pair that starts a
two-element list whose first element is @scheme[quote],
@scheme['quasiquote], @scheme['unquote], @scheme['unquote-splicing],
@scheme['syntax], @scheme['quasisyntax], @scheme['unsyntax],
@scheme['unsyntax-splicing]. In that case, the pair is printed with
the corresponding reader syntax: @litchar{'}, @litchar{`},
@litchar{,}, @litchar[",@"], @litchar{#'}, @litchar{#`}, @litchar{#,},
or @litchar["#,@"], respectively. After the reader syntax, the second
element of the list is printed. When the list is a tail of an
enclosing list, the tail is printed after a @litchar{.} in the
enclosing list (after which the reader abbreviations work), instead of
including the tail as two elements of the enclosing list.

The printed form of a pair is the same in both @scheme[write] and
@scheme[display] modes, except as the printed form of the pair's
@scheme[car] and @scheme[cdr] vary with the mode. The @scheme[print]
form is also the same is @scheme[print-as-quasiquote] is @scheme[#f].

When @scheme[print-as-quasiquote] is @scheme[#t] and the current
@scheme[quasiquote] depth is @scheme[0], then the empty list prints as
@litchar{'()} and a pair's output is prefixed with @litchar{`}; the
pair's content is printed at @scheme[quasiquote] depth is
@scheme[1]. In addition, when @scheme['quasiquote], @scheme['unquote],
or @scheme['unquote-splicing] appears as the first element of a
two-element list, the @scheme[quasiquote] depth is adjusted
appropriately for printing the second element of the list.

By default, mutable pairs (as created with @scheme[mcons]) print the
same as pairs, except that @litchar["{"] and @litchar["}"] are used
instead of @litchar{(} and @litchar{)}. Note that the reader
treats @litchar["{"]...@litchar["}"] and @litchar{(}...@litchar{)}
equivalently on input, creating immutable pairs in both cases.

If the @scheme[print-pair-curly-braces] parameter is set to
@scheme[#t], then immutable pairs print using @litchar["{"] and
@litchar["}"].  If the @scheme[print-mpair-curly-braces] parameter is
set to @scheme[#f], then mutable pairs print using @litchar{(} and
@litchar{)}.

@section{Printing Strings}

All strings @scheme[display] as their literal character sequences.

The @scheme[write] or @scheme[print] form of a string starts with @litchar{"} and ends
with another @litchar{"}. Between the @litchar{"}s, each character is
represented. Each graphic or blank character is represented as itself,
with two exceptions: @litchar{"} is printed as @litchar{\"}, and
@litchar{\} is printed as @litchar{\\}. Each non-graphic, non-blank
character (according to @scheme[char-graphic?] and
@scheme[char-blank?]) is printed using the escape sequences described
in @secref["parse-string"], using @litchar{\a}, @litchar{\b},
@litchar{\t}, @litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r},
or @litchar{\e} if possible, otherwise using @litchar{\u} with four
hexadecimal digits or @litchar{\U} with eight hexadecimal digits
(using the latter only if the character value does not fit into four
digits).

All byte strings @scheme[display] as their literal byte sequence; this
byte sequence may not be a valid UTF-8 encoding, so it may not
correspond to a sequence of characters.

The @scheme[write] or @scheme[print] form a byte string starts with @litchar{#"} and
ends with another @litchar{"}. Between the @litchar{"}s, each byte is
written using the corresponding ASCII decoding if the byte is between
0 and 127 and the character is graphic or blank (according to
@scheme[char-graphic?] and @scheme[char-blank?]). Otherwise, the byte
is written using @litchar{\a}, @litchar{\b}, @litchar{\t},
@litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r}, or
@litchar{\e} if possible, otherwise using @litchar{\} followed by one
to three octal digits (only as many as necessary).

@section[#:tag "print-vectors"]{Printing Vectors}

In @scheme[display] mode, the printed form of a vector is @litchar{#}
followed by the printed form of @scheme[vector->list] applied to the
vector. In @scheme[write] mode, the printed form is the same, except
that when the @scheme[print-vector-length] parameter is @scheme[#t], a
decimal integer is printed after the @litchar{#}, and a repeated last
element is printed only once.

Vectors @scheme[print] the same as they @scheme[write], unless
@scheme[print-as-quasiquote] is set to @scheme[#t] and the current
@scheme[quasiquote] depth is @scheme[0]. In that case, the vector's
@scheme[print]ed form is prefixed with @litchar{`}, and its content is
printed with @scheme[quasiquote] depth @scheme[1].


@section[#:tag "print-structure"]{Printing Structures}

When the @scheme[print-struct] parameter is set to @scheme[#t], then
the way that structures print depends on details of the structure type
for which the structure is an instance:

@itemize[

 @item{If the structure type is a @techlink{prefab} structure type,
       then it prints using @litchar{#s(} followed by the @tech{prefab}
       structure type key, then the printed form each field in the
       structure, and then @litchar{)}.

       In @scheme[print] mode when @scheme[print-as-quasiquote] is set
       to @scheme[#t] and the current @scheme[quasiquote] depth is
       @scheme[0], the structure's @scheme[print]ed form is prefixed
       with @litchar{`} and its content is printed with
       @scheme[quasiquote] depth @scheme[1].}

 @item{If the structure has a @scheme[prop:custom-write] property
       value, then the associated procedure is used to print the
       structure, unless the @racket[print-unreadable] parameter is
       set to @racket[#f].}

 @item{If the structure type is transparent, or if any ancestor is
       transparent, then the structure prints as the vector produced
       by @scheme[struct->vector] in @scheme[display] mode, in
       @scheme[write] mode, or in @scheme[print] mode when
       @scheme[print-as-quasiquote] is set to @scheme[#f].

       In @scheme[print] mode with @scheme[print-as-quasiquote] as
       @scheme[#t], then the printed form is prefixed with as many
       @litchar{,}s as the current @scheme[quasiquote] depth. Instead
       of printing as a vector, the structure content is printed as a
       list, where the first element is the list is the structure's
       type name (as determined by @scheme[object-name]) printed in
       @scheme[write] mode, while the remaining elements are
       @scheme[print]ed at @scheme[quasiquote] depth @scheme[0].}

 @item{For any other structure type, the structure prints as an
       unreadable value; see @secref["print-unreadable"] for more
       information.}
]

If the @scheme[print-struct] parameter is set to @scheme[#f], then all
structures without a @scheme[prop:custom-write] property print as
unreadable values.


@section[#:tag "print-hashtable"]{Printing Hash Tables}

When the @scheme[print-hash-table] parameter is set to @scheme[#t], a
hash table prints starting with @litchar{#hash(}, @litchar{#hasheqv(}, or @litchar{#hasheq(}
for a table using @scheme[equal?], @scheme[eqv?], or @scheme[eq?] key comparisons,
respectively. After this prefix, each key--value mapping is shown as
@litchar{(}, the printed form of a key, a space, @litchar{.}, a space,
the printed form the corresponding value, and @litchar{)}, with an
additional space if the key--value pair is not the last to be printed.
After all key-value pairs, the printed form completes with
@litchar{)}.

In @scheme[print] mode when @scheme[print-as-quasiquote] is
@scheme[#t] and the current quasiquote depth is @scheme[0], then the
printed form is prefixed with @litchar{`} and the hash table's content
is printed at @scheme[quasiquote] depth @scheme[1]. In the printed
form, keys may be printed with @litchar{,} escapes, even though
@scheme[quasiquote] does not support @scheme[unquote] escapes in the
key position.

When the @scheme[print-hash-table] parameter is set to @scheme[#f], a
hash table prints (un@scheme[read]ably) as @litchar{#<hash>}.

@section[#:tag "print-box"]{Printing Boxes}

When the @scheme[print-box] parameter is set to @scheme[#t], 
a box prints as @litchar{#&} followed by the printed form of its content.
In @scheme[print] mode when @scheme[print-as-quasiquote] is
@scheme[#t] and the current quasiquote depth is @scheme[0], then the
printed form is prefixed with @litchar{`} and the box's content
is printed at @scheme[quasiquote] depth @scheme[1].

When the @scheme[print-box] parameter is set to @scheme[#f], a box
prints (un@scheme[read]ably) as @litchar{#<box>}.

@section{Printing Characters}

Characters with the special names described in
@secref["parse-character"] @scheme[write] and @scheme[print] using the same name.
(Some characters have multiple names; the @scheme[#\newline] and
@scheme[#\nul] names are used instead of @scheme[#\linefeed] and
@scheme[#\null]).  Other graphic characters (according to
@scheme[char-graphic?]) @scheme[write] as @litchar{#\} followed by the
single character, and all others characters are written in
@scheme[#\u] notation with four digits or @scheme[#\U] notation with
eight digits (using the latter only if the character value it does not
fit in four digits).

All characters @scheme[display] directly as themselves (i.e., a single
character).

@section{Printing Keywords}

Keywords @scheme[write], @scheme[print], and @scheme[display] the same as symbols,
except (see @secref["print-symbol"]) with a leading @litchar{#:} (after any 
@litchar{'} prefix added in @scheme[print] mode),
and without special handing for an initial @litchar{#} or when the
printed form would matches a number or a delimited @litchar{.} (since
@litchar{#:} distinguishes the keyword).

@section{Printing Regular Expressions}

Regexp values in all modes (@scheme[write], @scheme[display], and @scheme[print])
starting with @litchar{#px} (for @scheme[pregexp]-based regexps) or
@litchar{#rx} (for @scheme[regexp]-based regexps) followed by the
@scheme[write] form of the regexp's source string or byte string.


@section[#:tag "print-unreadable"]{Printing Unreadable Values}

For any value with no other printing specification, assuming that the
@racket[print-unreadable] parameter is set to @racket[#t], the output
form is @litchar{#<}@nonterm{something}@litchar{>}, where
@nonterm{something} is specific to the type of the value and sometimes
to the value itself. If @racket[print-unreadable] is set to
@racket[#f], then attempting to print an unreadable value raises
@racket[exn:fail].
