#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "mz:printing" #:style 'quiet]{The Printer}

The default printer generally prints core datatypes in such a way that
using @scheme[read] on the output produces a value that is
@scheme[equal?] to the printed value---when the printed is used in
@scheme[write]. When the printer is used in @scheme[display] mode, the
printing of strings, byte strings, characters, and symbols changes to
render the character/byte content directly to the output port.

When the @scheme[print-graph] parameter is set to @scheme[#t], then
the printer first scans an object to detect cycles. The scan traverses
the components of pairs, vectors, boxes (when @scheme[print-box] is
@scheme[#t]), hash tables (when @scheme[print-hash-table] is
@scheme[#t]), and fields of structures exposed by
@scheme[struct->vector] (when @scheme[print-struct] is
@scheme[#t]). If @scheme[print-graph] is @scheme[#t], then this
information is used to display sharing by printing graph definitions
and references (see @secref["mz:parse-graph"]). If a cycle is detected
in the initial scan, then @scheme[print-graph] is effectively set to
@scheme[#t] automatically.

With the exception of displaying byte strings, printing is defined in
terms of Unicode characters; see @secref["mz:ports"] for information
on how a character stream is written to an port's underlying byte
stream.

@section[#:tag "mz:print-symbol"]{Printing Symbols}

Symbols containing spaces or special characters @scheme[write] using
escaping @litchar["\\"] and quoting @litchar{|}s. When the
@scheme[read-case-sensitive] parameter is set to @scheme[#f], then
symbols containing uppercase characters also use escaping escaping
@litchar["\\"] and quoting @litchar{|}s. In addition, symbols are
quoted with @litchar{|}s or leading @litchar["\\"] when they would
otherwise print the same as a numerical constant or as a delimited
@scheme{.} (when @scheme[read-accept-dot] is @scheme[#t]).

When @scheme[read-accept-bar-quote] is @scheme[#t], @litchar{|}s are
used in printing when one @litchar{|} at the beginning and one
@litchar{|} at the end suffices to correctly print the
symbol. Otherwise, @litchar["\\"]s are always used to escape special
characters, instead of quoting them with @litchar{|}s.

When @scheme[read-accept-bar-quote] is @scheme[#f], then @litchar{|}
is not treated as a special character. The following are always
special characters:

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["["] @litchar["]"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{\}
}

In addition, @litchar{#} is a special character when it appears at the
beginning of the symbol, and when it is not followed by @litchar{%}.

Symbols @scheme[display] without escaping or quoting special
characters. That is, the display form of a symbol is the same as the
display form of @scheme[symbol->string] applied to the symbol.

@section{Printing Numbers}

A number prints the same way in @scheme[write] and @scheme[display]
modes.

A @tech{complex number} that is not a @tech{real number} always prints
as @nonterm{m}@litchar{+}@nonterm{n}@litchar{i}, where @nonterm{m} and
@nonterm{n} are the printed forms of its real and imaginary parts,
respectively.

An inexact real number prints with either a @litchar{.}  decimal
point, an @litchar{e} exponent marker, or both.  The form is selected
so that the output is as short as possible, with the constraint that
reading the printed form back in produces an @scheme{equal?} number.

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
@scheme[#f] prints as @litchar{#f} in both @scheme[display] and
@scheme[write] modes.

@section{Printing Pairs and Lists}

A pair prints starting with @litchar{(} followed by the printed form
of its @scheme[car]. The rest of the printed form depends on the
@scheme[cdr]:

@itemize{

 @item{If the @scheme[cdr] is a pair or the empty list, then the
       printed form of the pair completes with the printed form of the
       @scheme[cdr], except that the leading @litchar{(} in the
       @scheme[cdr]'s printed form is omitted.}

 @item{Otherwise, the printed for of the pair continues with a space,
       @litchar{.}, another space, the printed form of the
       @scheme[cdr], and a @litchar{)}.}

}

The printed form of a pair is the same in both @scheme[write] and
@scheme[display] modes, except as the printed form of the pair's
@scheme[car]and @scheme[cdr] vary with the mode.

@section{Printing Strings}

All strings @scheme[display] as their literal character sequences.

The @scheme[write] form of a string starts with @litchar{"} and ends
with another @litchar{"}. Between the @litchar{"}s, each character is
represented. Each graphic or blank character is represented as itself,
with two exceptions: @litchar{"} is printed as @litchar{\"}, and
@litchar{\} is printed as @litchar{\\}. Each non-graphic, non-blank
character (according to @scheme[char-graphic?] and
@scheme[char-blank?]) is printed using the escape sequences described
in @secref["mz:parse-string"], using @litchar{\a}, @litchar{\b},
@litchar{\t}, @litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r},
or @litchar{\e} if possible, otherwise using @litchar{\u} with four
hexadecimal digits or @litchar{\U} with eight hexadecimal digits
(using the latter only if the character value does not fit into four
digits).

All byte strings @scheme[display] as their literal byte sequence; this
byte sequence may not be a valid UTF-8 encoding, so it may not
correspond to a sequence of characters.

The @scheme[write] form a byte string starts with @litchar{#"} and
ends with another @litchar{"}. Between the @litchar{"}s, each byte is
written using the corresponding ASCII decoding if the byte is between
0 and 127 and the character is graphic or blank (according to
@scheme[char-graphic?] and @scheme[char-blank?]). Otherwise, the byte
is written using @litchar{\a}, @litchar{\b}, @litchar{\t},
@litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r}, or
@litchar{\e} if possible, otherwise using @litchar{\} followed by one
to three octal digits (only as many as necessary).

@section[#:tag "mz:print-vectors"]{Printing Vectors}

In @scheme[display] mode, the printed form of a vector is @litchar{#}
followed by the printed form of @scheme[vector->list] applied to the
vector. In @scheme[write] mode, the printed form is the same, except
that a decimal integer is printed after the @litchar{#} when the
@scheme[print-vector-length] parameter is @scheme[#t].

@section[#:tag "mz:print-hashtable"]{Printing Hash Tables}

When the @scheme[print-hash-table] parameter is set to @scheme[#t], a
hash table prints starting with @litchar{#hash(} or @litchar{#hasheq(}
for a table using @scheme[equal?] or @scheme[eq?] key comparisons,
respectively. After this prefix, each key--value mapping is shown as
@litchar{(}, the printed form of a key, a space, @litchar{.}, a space,
the printed form the corresponding value, and @litchar{)}, with an
addition space if the key--value pairs is not the last to be printed.
After all key-value pairs, the printed form completes with
@litchar{)}.

When the @scheme[print-hash-table] parameter is set to @scheme[#f], a
hash table prints (un@scheme[read]ably) as @litchar{#<hash-table>}.

@section[#:tag "mz:print-box"]{Printing Boxes}

When the @scheme[print-box] parameter is set to @scheme[#t], 
a box prints as @litchar{#&} followed by the printed form of its content.

When the @scheme[print-box] parameter is set to @scheme[#f], a box
prints (un@scheme[read]ably) as @litchar{#<box>}.

@section{Printing Characters}

Characters with the special names described in
@secref["mz:parse-character"] @scheme[write] using the same name.
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

Keywords @scheme[write] and @scheme[display] the same as symbols,
except (see @secref["mz:print-symbol"]) with a leading @litchar{#:},
and without special handing for an initial @litchar{#} or when the
printed form would matches a number or a delimited @litchar{.} (since
@litchar{#:} distinguishes the keyword).

@section{Printing Regular Expressions}

Regexp values in both @scheme[write] and @scheme[display] mode print
starting with @litchar{#px} (for @scheme[pregexp]-based regexps) or
@litchar{#rx} (for @scheme[regexp]-based regexps) followed by the
@scheme[write] form of the regexp's source string or byte string.

