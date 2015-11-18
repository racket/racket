#lang scribble/doc
@(require scribble/bnf "mz.rkt")

@title[#:tag "printing" #:style 'quiet]{The Printer}

The Racket printer supports three modes:

@itemlist[

 @item{@racket[write] mode prints core datatypes in such a way that
       using @racket[read] on the output produces a value that is
       @racket[equal?] to the printed value;}

 @item{@racket[display] mode prints core datatypes in a more
       ``end-user'' style rather than ``programmer'' style; for
       example, a string @racket[display]s as its content characters
       without surrounding @litchar{"}s or escapes;}

 @item{@racket[print] mode by default---when
       @racket[print-as-expression] is @racket[#t]---prints most
       datatypes in such a way that evaluating the output as an
       expression produces a value that is @racket[equal?] to the
       printed value; when @racket[print-as-expression] is set to
       @racket[#f], then @racket[print] mode is like @racket[write]
       mode.}

]

In @racket[print] mode when @racket[print-as-expression] is
@racket[#t] (as is the default), a value prints at a @deftech{quoting
depth} of either @racket[0] (unquoted) or @racket[1] (quoted). The
initial quoting depth is accepted as an optional argument by
@racket[print], and printing of some compound datatypes adjusts the
print depth for component values. For example, when a list is printed
at quoting depth @racket[0] and all of its elements are
@deftech{quotable}, the list is printed with a @litchar{'} prefix, and
the list's elements are printed at quoting depth @racket[1].

When the @racket[print-graph] parameter is set to @racket[#t], then
the printer first scans an object to detect cycles. The scan traverses
the components of pairs, mutable pairs, vectors, boxes (when
@racket[print-box] is @racket[#t]), hash tables (when
@racket[print-hash-table] is @racket[#t]), fields of structures
exposed by @racket[struct->vector] (when @racket[print-struct] is
@racket[#t]), and fields of structures exposed by printing when the
structure's type has the @racket[prop:custom-write] property. If
@racket[print-graph] is @racket[#t], then this information is used to
print sharing through graph definitions and references (see
@secref["parse-graph"]). If a cycle is detected in the initial scan,
then @racket[print-graph] is effectively set to @racket[#t]
automatically.

With the exception of displaying @tech{byte strings}, printing is defined in
terms of Unicode characters; see @secref["ports"] for information
on how a character stream is written to a port's underlying byte
stream.


@section[#:tag "print-symbol"]{Printing Symbols}

@tech{Symbols} containing spaces or special characters @racket[write] using
escaping @litchar{\} and quoting @litchar{|}s. When the
@racket[read-case-sensitive] parameter is set to @racket[#f], then
symbols containing uppercase characters also use escaping
@litchar{\} and quoting @litchar{|}s. In addition, symbols are
quoted with @litchar{|}s or leading @litchar{\} when they would
otherwise print the same as a numerical constant or as a delimited
@litchar{.} (when @racket[read-accept-dot] is @racket[#t]).

When @racket[read-accept-bar-quote] is @racket[#t], @litchar{|}s are
used in printing when one @litchar{|} at the beginning and one
@litchar{|} at the end suffice to correctly print the
symbol. Otherwise, @litchar{\}s are always used to escape special
characters, instead of quoting them with @litchar{|}s.

When @racket[read-accept-bar-quote] is @racket[#f], then @litchar{|}
is not treated as a special character. The following are always
special characters:

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["}"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{\}
}

In addition, @litchar{#} is a special character when it appears at the
beginning of the symbol, and when it is not followed by @litchar{%}.

Symbols @racket[display] without escaping or quoting special
characters. That is, the display form of a symbol is the same as the
display form of @racket[symbol->string] applied to the symbol.

Symbols @racket[print] the same as they @racket[write], unless
@racket[print-as-expression] is set to @racket[#t] (as is the default) and the current
@tech{quoting depth} is @racket[0]. In that case, the symbol's
@racket[print]ed form is prefixed with @litchar{'}. For the purposes
of printing enclosing datatypes, a symbol is @tech{quotable}.

@section[#:tag "print-number"]{Printing Numbers}

A @tech{number} prints the same way in @racket[write], @racket[display], and
@racket[print] modes. For the purposes of printing enclosing
datatypes, a number is @tech{quotable}.

A @tech{complex number} that is not a @tech{real number} always prints
as @nonterm{m}@litchar{+}@nonterm{n}@litchar{i}, where @nonterm{m} and
@nonterm{n} are the printed forms of its real and imaginary parts,
respectively.

An inexact real number prints with either a @litchar{.} decimal
point, an @litchar{e} exponent marker, or both.  The form is selected
so that the output is as short as possible, with the constraint that
reading the printed form back in produces an @racket[equal?] number.

An exact @racket[0] prints as @litchar{0}.

A positive, exact integer prints as a sequence of decimal digits that
does not start with @racket[0].

A positive, exact, real, non-integer number prints as
@nonterm{m}@litchar{/}@nonterm{n}, where @nonterm{m} and @nonterm{n}
are the printed forms of the number's numerator and denominator (as
determined by @racket[numerator] and @racket[denominator]).

A negative @tech{exact number} prints with a @litchar{-} prefix on the
printed form of the number's exact negation.

@section[#:tag "print-extflonum"]{Printing Extflonums}

An @tech{extflonum} prints the same way in @racket[write],
@racket[display], and @racket[print] modes. For the purposes of
printing enclosing datatypes, an extflonum is @tech{quotable}.

An extflonum prints in the same way an inexact number, but
always with a @litchar{t} or @litchar{T} exponent marker. When
extflonum operations are supported, printing always uses
@litchar{t}; when extflonum operations are not supported, an
extflonum prints the same as its reader (see @secref["reader"])
source, since reading is the only way to produce an extflonum.

@section[#:tag "print-booleans"]{Printing Booleans}

The @tech{boolean} constant @racket[#t] prints as @litchar{#true} or @litchar{#t} in
all modes (@racket[display], @racket[write], and @racket[print]),
depending on the value of @racket[print-boolean-long-form], and the
constant @racket[#f] prints as @litchar{#false} or @litchar{#f}. For
the purposes of printing enclosing datatypes, a symbol is
@tech{quotable}.

@section[#:tag "print-pairs"]{Printing Pairs and Lists}

In @racket[write] and @racket[display] modes, an empty @tech{list} prints as
@litchar{()}. A @tech{pair} normally prints starting with @litchar{(}
followed by the printed form of its @racket[car]. The rest of the
printed form depends on the @racket[cdr]:

@itemize[

 @item{If the @racket[cdr] is a pair or the empty list, then the
       printed form of the pair completes with the printed form of the
       @racket[cdr], except that the leading @litchar{(} in the
       @racket[cdr]'s printed form is omitted.}

 @item{Otherwise, the printed for of the pair continues with a space,
       @litchar{.}, another space, the printed form of the
       @racket[cdr], and a @litchar{)}.}

]

If @racket[print-reader-abbreviations] is set to @racket[#t], then
pair printing in @racket[write] mode is adjusted in the case of a pair
that starts a two-element list whose first element is @racket['quote],
@racket['quasiquote], @racket['unquote], @racket['unquote-splicing],
@racket['syntax], @racket['quasisyntax], @racket['unsyntax], or
@racket['unsyntax-splicing]. In that case, the pair is printed with
the corresponding reader syntax: @litchar{'}, @litchar{`},
@litchar{,}, @litchar[",@"], @litchar{#'}, @litchar{#`}, @litchar{#,},
or @litchar["#,@"], respectively. After the reader syntax, the second
element of the list is printed. When the list is a tail of an
enclosing list, the tail is printed after a @litchar{.} in the
enclosing list (after which the reader abbreviations work), instead of
including the tail as two elements of the enclosing list.

The printed form of a pair is the same in both @racket[write] and
@racket[display] modes, except as the printed form of the pair's
@racket[car] and @racket[cdr] vary with the mode. The @racket[print]
form is also the same if @racket[print-as-expression] is @racket[#f]
or the quoting depth is @racket[1].

For @racket[print] mode when @racket[print-as-expression] is
@racket[#t] and the @tech{quoting depth} is @racket[0], then the empty
list prints as @litchar{'()}. For a pair whose @racket[car] and
@racket[cdr] are @tech{quotable}, the pair prints in @racket[write]
mode but with a @litchar{'} prefix; the pair's content is printed with
@tech{quoting depth} @racket[1]. Otherwise, when the @racket[car] or
@racket[cdr] is not @tech{quotable}, then pair prints with either
@litchar{cons} (when the @racket[cdr] is not a pair), @litchar{list}
(when the pair is a list), or @litchar{list*} (otherwise) after the
opening @litchar{(}, any @litchar{.} that would otherwise be printed
is suppressed, and the pair content is printed at @tech{quoting depth}
@racket[0]. In all cases, when @racket[print-as-expression] is
@racket[#t] for @racket[print] mode, then the value of
@racket[print-reader-abbreviations] is ignored and reader
abbreviations are always used for lists printed at @tech{quoting
depth} @racket[1].

By default, mutable pairs (as created with @racket[mcons]) print the
same as pairs for @racket[write] and @racket[display], except that
@litchar["{"] and @litchar["}"] are used instead of @litchar{(} and
@litchar{)}. Note that the reader treats @litchar["{"]...@litchar["}"]
and @litchar{(}...@litchar{)} equivalently on input, creating
immutable pairs in both cases. Mutable pairs in @racket[print] mode with
@racket[print-as-expression] as @racket[#f] or a @tech{quoting depth}
of @racket[1] also use @litchar["{"] and @litchar["}"]. In
@racket[print] mode with @racket[print-as-expression] as @racket[#t]
and a @tech{quoting depth} of @racket[0], a mutable pair prints as
@litchar{(mcons }, the @racket[mcar] and @racket[mcdr] printed at
@tech{quoting depth} @racket[0] and separated by a space, and a
closing @litchar{)}.

If the @racket[print-pair-curly-braces] parameter is set to
@racket[#t], then pairs print using @litchar["{"] and @litchar["}"]
when not using @racket[print] mode with @racket[print-as-expression] as
@racket[#t] and a @tech{quoting depth} of @racket[0].  If the
@racket[print-mpair-curly-braces] parameter is set to @racket[#f],
then mutable pairs print using @litchar{(} and @litchar{)} in that
mode.

For the purposes of printing enclosing datatypes, an empty list is
always @tech{quotable}, a pair is @tech{quotable} when its
@racket[car] and @racket[cdr] are @tech{quotable}, and a mutable list
is never @tech{quotable}.

@section[#:tag "print-string"]{Printing Strings}

All @tech{strings} @racket[display] as their literal character sequences.

The @racket[write] or @racket[print] form of a string starts with @litchar{"} and ends
with another @litchar{"}. Between the @litchar{"}s, each character is
represented. Each graphic or blank character is represented as itself,
with two exceptions: @litchar{"} is printed as @litchar{\"}, and
@litchar{\} is printed as @litchar{\\}. Each non-graphic, non-blank
character (according to @racket[char-graphic?] and
@racket[char-blank?]) is printed using the escape sequences described
in @secref["parse-string"], using @litchar{\a}, @litchar{\b},
@litchar{\t}, @litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r},
or @litchar{\e} if possible, otherwise using @litchar{\u} with four
hexadecimal digits or @litchar{\U} with eight hexadecimal digits
(using the latter only if the character value does not fit into four
digits).

All byte strings @racket[display] as their literal byte sequence; this
byte sequence may not be a valid UTF-8 encoding, so it may not
correspond to a sequence of characters.

The @racket[write] or @racket[print] form of a byte string starts with @litchar{#"} and
ends with a @litchar{"}. Between the @litchar{"}s, each byte is
written using the corresponding ASCII decoding if the byte is between
0 and 127 and the character is graphic or blank (according to
@racket[char-graphic?] and @racket[char-blank?]). Otherwise, the byte
is written using @litchar{\a}, @litchar{\b}, @litchar{\t},
@litchar{\n}, @litchar{\v}, @litchar{\f}, @litchar{\r}, or
@litchar{\e} if possible, otherwise using @litchar{\} followed by one
to three octal digits (only as many as necessary).

For the purposes of printing enclosing datatypes, a string or a byte
string is @tech{quotable}.


@section[#:tag "print-vectors"]{Printing Vectors}

In @racket[display] mode, the printed form of a @tech{vector} is @litchar{#}
followed by the printed form of @racket[vector->list] applied to the
vector. In @racket[write] mode, the printed form is the same, except
that when the @racket[print-vector-length] parameter is @racket[#t], a
decimal integer is printed after the @litchar{#}, and a repeated last
element is printed only once.

Vectors @racket[print] the same as they @racket[write], unless
@racket[print-as-expression] is set to @racket[#t] and the current
@tech{quoting depth} is @racket[0]. In that case, if all of the
vector's elements are @tech{quotable}, then the vector's
@racket[print]ed form is prefixed with @litchar{'} and its elements
printed with @tech{quoting depth} @racket[1]. If its elements are not
all @tech{quotable}, then the vector @racket[print]s as
@litchar["(vector "], the elements at @tech{quoting depth} @racket[0],
and a closing @litchar{)}. A vector is @tech{quotable} when all of
its elements are @tech{quotable}.

In @racket[write] or @racket[display] mode, a @tech{flvector} prints
like a @tech{vector}, but with a @litchar{#fl} prefix instead of
@litchar{#}. A @tech{fxvector} similarly prints with a @litchar{#fx}
prefix instead of @litchar{#}. The @racket[print-vector-length]
parameter affects @tech{flvector} and @tech{fxvector} printing the
same as @tech{vector} printing. In @racket[print] mode,
@tech{flvectors} and @tech{fxvectors} are not @tech{quotable}, and
they print like a @tech{vector} at @tech{quoting depth} 0 using a
@litchar["(flvector "] or @litchar["(fxvector "] prefix, respectively.


@section[#:tag "print-structure"]{Printing Structures}

When the @racket[print-struct] parameter is set to @racket[#t], then
the way that @tech{structures} print depends on details of the structure type
for which the structure is an instance:

@itemize[

 @item{If the structure type is a @tech{prefab} structure type,
       then it prints in @racket[write] or @racket[display] mode using
       @litchar{#s(} followed by the @tech{prefab} structure type key,
       then the printed form of each field in the structure, and then
       @litchar{)}.

       In @racket[print] mode when @racket[print-as-expression] is set
       to @racket[#t] and the current @tech{quoting depth} is
       @racket[0], if the structure's content is all @tech{quotable},
       then the structure's @racket[print]ed form is prefixed with
       @litchar{'} and its content is printed with @tech{quoting
       depth} @racket[1]. If any of its content is not quotable, then
       the structure type prints the same as a non-@tech{prefab}
       structure type.

       An instance of a @tech{prefab} structure type is @tech{quotable}
       when all of its content is @tech{quotable}.}

 @item{If the structure has a @racket[prop:custom-write] property
       value, then the associated procedure is used to print the
       structure, unless the @racket[print-unreadable] parameter is
       set to @racket[#f].

       For @racket[print] mode, an instance of a structure type with a
       @racket[prop:custom-write] property is treated as
       @tech{quotable} if it has the
       @racket[prop:custom-print-quotable] property with a value of
       @racket['always]. If it has @racket['maybe] as the property
       value, then the structure is treated as @tech{quotable} if its
       content is @tech{quotable}, where the content is determined by
       the values recursively printed by the structure's
       @racket[prop:custom-write] procedure. Finally, if the structure
       has @racket['self] as the property value, then it is treated as
       @tech{quotable}.

       In @racket[print] mode when @racket[print-as-expression] is
       @racket[#t], the structure's @racket[prop:custom-write]
       procedure is called with either @racket[0] or @racket[1] as the
       @tech{quoting depth}, normally depending on the structure's
       @racket[prop:custom-print-quotable] property value. If the
       property value is @racket['always], the @tech{quoting depth} is
       normally @racket[1]. If the property value is @racket['maybe],
       then the @tech{quoting depth} is @racket[1] if the structure is
       @tech{quotable}, or normally @racket[0] otherwise. If the
       property value is @racket['self], then the quoting depth may be
       @racket[0] or @racket[1]; it is normally @racket[0] if the
       structure is not printed as a part of an enclosing
       @tech{quotable} value, even though the structure is treated as
       @tech{quotable}. Finally, if the property value is
       @racket['never], then the @tech{quoting depth} is normally
       @racket[0]. The @tech{quoting depth} can vary from its normal
       value if the structure is printed with an explicit quoting
       depth of @racket[1].}

 @item{If the structure's type is transparent or if any ancestor is
       transparent (i.e., @racket[struct?] on the instance produces
       @racket[#t]), then the structure prints as the vector produced
       by @racket[struct->vector] in @racket[display] mode, in
       @racket[write] mode, or in @racket[print] mode when
       @racket[print-as-expression] is set to @racket[#f] or when the
       @tech{quoting depth} is @racket[0].

       In @racket[print] mode with @racket[print-as-expression] as
       @racket[#t] and a @tech{quoting depth} of @racket[0], the
       structure content is printed with a @litchar{(} followed by
       the structure's type name (as determined by
       @racket[object-name]) in @racket[write] mode; the remaining
       elements are @racket[print]ed at @tech{quoting depth}
       @racket[0] and separated by a space, and finally a closing
       @litchar{)}.

       A transparent structure type that is not a @tech{prefab}
       structure type is never @tech{quotable}.}

 @item{For any other structure type, the structure prints as an
       unreadable value; see @secref["print-unreadable"] for more
       information.}
]

If the @racket[print-struct] parameter is set to @racket[#f], then all
structures without a @racket[prop:custom-write] property print as
unreadable values (see @secref["print-unreadable"]) and count as
@tech{quotable}.


@section[#:tag "print-hashtable"]{Printing Hash Tables}

When the @racket[print-hash-table] parameter is set to @racket[#t], in
@racket[write] and @racket[display] modes, a @tech{hash table} prints
starting with @litchar{#hash(}, @litchar{#hasheqv(}, or
@litchar{#hasheq(} for a table using @racket[equal?], @racket[eqv?],
or @racket[eq?] key comparisons, respectively. After the prefix, each
key--value mapping is shown as @litchar{(}, the printed form of a key,
a space, @litchar{.}, a space, the printed form the corresponding
value, and @litchar{)}, with an additional space if the key--value
pair is not the last to be printed.  After all key--value pairs, the
printed form completes with @litchar{)}.

In @racket[print] mode when @racket[print-as-expression] is
@racket[#f] or the @tech{quoting depth} is @racket[1], the printed form
is the same as for @racket[write]. Otherwise, if the hash table's keys
and values are all @tech{quotable}, the table prints with a
@litchar{'} prefix, and the table's key and values are @racket[print]ed
at @tech{quoting depth} @racket[1]. If some key or value is not
@tech{quotable}, the hash table prints as @litchar["(hash "],
@litchar["(hasheqv "], or @litchar["(hasheq "] followed by alternating
keys and values @racket[print]ed at @tech{quoting depth} @racket[1] and
separated by spaces, and finally a closing @litchar{)}. A hash table
is @tech{quotable} when all of its keys and values are
@tech{quotable}.

When the @racket[print-hash-table] parameter is set to @racket[#f], a
hash table prints as @litchar{#<hash>} and counts as @tech{quotable}.


@section[#:tag "print-box"]{Printing Boxes}

When the @racket[print-box] parameter is set to @racket[#t], a @tech{box}
prints as @litchar{#&} followed by the printed form of its content in
@racket[write], @racket[display], or @racket[print] mode when
@racket[print-as-expression] is @racket[#f] or the @tech{quoting
depth} is @racket[1].

In @racket[print] mode when @racket[print-as-expression] is
@racket[#t] and the @tech{quoting depth} is @racket[0], a box prints
with a @litchar{'} prefix and its value is printed at @tech{quoting
depth} @racket[1] when its content is @tech{quotable}, otherwise the
box prints a @litchar["(box "] followed by the content at
@tech{quoting depth} @racket[0] and a closing @litchar{)}. A box is
@tech{quotable} when its content is @tech{quotable}.

When the @racket[print-box] parameter is set to @racket[#f], a box
prints as @litchar{#<box>} and counts as @tech{quotable}.


@section[#:tag "print-character"]{Printing Characters}

@tech{Characters} with the special names described in
@secref["parse-character"] @racket[write] and @racket[print] using the
same name.  (Some characters have multiple names; the
@racket[#\newline] and @racket[#\nul] names are used instead of
@racketvalfont{#\linefeed} and @racketvalfont{#\null}.)  Other graphic characters
(according to @racket[char-graphic?]) @racket[write] as @litchar{#\}
followed by the single character, and all others characters are
written in @racket[#\u] notation with four digits or @racket[#\U]
notation with eight digits (using the latter only if the character
value does not fit in four digits).

All characters @racket[display] directly as themselves (i.e., a single
character).

For the purposes of printing enclosing datatypes, a character is
@tech{quotable}.


@section[#:tag "print-keyword"]{Printing Keywords}

@tech{Keywords} @racket[write], @racket[print], and @racket[display] the same
as symbols (see @secref["print-symbol"]) except with a leading
@litchar{#:} (after any @litchar{'} prefix added in @racket[print]
mode), and without special handling for an initial @litchar{#} or when
the printed form would match a number or a delimited @litchar{.}
(since @litchar{#:} distinguishes the keyword).

For the purposes of printing enclosing datatypes, a keyword is
@tech{quotable}.


@section[#:tag "print-regexp"]{Printing Regular Expressions}

@tech{Regexp values} @racket[write], @racket[display], and @racket[print]
starting with @litchar{#px} (for @racket[pregexp]-based regexps) or
@litchar{#rx} (for @racket[regexp]-based regexps) followed by the
@racket[write] form of the regexp's source string or byte string.

For the purposes of printing enclosing datatypes, a regexp value is
@tech{quotable}.


@section[#:tag "print-path"]{Printing Paths}

@tech{Paths} @racket[write] and @racket[print] as @litchar{#<path:....>}. A
path @racket[display]s the same as the string produced by
@racket[path->string]. For the purposes of printing enclosing
datatypes, a path counts as @tech{quotable}.

Although a path can be converted to a string with
@racket[path->string] or to a byte string with @racket[path->bytes],
neither is clearly the right choice for printing a path and reading it
back. If the path value is meant to be moved among platforms, then a
string is probably the right choice, despite the potential for losing
information when converting a path to a string. For a path that is
intended to be re-read on the same platform, a byte string is probably
the right choice, since it preserves information in an unportable
way. Paths do not print in a readable way so that programmers are not
misled into thinking that either choice is always appropriate.


@section[#:tag "print-unreadable"]{Printing Unreadable Values}

For any value with no other printing specification, assuming that the
@racket[print-unreadable] parameter is set to @racket[#t], the output
form is @litchar{#<}@nonterm{something}@litchar{>}, where
@nonterm{something} is specific to the type of the value and sometimes
to the value itself. If @racket[print-unreadable] is set to
@racket[#f], then attempting to print an unreadable value raises
@racket[exn:fail].

For the purposes of printing enclosing datatypes, a value that prints
unreadably nevertheless counts as @tech{quotable}.


@section[#:tag "print-compiled"]{Printing Compiled Code}

Compiled code as produced by @racket[compile] prints using
@litchar{#~}. Compiled code printed with @litchar{#~} is essentially
assembly code for Racket, and reading such a form produces a compiled
form when the @racket[read-accept-compiled] parameter is set to
@racket[#t].

When a compiled form contains syntax object constants, they must not
be @tech{tainted} or @tech{armed}; the @litchar{#~}-marshaled form
drops source-location information and properties (see
@secref["stxprops"]) for the @tech{syntax objects}.

Compiled code parsed from @litchar{#~} may contain references to
unexported or protected bindings from a module. At read time, such
references are associated with the current code inspector (see
@racket[current-code-inspector]), and the code will only execute if
that inspector controls the relevant module invocation (see
@secref["modprotect"]).

A compiled-form object may contain @tech{uninterned} symbols (see
@secref["symbols"]) that were created by @racket[gensym] or
@racket[string->uninterned-symbol]. When the compiled object is read
via @litchar{#~}, each uninterned symbol in the original form is
mapped to a new uninterned symbol, where multiple instances of a
single symbol are consistently mapped to the same new symbol. The
original and new symbols have the same printed
representation. @tech{Unreadable symbols}, which are typically
generated indirectly during expansion and compilation, are saved and
restored consistently through @litchar{#~}.

The dynamic nature of @tech{uninterned} symbols and their localization
within @litchar{#~} can cause problems when @racket[gensym] or
@racket[string->uninterned-symbol] is used to construct an identifier
for a top-level or module binding (depending on how the identifier and
its references are compiled). To avoid problems, generate distinct
identifiers either with @racket[generate-temporaries] or by applying
the result of @racket[make-syntax-introducer] to an existing
identifier; those functions lead to top-level and module variables
with @tech{unreadable symbol}ic names, and the names are deterministic
as long as expansion is otherwise deterministic. 

Despite the problems inherent with @tech{uninterned} symbols as
variable names, they are partially supported even across multiple
@litchar{#~}s: When compiled code contains a reference to a module-defined
variable whose name is an @tech{uninterned} symbol, the relative
position of the variable among the module's definitions is recorded,
and the reference can be linked back to the definition based on its
position and the characters in its name. This accommodation works only
for variable references in compiled code; it does not work for
@racket[syntax]-quoted identifiers, for example.

Finally, a compiled form may contain path literals. Although paths are
not normally printed in a way that can be read back in, path literals
can be written and read as part of compiled code. The
@racket[current-write-relative-directory] parameter is used to convert
the path to a relative path as is it written, and then
@racket[current-load-relative-directory] parameter is used to convert
any relative path back as it is read. The relative-path conversion
applies on reading whether the path was originally relative or not.
