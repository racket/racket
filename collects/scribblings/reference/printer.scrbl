#lang scribble/doc
@(require scribble/bnf
          "mz.ss")

@title[#:tag "printing" #:style 'quiet]{The Printer}

The Racket printer supports three modes:

@itemlist[

 @item{@racket[write] mode prints core datatypes in such a way that
       using @racket[read] on the output produces a value that is
       @racket[equal?] to the printed value;}

 @item{@racket[display] mode prints core datatypes is a form in a more
       ``end-user'' style rather than ``programmer' style; for
       example, a string @racket[display]s as its content characters
       without surrounding @litchar{"}s or escapes;}

 @item{@racket[print] mode by default---when
       @scheme[print-as-expression] is @scheme[#t]---prints most
       datatypes in such a way that evaluating the output as an
       expression produces a value that is @racket[equal?] to the
       printed value; when @racket[print-as-expression] is set to
       @racket[#f], then @racket[print] mode is like @racket[write]
       mode.}

]

In @racket[print] mode when @racket[print-as-expression] is
@scheme[#t] (as is the default), a value prints at a @deftech{quoting
depth} of either @scheme[0] (unquoted) or @scheme[1] (quoted). The
initial quoting depth is accepted as an optional argument by
@racket[print], and printing of some compound datatypes adjusts the
print depth for component values. For example, when a list is printed
at quoting depth @scheme[0] and all of its elements are
@deftech{quotable}, the list is printed with a @litchar{'} prefix, and
the list's elements are printed at quoting depth @scheme[1].

When the @scheme[print-graph] parameter is set to @scheme[#t], then
the printer first scans an object to detect cycles. The scan traverses
the components of pairs, mutable pairs, vectors, boxes (when
@scheme[print-box] is @scheme[#t]), hash tables (when
@scheme[print-hash-table] is @scheme[#t]), fields of structures
exposed by @scheme[struct->vector] (when @scheme[print-struct] is
@scheme[#t]), and fields of structures exposed by printing when the
structure's type has the @scheme[prop:custom-write] property. If
@scheme[print-graph] is @scheme[#t], then this information is used to
print sharing through graph definitions and references (see
@secref["parse-graph"]). If a cycle is detected in the initial scan,
then @scheme[print-graph] is effectively set to @scheme[#t]
automatically.

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
@scheme[print-as-expression] is set to @scheme[#t] and the current
@tech{quoting depth} is @scheme[0]. In that case, the symbol's
@scheme[print]ed form is prefixed with @litchar{'}. For the purposes
of printing enclosing datatypes, a symbol is @tech{quotable}.

@section{Printing Numbers}

A number prints the same way in @scheme[write], @scheme[display], and
@scheme[print] modes. For the purposes of printing enclosing
datatypes, a symbol is @tech{quotable}.

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

The constant @scheme[#t] prints as @litchar{#true} or @litchar{#t} in
all modes (@scheme[display], @scheme[write], and @scheme[print]),
depending on the value of @racket[print-boolean-long-form], and the
constant @scheme[#f] prints as @litchar{#false} or @litchar{#f}. For
the purposes of printing enclosing datatypes, a symbol is
@tech{quotable}.

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
pair printing in @scheme[write] mode is adjusted in the case of a pair
that starts a two-element list whose first element is @scheme['quote],
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
form is also the same is @scheme[print-as-expression] is @scheme[#f]
or when the quoting depth is @scheme[1].

For @scheme[print] mode when @scheme[print-as-expression] is
@scheme[#t] and the @tech{quoting depth} is @scheme[0], then the empty
list prints as @litchar{'()}. For a pair whose @scheme[car] and
@scheme[cdr] are @tech{quotable}, the pair prints in @scheme[write]
mode but with a @litchar{'} prefix; the pair's content is printed with
@tech{quoting depth} @scheme[1]. Otherwise, when the @scheme[car] or
@scheme[cdr] is not @tech{quotable}, then pair prints with either
@litchar{cons} (when the @scheme[cdr] is not a pair), @litchar{list}
(when the pair is a list), or @litchar{list*} (otherwise) after the
openining @litchar{(}, any @litchar{.} that would otherwise be printed
is suppressed, and the pair content is printed at @tech{quoting depth}
@scheme[0]. In all cases, when @scheme[print-as-expression] is
@scheme[#t] for @scheme[print] mode, then the value of
@scheme[print-reader-abbreviations] is ignored and reader
abbreviations are always used for lists printed at @tech{quoting
depth} @scheme[1].

By default, mutable pairs (as created with @scheme[mcons]) print the
same as pairs for @scheme[write] and @scheme[display], except that
@litchar["{"] and @litchar["}"] are used instead of @litchar{(} and
@litchar{)}. Note that the reader treats @litchar["{"]...@litchar["}"]
and @litchar{(}...@litchar{)} equivalently on input, creating
immutable pairs in both cases. Mutable in @scheme[print] mode with
@scheme[print-as-expression] as @scheme[#f] or a @tech{quoting depth}
of @scheme[1] also use @litchar["{"] and @litchar["}"]. In
@scheme[print] mode with @scheme[print-as-expression] as @scheme[#t]
and a @tech{quoting depth} of @scheme[0], a mutable pair prints as
@litchar{(mcons }, the @scheme[mcar] and @scheme[mcdr] printed at
@tech{quoting depth} @scheme[0] and separated by a space, and a
closing @litchar{)}.

If the @scheme[print-pair-curly-braces] parameter is set to
@scheme[#t], then pairs print using @litchar["{"] and @litchar["}"] in
when not using @scheme[print] mode with @scheme[print-as-expression] a
@scheme[#t] and a @tech{quoting depth} of @scheme[0].  If the
@scheme[print-mpair-curly-braces] parameter is set to @scheme[#f],
then mutable pairs print using @litchar{(} and @litchar{)} in that
mode.

For the purposes of printing enclosing datatypes, an empty list is
always @tech{quotable}, a pair is @tech{quotable} when its
@scheme[car] and @scheme[cdr] are @tech{quotable}, and a mutable list
is never @tech{quotable}.

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

For the purposes of printing enclosing datatypes, a string or a byte
string is @tech{quotable}.


@section[#:tag "print-vectors"]{Printing Vectors}

In @scheme[display] mode, the printed form of a vector is @litchar{#}
followed by the printed form of @scheme[vector->list] applied to the
vector. In @scheme[write] mode, the printed form is the same, except
that when the @scheme[print-vector-length] parameter is @scheme[#t], a
decimal integer is printed after the @litchar{#}, and a repeated last
element is printed only once.

Vectors @scheme[print] the same as they @scheme[write], unless
@scheme[print-as-expression] is set to @scheme[#t] and the current
@tech{quoting depth} is @scheme[0]. In that case, if all of the
vector's elements are @tech{quotable}, then the vector's
@scheme[print]ed form is prefixed with @litchar{'} and its elements
printed with @tech{quoting depth} @scheme[1]. If its elements are not
all @tech{quotable}, then the vector @racket[print]s as
@litchar["(vector "], the elements at @tech{quoting depth} @scheme[0],
and a closing @litchar{)}. A vector is @tech{quotable} when all of
its elements are @tech{quotable}.


@section[#:tag "print-structure"]{Printing Structures}

When the @scheme[print-struct] parameter is set to @scheme[#t], then
the way that structures print depends on details of the structure type
for which the structure is an instance:

@itemize[

 @item{If the structure type is a @tech{prefab} structure type,
       then it prints in @scheme[write] or @scheme[display] mode using
       @litchar{#s(} followed by the @tech{prefab} structure type key,
       then the printed form each field in the structure, and then
       @litchar{)}.

       In @scheme[print] mode when @scheme[print-as-expression] is set
       to @scheme[#t] and the current @tech{quoting depth} is
       @scheme[0], if the structure's content is all @tech{quotable},
       then the structure's @scheme[print]ed form is prefixed with
       @litchar{'} and its content is printed with @tech{quoting
       depth} @scheme[1]. If any of its content is not quotable, then
       the structure type prints the same as a non-@tech{prefab}
       structure type.

       An instance of a @tech{prefab} structure type is @tech{quotable}
       when all of its content is @tech{quotable}.}

 @item{If the structure has a @scheme[prop:custom-write] property
       value, then the associated procedure is used to print the
       structure, unless the @racket[print-unreadable] parameter is
       set to @racket[#f].

       For @scheme[print] mode, an instance of a structure type with a
       @scheme[prop:custom-write] property is treated as
       @tech{quotable} if it has the
       @scheme[prop:custom-print-quotable] property with a value of
       @scheme['always]. If it has @scheme['maybe] as the property
       value, then the structure is treated as @tech{quotable} if its
       content is @tech{quotable}, where the content is determined by
       the values recursively printed by the structure's
       @scheme[prop:custom-write] procedure. Finally, if the structure
       has @scheme['self] as the property value, then it is treated as
       @tech{quotable}.

       In @scheme[print] mode when @scheme[print-as-expression] is
       @scheme[#t], the structure's @scheme[prop:custom-write]
       procedure is called with either @scheme[0] or @scheme[1] as the
       @tech{quoting depth}, normally depending on the structure's
       @scheme[prop:custom-print-quotable] property value. If the
       property value is @scheme['always], the @tech{quoting depth} is
       normally @scheme[1]. If the property value is @scheme['maybe],
       then the @tech{quoting depth} is @scheme[1] if the structure is
       @tech{quotable}, or normally @scheme[0] otherwise. If the
       property value is @scheme['self], then the quoting depth may be
       @scheme[0] or @scheme[1]; it is normally @scheme[0] if the
       structure is not printed as a part of an enclosing
       @tech{quotable} value, even though the structure is treated as
       @tech{quotable}. Finally, if the property value is
       @scheme['never], then the @tech{quoting depth} is normally
       @scheme[0]. The @tech{quoting depth} can vary from its normal
       value if the structure is printed with an explicit quoting
       depth of @scheme[1].}

 @item{If the structure's type is transparent or if any ancestor is
       transparent (i.e,. @scheme[struct?] on the instance produces
       @scheme[#t]), then the structure prints as the vector produced
       by @scheme[struct->vector] in @scheme[display] mode, in
       @scheme[write] mode, or in @scheme[print] mode when
       @scheme[print-as-expression] is set to @scheme[#f] or when the
       @tech{quoting depth} is @scheme[0].

       In @scheme[print] mode with @scheme[print-as-expression] as
       @scheme[#t] and a @tech{quoting depth} of @scheme[0], the
       structure content is printed with a @litchar{(} followed by the
       list is the structure's type name (as determined by
       @scheme[object-name]) in @scheme[write] mode; the remaining
       elements are @scheme[print]ed at @tech{quoting depth}
       @scheme[0] and separated by a space, and finally a closing
       @litchar{)}.

       A transparent structure type that is not a @tech{prefab}
       structure type is never @tech{quotable}.}

 @item{For any other structure type, the structure prints as an
       unreadable value; see @secref["print-unreadable"] for more
       information.}
]

If the @scheme[print-struct] parameter is set to @scheme[#f], then all
structures without a @scheme[prop:custom-write] property print as
unreadable values (see @secref["print-unreadable"]) and count as
@tech{quotable}.


@section[#:tag "print-hashtable"]{Printing Hash Tables}

When the @scheme[print-hash-table] parameter is set to @scheme[#t], in
@scheme[write] and @scheme[display] modes, a hash table prints
starting with @litchar{#hash(}, @litchar{#hasheqv(}, or
@litchar{#hasheq(} for a table using @scheme[equal?], @scheme[eqv?],
or @scheme[eq?] key comparisons, respectively. After this prefix, each
key--value mapping is shown as @litchar{(}, the printed form of a key,
a space, @litchar{.}, a space, the printed form the corresponding
value, and @litchar{)}, with an additional space if the key--value
pair is not the last to be printed.  After all key-value pairs, the
printed form completes with @litchar{)}.

In @scheme[print] mode when @scheme[print-as-expression] is
@scheme[#f] or the @tech{quoting depth} is @scheme[1], the printed for
is the same as for @scheme[write]. Otherwise, if the hash table's keys
and values are all @tech{quotable}, the table prints with a
@litchar{'} prefix, and the table's key and values are @racket[print]ed
at @tech{quoting depth} @scheme[1]. If some key or value is not
@tech{quotable}, the hash table prints as @litchar["(hash "],
@litchar["(hasheqv "], or @litchar["(hasheq "] followed by alternating
keys and values @racket[print]ed at @tech{quoting depth} @scheme[1] and
separated by spaces, and finally a closing @litchar{)}. A hash table
is @tech{quotable} when all of its keys and values are
@tech{quotable}.

When the @scheme[print-hash-table] parameter is set to @scheme[#f], a
hash table prints as @litchar{#<hash>} and counts as @tech{quotable}.


@section[#:tag "print-box"]{Printing Boxes}

When the @scheme[print-box] parameter is set to @scheme[#t], a box
prints as @litchar{#&} followed by the printed form of its content in
@scheme[write] mode, @scheme[display], or @scheme[print] mode when
@scheme[print-as-expression] is @scheme[#f] or the @tech{quoting
depth} is @scheme[1].

In @scheme[print] mode when @scheme[print-as-expression] is
@scheme[#t] and the @tech{quoting depth} is @scheme[0], a box prints
with a @litchar{'} prefix and its value is printed at @tech{quoting
depth} @scheme[1] when its content is @tech{quotable}, otherwise the
box prints a @litchar["(box "] followed by the content at
@tech{quoting depth} @scheme[0] and a closing @litchar{)}. A box is
@tech{quotable} when its content is @tech{quotable}.

When the @scheme[print-box] parameter is set to @scheme[#f], a box
prints as @litchar{#<box>} and counts as @tech{quotable}.


@section{Printing Characters}

Characters with the special names described in
@secref["parse-character"] @scheme[write] and @scheme[print] using the
same name.  (Some characters have multiple names; the
@scheme[#\newline] and @scheme[#\nul] names are used instead of
@scheme[#\linefeed] and @scheme[#\null]).  Other graphic characters
(according to @scheme[char-graphic?]) @scheme[write] as @litchar{#\}
followed by the single character, and all others characters are
written in @scheme[#\u] notation with four digits or @scheme[#\U]
notation with eight digits (using the latter only if the character
value it does not fit in four digits).

All characters @scheme[display] directly as themselves (i.e., a single
character).

For the purposes of printing enclosing datatypes, a character is
@tech{quotable}.


@section{Printing Keywords}

Keywords @scheme[write], @scheme[print], and @scheme[display] the same
as symbols, except (see @secref["print-symbol"]) with a leading
@litchar{#:} (after any @litchar{'} prefix added in @scheme[print]
mode), and without special handing for an initial @litchar{#} or when
the printed form would matches a number or a delimited @litchar{.}
(since @litchar{#:} distinguishes the keyword).

For the purposes of printing enclosing datatypes, a keyword is
@tech{quotable}.


@section{Printing Regular Expressions}

Regexp values @scheme[write], @scheme[display], and @scheme[print]
starting with @litchar{#px} (for @scheme[pregexp]-based regexps) or
@litchar{#rx} (for @scheme[regexp]-based regexps) followed by the
@scheme[write] form of the regexp's source string or byte string.

For the purposes of printing enclosing datatypes, a regexp value is
@tech{quotable}.


@section[#:tag "print-path"]{Printing Paths}

Paths @scheme[write] and @scheme[print] as @litchar{#<path:....>}. A
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
mislead into thinking that either choice is always appropriate.


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
assembly code for Racket, and reading such an form produces a compiled
form when the @racket[read-accept-compiled] parameter is set to
@racket[#t].

When a compiled form contains syntax object constants, the
@litchar{#~}-marshaled form drops source-location information and
properties (@secref["stxprops"]) for the @tech{syntax objects}.

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

Due to the restrictions on @tech{uninterned} symbols in @litchar{#~},
do not use @racket[gensym] or @racket[string->uninterned-symbol] to
construct an identifier for a top-level or module binding. Instead,
generate distinct identifiers either with
@racket[generate-temporaries] or by applying the result of
@racket[make-syntax-introducer] to an existing identifier; those
functions will lead to top-level and module bindings with
@tech{unreadable symbol}ic names.

Finally, a compiled form may contain path literals. Although paths are
not normally printed in a way that can be read back in, path literals
can be written and read as part of compiled code. The
@racket[current-write-relative-directory] parameter is used to convert
the path to a relative path as is it written, and then
@racket[current-load-relative-directory] parameter is used to convert
any relative path back as it is read. The relative-path conversion
applies on reading whether the path was originally relative or not.
