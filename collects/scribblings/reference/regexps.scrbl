#lang scribble/doc
@(require scribble/bnf
          "mz.ss"
          "rx.ss")

@title[#:tag "regexp"]{Regular Expressions}

@section-index{regexps}
@section-index{pattern matching}
@section-index["strings" "pattern matching"]
@section-index["input ports" "pattern matching"]

@guideintro["regexp"]{regular expressions}

@deftech{Regular expressions} are specified as strings or byte
strings, using the same pattern language as the Unix utility
@exec{egrep} or Perl. A string-specified pattern produces a character
regexp matcher, and a byte-string pattern produces a byte regexp
matcher. If a character regexp is used with a byte string or input
port, it matches UTF-8 encodings (see @secref["encodings"]) of
matching character streams; if a byte regexp is used with a character
string, it matches bytes in the UTF-8 encoding of the string.

Regular expressions can be compiled into a @deftech{regexp value} for
repeated matches. The @scheme[regexp] and @scheme[byte-regexp]
procedures convert a string or byte string (respectively) into a
regexp value using one syntax of regular expressions that is most
compatible to @exec{egrep}. The @scheme[pregexp] and
@scheme[byte-pregexp] procedures produce a regexp value using a
slightly different syntax of regular expressions that is more
compatible with Perl.  In addition, Scheme constants written with
@litchar{#rx} or @litchar{#px} (see @secref["reader"]) produce
compiled regexp values.

The internal size of a regexp value is limited to 32 kilobytes; this
limit roughly corresponds to a source string with 32,000 literal
characters or 5,000 operators.

@;------------------------------------------------------------------------
@section[#:tag "regexp-syntax"]{Regexp Syntax}

The following syntax specifications describe the content of a string
that represents a regular expression. The syntax of the corresponding
string may involve extra escape characters. For example, the regular
expression @litchar{(.*)\1} can be represented with the string
@scheme["(.*)\\1"] or the regexp constant @scheme[#rx"(.*)\\1"]; the
@litchar{\} in the regular expression must be escaped to include it
in a string or regexp constant.

The @scheme[regexp] and @scheme[pregexp] syntaxes share a common core:

@common-table

The following completes the grammar for @scheme[regexp], which treats
@litchar["{"] and @litchar["}"] as literals, @litchar{\} as a
literal within ranges, and @litchar{\} as a literal producer
outside of ranges.

@rx-table

The following completes the grammar for @scheme[pregexp], which uses
@litchar["{"] and @litchar["}"] bounded repetition and uses
@litchar{\} for meta-characters both inside and outside of ranges.

@px-table

@;------------------------------------------------------------------------
@section{Additional Syntactic Constraints}

In addition to matching a grammars, regular expressions must meet two
syntactic restrictions:

@itemize[

 @item{In a @nonterm{repeat} other than @nonterm{atom}@litchar{?},
       then @nonterm{atom} must not match an empty sequence.}

 @item{In a @litchar{(?<=}@nonterm{regexp}@litchar{)} or
       @litchar{(?<!}@nonterm{regexp}@litchar{)},
       the @nonterm{regexp} must match a bounded sequence, only.}

]

These contraints are checked syntactically by the following type
system. A type [@math{n}, @math{m}] corresponds to an expression that
matches between @math{n} and @math{m} characters. In the rule for
@litchar{(}@nonterm{Regexp}@litchar{)}, @math{N} means the number such
that the opening parenthesis is the @math{N}th opening parenthesis for
collecting match reports.  Non-emptiness is inferred for a
backreference pattern, @litchar{\}@nonterm{N}, so that a
backreference can be used for repetition patterns; in the case of
mutual dependencies among backreferences, the inference chooses the
fixpoint that maximizes non-emptiness.  Finiteness is not inferred for
backreferences (i.e., a backreference is assumed to match an
arbitrarily large sequence).

@type-table

@;------------------------------------------------------------------------
@section{Regexp Constructors}

@defproc[(regexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{regexp value} created by
@scheme[regexp] or @scheme[pregexp], @scheme[#f] otherwise.}


@defproc[(pregexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{regexp value} created by
@scheme[pregexp] (not @scheme[regexp]), @scheme[#f] otherwise.}


@defproc[(byte-regexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{regexp value} created by
@scheme[byte-regexp] or @scheme[byte-pregexp], @scheme[#f] otherwise.}


@defproc[(byte-pregexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{regexp value} created by
@scheme[byte-pregexp] (not @scheme[byte-regexp]), @scheme[#f]
otherwise.}


@defproc[(regexp [str string?]) regexp?]{

Takes a string representation of a regular expression (using the
syntax in @secref["regexp-syntax"]) and compiles it into a @tech{regexp
value}. Other regular expression procedures accept either a string or a
@tech{regexp value} as the matching pattern. If a regular expression string
is used multiple times, it is faster to compile the string once to a
@tech{regexp value} and use it for repeated matches instead of using the
string each time.

The @scheme[object-name] procedure returns
the source string for a @tech{regexp value}.

@examples[
(regexp "ap*le")
(object-name #rx"ap*le")
]}

@defproc[(pregexp [string string?]) pregexp?]{

Like @scheme[regexp], except that it uses a slightly different syntax
(see @secref["regexp-syntax"]). The result can be used with
@scheme[regexp-match], etc., just like the result from
@scheme[regexp].

@examples[
(pregexp "ap*le")
(regexp? #px"ap*le")
]}

@defproc[(byte-regexp [bstr bytes?]) byte-regexp?]{

Takes a byte-string representation of a regular expression (using the
syntax in @secref["regexp-syntax"]) and compiles it into a
byte-@tech{regexp value}.

The @scheme[object-name] procedure
returns the source byte string for a @tech{regexp value}.

@examples[
(byte-regexp #"ap*le")
(object-name #rx#"ap*le")
(byte-regexp "ap*le")
]}

@defproc[(byte-pregexp [bstr bytes?]) byte-pregexp?]{

Like @scheme[byte-regexp], except that it uses a slightly different
syntax (see @secref["regexp-syntax"]). The result can be used with
@scheme[regexp-match], etc., just like the result from
@scheme[byte-regexp].

@examples[
(byte-pregexp #"ap*le")
]}

@defproc*[([(regexp-quote [str string?] [case-sensitive? any/c #t]) string?]
           [(regexp-quote [bstr bytes?] [case-sensitive? any/c #t]) bytes?])]{

Produces a string or byte string suitable for use with @scheme[regexp]
to match the literal sequence of characters in @scheme[str] or
sequence of bytes in @scheme[bstr]. If @scheme[case-sensitive?] is
true, the resulting regexp matches letters in @scheme[str] or
@scheme[bytes] case-insensitively, otherwise it matches
case-sensitively.

@examples[
(regexp-match "." "apple.scm")
(regexp-match (regexp-quote ".") "apple.scm")
]}


@;------------------------------------------------------------------------
@section{Regexp Matching}

@defproc[(regexp-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? #f) #f]
                       [output-port (or/c output-port? #f) #f])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (or/c #f (cons/c string? (listof (or/c string? #f))))
             (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))]{

Attempts to match @scheme[pattern] (a string, byte string, @tech{regexp
value}, or byte-@tech{regexp value}) once to a portion of @scheme[input].  The
matcher finds a portion of @scheme[input] that matches and is closest
to the start of the input (after @scheme[start-pos]).

The optional @scheme[start-pos] and @scheme[end-pos] arguments select
a portion of @scheme[input] for matching; the default is the entire
string or the stream up to an end-of-file. When @scheme[input] is a
string, @scheme[start-pos] is a character position; when
@scheme[input] is a byte string, then @scheme[start-pos] is a byte
position; and when @scheme[input] is an input port, @scheme[start-pos]
is the number of bytes to skip before starting to match. The
@scheme[end-pos] argument can be @scheme[#f], which corresponds to the
end of the string or the end-of-file in the stream; otherwise, it is a
character or byte position, like @scheme[start-pos]. If @scheme[input]
is an input port, and if the end-of-file is reached before
@scheme[start-pos] bytes are skipped, then the match fails.

In @scheme[pattern], a start-of-string @litchar{^} refers to the first
position of @scheme[input] after @scheme[start-pos], and the
end-of-input @litchar{$} refers to the @scheme[end-pos]th position or
(in the case of an input port) the end of file, whichever comes first.

If the match fails, @scheme[#f] is returned. If the match succeeds, a
list containing strings or byte string, and possibly @scheme[#f], is
returned. The list contains strings only if @scheme[input] is a string
and @scheme[pattern] is not a byte regexp. Otherwise, the list
contains byte strings (substrings of the UTF-8 encoding of
@scheme[input], if @scheme[input] is a string).

The first [byte] string in a result list is the portion of
@scheme[input] that matched @scheme[pattern]. If two portions of
@scheme[input] can match @scheme[pattern], then the match that starts
earliest is found.

Additional [byte] strings are returned in the list if @scheme[pattern]
contains parenthesized sub-expressions (but not when the open
parenthesis is followed by @litchar{?:}). Matches for the
sub-expressions are provided in the order of the opening parentheses
in @scheme[pattern]. When sub-expressions occur in branches of an
@litchar{|} ``or'' pattern, in a @litchar{*} ``zero or more''
pattern, or other places where the overall pattern can succeed without
a match for the sub-expression, then a @scheme[#f] is returned for the
sub-expression if it did not contribute to the final match. When a
single sub-expression occurs within a @litchar{*} ``zero or more''
pattern or other multiple-match positions, then the rightmost match
associated with the sub-expression is returned in the list.

If the optional @scheme[output-port] is provided as an output port,
the part of @scheme[input] from its beginning (not @scheme[start-pos])
that precedes the match is written to the port. All of @scheme[input]
up to @scheme[end-pos] is written to the port if no match is
found. This functionality is most useful when @scheme[input] is an
input port.

When matching an input port, a match failure reads up to
@scheme[end-pos] bytes (or end-of-file), even if @scheme[pattern]
begins with a start-of-string @litchar{^}; see also
@scheme[regexp-try-match]. On success, all bytes up to and including
the match are eventually read from the port, but matching proceeds by
first peeking bytes from the port (using @scheme[peek-bytes-avail!]),
and then (re-)reading matching bytes to discard them after the match
result is determined. Non-matching bytes may be read and discarded
before the match is determined. The matcher peeks in blocking mode
only as far as necessary to determine a match, but it may peek extra
bytes to fill an internal buffer if immediately available (i.e.,
without blocking). Greedy repeat operators in @scheme[pattern], such
as @litchar{*} or @litchar{+}, tend to force reading the entire
content of the port (up to @scheme[end-pos]) to determine a match.

If the input port is read simultaneously by another thread, or if the
port is a custom port with inconsistent reading and peeking procedures
(see @secref["customport"]), then the bytes that are peeked and
used for matching may be different than the bytes read and discarded
after the match completes; the matcher inspects only the peeked
bytes. To avoid such interleaving, use @scheme[regexp-match-peek]
(with a @scheme[progress-evt] argument) followed by
@scheme[port-commit-peeked].

@examples[
(regexp-match #rx"x." "12x4x6")
(regexp-match #rx"y." "12x4x6")
(regexp-match #rx"x." "12x4x6" 3)
(regexp-match #rx"x." "12x4x6" 3 4)
(regexp-match #rx#"x." "12x4x6")
(regexp-match #rx"x." "12x4x6" 0 #f (current-output-port))
(regexp-match #rx"(-[0-9]*)+" "a-12--345b")
]}


@defproc[(regexp-match* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? input-port?)]
                        [start-pos exact-nonnegative-integer? 0]
                        [end-pos (or/c exact-nonnegative-integer? #f) #f])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (listof string?)
             (listof bytes?))]{

Like @scheme[regexp-match], but the result is a list of strings or
byte strings corresponding to a sequence of matches of
@scheme[pattern] in @scheme[input]. (Unlike @scheme[regexp-match],
results for parenthesized sub-patterns in @scheme[pattern] are not
returned.)

The @scheme[pattern] is used in order to find matches, where each
match attempt starts at the end of the last match.  Empty matches are
handled like any matches, returning a zero-length string or byte
sequence (they are more useful in the complementing
@scheme[regexp-split] function).  However, the @scheme[pattern] is
restricted from matching an empty string at the beginning (or right
after a previous match) or at the end.

If @scheme[input] contains no matches (in the range @scheme[start-pos]
to @scheme[end-pos]), @scheme[null] is returned. Otherwise, each item
in the resulting list is a distinct substring or byte sequence from
@scheme[input] that matches @scheme[pattern]. The @scheme[end-pos]
argument can be @scheme[#f] to match to the end of @scheme[input]
(which corresponds to an end-of-file if @scheme[input] is an input
port).

@examples[
(regexp-match* #rx"x." "12x4x6")
]}


@defproc[(regexp-try-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                           [input input-port?]
                           [start-pos exact-nonnegative-integer? 0]
                           [end-pos (or/c exact-nonnegative-integer? #f) #f]
                           [output-port (or/c output-port? #f) #f])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (or/c #f (cons/c string? (listof (or/c string? #f))))
             (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))]{

Like @scheme[regexp-match] on input ports, except that if the match
fails, no characters are read and discarded from @scheme[in].

This procedure is especially useful with a @scheme[pattern] that
begins with a start-of-string @litchar{^} or with a non-@scheme[#f]
@scheme[end-pos], since each limits the amount of peeking into the
port. Otherwise, beware that a large portion of the stream may be
peeked (and therefore pulled into memory) before the match succeeds or
fails.}


@defproc[(regexp-match-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                                 [input (or/c string? bytes? input-port?)]
                                 [start-pos exact-nonnegative-integer? 0]
                                 [end-pos (or/c exact-nonnegative-integer? #f) #f]
                                 [output-port (or/c output-port? #f) #f])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @scheme[regexp-match], but returns a list of number pairs (and
@scheme[#f]) instead of a list of strings. Each pair of numbers refers
to a range of characters or bytes in @scheme[input]. If the result for
the same arguments with @scheme[regexp-match] would be a list of byte
strings, the resulting ranges correspond to byte ranges; in that case,
if @scheme[input] is a character string, the byte ranges correspond to
bytes in the UTF-8 encoding of the string.

Range results are returned in a @scheme[substring]- and
@scheme[subbytes]-compatible manner, independent of
@scheme[start-pos]. In the case of an input port, the returned
positions indicate the number of bytes that were read, including
@scheme[start-pos], before the first matching byte.

@examples[
(regexp-match-positions #rx"x." "12x4x6")
(regexp-match-positions #rx"x." "12x4x6" 3)
(regexp-match-positions #rx"(-[0-9]*)+" "a-12--345b")
]}


@defproc[(regexp-match-positions* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                                  [input (or/c string? bytes? input-port?)]
                                  [start-pos exact-nonnegative-integer? 0]
                                  [end-pos (or/c exact-nonnegative-integer? #f) #f])
         (listof (cons/c exact-nonnegative-integer?
                         exact-nonnegative-integer?))]{

Like @scheme[regexp-match-positions], but returns multiple matches
like @scheme[regexp-match*].

@examples[
(regexp-match-positions #rx"x." "12x4x6")
]}


@defproc[(regexp-match? [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? input-port?)]
                        [start-pos exact-nonnegative-integer? 0]
                        [end-pos (or/c exact-nonnegative-integer? #f) #f]
                        [output-port (or/c output-port? #f) #f])
           boolean?]{

Like @scheme[regexp-match], but returns merely @scheme[#t] when the
match succeeds, @scheme[#f] otherwise.

@examples[
(regexp-match? #rx"x." "12x4x6")
(regexp-match? #rx"y." "12x4x6")
]}


@defproc[(regexp-match-exact? [pattern (or/c string? bytes? regexp? byte-regexp?)]
                              [input (or/c string? bytes? input-port?)])
          boolean?]{

Like @scheme[regexp-match?], but @scheme[#t] is only returned when the
entire content of @scheme[input] matches @scheme[pattern].

@examples[
(regexp-match-exact? #rx"x." "12x4x6")
(regexp-match-exact? #rx"1.*x." "12x4x6")
]}


@defproc[(regexp-match-peek [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f])
          (or/c (cons/c bytes? (listof (or/c bytes? #f)))
                #f)]{

Like @scheme[regexp-match] on input ports, but only peeks bytes from
@scheme[input-port] instead of reading them. Furthermore, instead of
an output port, the last optional argument is a progress event for
@scheme[input-port] (see @scheme[port-progress-evt]). If @scheme[progress]
becomes ready, then the match stops peeking from @scheme[input-port]
and returns @scheme[#f]. The @scheme[progress] argument can be
@scheme[#f], in which case the peek may continue with inconsistent
information if another process meanwhile reads from
@scheme[input-port].

@examples[
(define p (open-input-string "a abcd"))
(regexp-match-peek ".*bc" p)
(regexp-match-peek ".*bc" p 2)
(regexp-match ".*bc" p 2)
(peek-char p)
(regexp-match ".*bc" p)
(peek-char p)
]}


@defproc[(regexp-match-peek-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @scheme[regexp-match-positions] on input ports, but only peeks
bytes from @scheme[input-port] instead of reading them, and with a
@scheme[progress] argument like @scheme[regexp-match-peek].}


@defproc[(regexp-match-peek-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f])
          (or/c (cons/c bytes? (listof (or/c bytes? #f)))
                #f)]{

Like @scheme[regexp-match-peek], but it attempts to match only bytes
that are available from @scheme[input-port] without blocking.  The
match fails if not-yet-available characters might be used to match
@scheme[pattern].}


@defproc[(regexp-match-peek-positions-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @scheme[regexp-match-peek-positions], but it attempts to match
only bytes that are available from @scheme[input-port] without
blocking. The match fails if not-yet-available characters might be
used to match @scheme[pattern].}


@defproc[(regexp-match-peek-positions* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f])
         (listof (cons/c exact-nonnegative-integer?
                         exact-nonnegative-integer?))]{

Like @scheme[regexp-match-peek-positions], but returns multiple matches like
@scheme[regexp-match*].}

@;------------------------------------------------------------------------
@section{Regexp Splitting}

@defproc[(regexp-split [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? #f) #f])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (cons/c string? (listof string?))
             (cons/c bytes? (listof bytes?)))]{

The complement of @scheme[regexp-match*]: the result is a list of
strings (if @scheme[pattern] is a string or character regexp and
@scheme[input] is a string) or byte strings (otherwise) from in
@scheme[input] that are separated by matches to
@scheme[pattern]. Adjacent matches are separated with @scheme[""] or
@scheme[#""]. Zero-length matches are treated the same as in
@scheme[regexp-match*], but are more useful in this case.

If @scheme[input] contains no matches (in the range @scheme[start-pos]
to @scheme[end-pos]), the result is a list containing @scheme[input]'s
content (from @scheme[start-pos] to @scheme[end-pos]) as a single
element. If a match occurs at the beginning of @scheme[input] (at
@scheme[start-pos]), the resulting list will start with an empty
string or byte string, and if a match occurs at the end (at
@scheme[end-pos]), the list will end with an empty string or byte
string. The @scheme[end-pos] argument can be @scheme[#f], in which
case splitting goes to the end of @scheme[input] (which corresponds to
an end-of-file if @scheme[input] is an input port).

@examples[
(regexp-split #rx" +" "12  34")
(regexp-split #rx"." "12  34")
(regexp-split #rx"" "12  34")
(regexp-split #rx" *" "12  34")
(regexp-split #px"\\b" "12, 13 and 14.")
]}

@;------------------------------------------------------------------------
@section{Regexp Substitution}

@defproc[(regexp-replace [pattern (or/c string? bytes? regexp? byte-regexp?)]
                         [input (or/c string? bytes?)]
                         [insert (or/c string? bytes? 
                                       ((string?) () #:rest (listof string?) . ->* . string?)
                                       ((bytes?) () #:rest (listof bytes?) . ->* . bytes?))])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             string?
             bytes?)]{

Performs a match using @scheme[pattern] on @scheme[input], and then
returns a string or byte string in which the matching portion of
@scheme[input] is replaced with @scheme[insert].  If @scheme[pattern]
matches no part of @scheme[input], then @scheme[iput] is returned
unmodified.

The @scheme[insert] argument can be either a (byte) string, or a
function that returns a (byte) string. In the latter case, the
function is applied on the list of values that @scheme[regexp-match]
would return (i.e., the first argument is the complete match, and then
one argument for each parenthesized sub-expression) to obtain a
replacement (byte) string.

If @scheme[pattern] is a string or character regexp and @scheme[input]
is a string, then @scheme[insert] must be a string or a procedure that
accept strings, and the result is a string. If @scheme[pattern] is a
byte string or byte regexp, or if @scheme[input] is a byte string,
then @scheme[insert] as a string is converted to a byte string,
@scheme[insert] as a procedure is called with a byte string, and the
result is a byte string.

If @scheme[insert] contains @litchar{&}, then @litchar{&}
is replaced with the matching portion of @scheme[input] before it is
substituted into the match's place.  If @scheme[insert] contains
@litchar{\}@nonterm{n} for some integer @nonterm{n}, then it is
replaced with the @nonterm{n}th matching sub-expression from
@scheme[input]. A @litchar{&} and @litchar{\0} are synonymous. If
the @nonterm{n}th sub-expression was not used in the match, or if
@nonterm{n} is greater than the number of sub-expressions in
@scheme[pattern], then @litchar{\}@nonterm{n} is replaced with the
empty string.

To substitute a literal @litchar{&} or @litchar{\}, use
@litchar{\&} and @litchar{\\}, respectively, in
@scheme[insert]. A @litchar{\$} in @scheme[insert] is
equivalent to an empty sequence; this can be used to terminate a
number @nonterm{n} following @litchar{\}. If a @litchar{\} in
@scheme[insert] is followed by anything other than a digit,
@litchar{&}, @litchar{\}, or @litchar{$}, then the @litchar{\}
by itself is treated as @litchar{\0}.

Note that the @litchar{\} described in the previous paragraphs is a
character or byte of @scheme[input]. To write such an @scheme[input]
as a Scheme string literal, an escaping @litchar{\} is needed
before the @litchar{\}. For example, the Scheme constant
@scheme["\\1"] is @litchar{\1}.

@examples[
(regexp-replace "mi" "mi casa" "su")
(regexp-replace "mi" "mi casa" string-upcase)
(regexp-replace "([Mm])i ([a-zA-Z]*)" "Mi Casa" "\\1y \\2")
(regexp-replace "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                "\\1y \\2")
(regexp-replace #rx"x" "12x4x6" "\\\\")
(display (regexp-replace #rx"x" "12x4x6" "\\\\"))
]}

@defproc[(regexp-replace* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                          [input (or/c string? bytes?)]
                          [insert (or/c string? bytes? 
                                        (string? . -> . string?)
                                        (bytes? . -> . bytes?))])
         (or/c string? bytes?)]{

Like @scheme[regexp-replace], except that every instance of
@scheme[pattern] in @scheme[input] is replaced with @scheme[insert],
instead of just the first match. Only non-overlapping instances of
@scheme[pattern] in @scheme[input] are replaced, so instances of
@scheme[pattern] within inserted strings are @italic{not} replaced
recursively. Zero-length matches are treated the same as in
@scheme[regexp-match*].

@examples[
(regexp-replace* "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi" 
                 "\\1y \\2")
(regexp-replace* "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi" 
                 (lambda (all one two)
                   (string-append (string-downcase one) "y"
                                  (string-upcase two))))
(display (regexp-replace* #rx"x" "12x4x6" "\\\\"))
]}

@defproc*[([(regexp-replace-quote [str string?]) string?]
           [(regexp-replace-quote [bstr bytes?]) bytes?])]{

Produces a string suitable for use as the third argument to
@scheme[regexp-replace] to insert the literal sequence of characters
in @scheme[str] or bytes in @scheme[bstr] as a replacement.
Concretely, every @litchar{\} and @litchar{&} in @scheme[str] or
@scheme[bstr] is protected by a quoting @litchar{\}.

@examples[
(regexp-replace "UT" "Go UT!" "A&M")
(regexp-replace "UT" "Go UT!" (regexp-replace-quote "A&M"))
]}

