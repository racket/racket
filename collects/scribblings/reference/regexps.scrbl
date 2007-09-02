#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@require["rx.ss"]

@title[#:tag "regexp"]{Regular Expressions}

@declare-exporting[big (lib "big/regexp")]

@section-index{regexps}
@section-index{pattern matching}
@section-index["strings" "pattern matching"]
@section-index["input ports" "pattern matching"]

Regular expressions are specified as strings or byte strings, using
the same pattern language as the Unix utility @exec{egrep} or Perl. A
string-specified pattern produces a character regexp matcher, and a
byte-string pattern produces a byte regexp matcher. If a character
regexp is used with a byte string or input port, it matches UTF-8
encodings (see @secref["encodings"]) of matching character streams;
if a byte regexp is used with a character string, it matches bytes in
the UTF-8 encoding of the string.

Regular expressions can be compiled into a @defterm{regexp value} for
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
expression @litchar["(.*)\\1"] can be represented with the string
@scheme["(.*)\\1"] or the regexp constant @scheme[#rx"(.*)\\1"]; the
@litchar["\\"] in the regular expression must be escaped to include it
in a string or regexp constant.

The @scheme[regexp] and @scheme[pregexp] syntaxes share a common core:

@common-table

The following completes the grammar for @scheme[regexp], which treats
@litchar["{"] and @litchar["}"] as literals, @litchar["\\"] as a
literal within ranges, and @litchar["\\"] as a literal producer
outside of ranges.

@rx-table

The following completes the grammar for @scheme[pregexp], which uses
@litchar["{"] and @litchar["}"] bounded repetition and uses
@litchar["\\"] for meta-characters both inside and outside of ranges.

@px-table

@;------------------------------------------------------------------------
@section{Additional Syntactic Constraints}

In addition to matching a grammars, regular expressions must meet two
syntactic restrictions:

@itemize{

 @item{In a @nonterm{repeat} other than @nonterm{atom}@litchar{?},
       then @nonterm{atom} must not match an empty sequence.}

 @item{In a @litchar{(?<=}@nonterm{regexp}@litchar{)} or
       @litchar{(?<!}@nonterm{regexp}@litchar{)},
       the @nonterm{regexp} must match a bounded sequence, only.}

}

These contraints are checked syntactically by the following type
system. A type [@math{n}, @math{m}] corresponds to an expression that
matches between @math{n} and @math{m} characters. In the rule for
@litchar{(}@nonterm{Regexp}@litchar{)}, @math{N} means the number such
that the opening parenthesis is the @math{N}th opening parenthesis for
collecting match reports.  Non-emptiness is inferred for a
backreference pattern, @litchar["\\"]@nonterm{N}, so that a
backreference can be used for repetition patterns; in the case of
mutual dependencies among backreferences, the inference chooses the
fixpoint that maximizes non-emptiness.  Finiteness is not inferred for
backreferences (i.e., a backreference is assumed to match an
arbitrarily large sequence).

@type-table

@;------------------------------------------------------------------------
@section{Regexp Constructors}

@defproc[(regexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a regexp value created by
@scheme[regexp] or @scheme[pregexp], @scheme[#f] otherwise.}


@defproc[(pregexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a regexp value created by
@scheme[pregexp] (not @scheme[regexp]), @scheme[#f] otherwise.}


@defproc[(byte-regexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a regexp value created by
@scheme[byte-regexp] or @scheme[byte-pregexp], @scheme[#f] otherwise.}


@defproc[(byte-pregexp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a regexp value created by
@scheme[byte-pregexp] (not @scheme[byte-regexp]), @scheme[#f]
otherwise.}


@defproc[(regexp [str string?]) regexp?]{

Takes a string representation of a regular expression (using the
syntax in @secref["regexp-syntax"]) and compiles it into a regexp
value. Other regular expression procedures accept either a string or a
regexp value as the matching pattern. If a regular expression string
is used multiple times, it is faster to compile the string once to a
regexp value and use it for repeated matches instead of using the
string each time.

The @scheme[object-name] procedure returns
the source string for a regexp value.

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
byte-regexp value.

The @scheme[object-name] procedure
returns the source byte string for a regexp value.

@examples[
(byte-regexp #"ap*le")
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


@;------------------------------------------------------------------------
@section{Regexp Matching}

@defproc[(regexp-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos nonnegative-exact-integer? 0]
                       [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                       [output-port (or/c output-port? false/c) #f])
         (or/c (listof (or/c (cons (or/c string? bytes?)
                                   (or/c string? bytes?))
                             false/c))
               false/c)]{

Attempts to match @scheme[pattern] (a string, byte string, regexp
value, or byte-regexp value) once to a portion of @scheme[input].  The
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
and @scheme[pattern] is not a byte regexp value. Otherwise, the list
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
@litchar["|"] ``or'' pattern, in a @litchar{*} ``zero or more''
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
@scheme[regexp-match/fail-without-reading]. On success, all bytes up
to and including the match are eventually read from the port, but
matching proceeds by first peeking bytes from the port (using
@scheme[peek-bytes-avail!]), and then (re-)reading matching bytes to
discard them after the match result is determined. Non-matching bytes
may be read and discarded before the match is determined. The matcher
peeks in blocking mode only as far as necessary to determine a match,
but it may peek extra bytes to fill an internal buffer if immediately
available (i.e., without blocking). Greedy repeat operators in
@scheme[pattern], such as @litchar{*} or @litchar{+}, tend to force
reading the entire content of the port (up to @scheme[end-pos]) to
determine a match.

If the input port is read simultaneously by another thread, or if the
port is a custom port with inconsistent reading and peeking procedures
(see @secref["customport"]), then the bytes that are peeked and
used for matching may be different than the bytes read and discarded
after the match completes; the matcher inspects only the peeked
bytes. To avoid such interleaving, use @scheme[regexp-match-peek]
(with a @scheme[progress-evt] argument) followed by
@scheme[port-commit-peeked].}

@defproc[(regexp-match-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? input-port?)]
                        [start-pos nonnegative-exact-integer? 0]
                        [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                        [output-port (or/c output-port? false/c) #f])
          (or/c (listof (or/c (cons nonnegative-exact-integer?
                                    nonnegative-exact-integer?)
                              false/c))
                false/c)]{

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
@scheme[start-pos], before the first matching byte.}

@defproc[(regexp-match? [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? input-port?)]
                        [start-pos nonnegative-exact-integer? 0]
                        [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                        [output-port (or/c output-port? false/c) #f])
           boolean?]{

Like @scheme[regexp-match], but returns merely @scheme[#t] when the
match succeeds, @scheme[#f] otherwise.}

@defproc[(regexp-match-peek [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos nonnegative-exact-integer? 0]
                            [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                            [progress (or/c evt false/c) #f])
          (or/c (listof (or/c (cons bytes? bytes?)
                              false/c))
                false/c)]{

Like @scheme[regexp-match] on input ports, but only peeks bytes from
@scheme[input-port] instead of reading them. Furthermore, instead of
an output port, the last optional argument is a progress event for
@scheme[input-port] (see @scheme[port-progress-evt]). If @scheme[progress]
becomes ready, then the match stops peeking from @scheme[input-port]
and returns @scheme[#f]. The @scheme[progress] argument can be
@scheme[#f], in which case the peek may continue with inconsistent
information if another process meanwhile reads from
@scheme[input-port].}

@defproc[(regexp-match-peek-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos nonnegative-exact-integer? 0]
                            [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                            [progress (or/c evt false/c) #f])
          (or/c (listof (or/c (cons nonnegative-exact-integer?
                                    nonnegative-exact-integer?)
                              false/c))
                false/c)]{

Like @scheme[regexp-match-positions] on input ports, but only peeks
bytes from @scheme[input-port] instead of reading them, and with a
@scheme[progress] argument like @scheme[regexp-match-peek].}

@defproc[(regexp-match-peek-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos nonnegative-exact-integer? 0]
                            [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                            [progress (or/c evt false/c) #f])
          (or/c (listof (or/c (cons bytes? bytes?)
                              false/c))
                false/c)]{

Like @scheme[regexp-match-peek], but it attempts to match only bytes
that are available from @scheme[input-port] without blocking.  The
match fails if not-yet-available characters might be used to match
@scheme[pattern].}

@defproc[(regexp-match-peek-positions-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos nonnegative-exact-integer? 0]
                            [end-pos (or/c nonnegative-exact-integer? false/c) #f]
                            [progress (or/c evt false/c) #f])
          (or/c (listof (or/c (cons nonnegative-exact-integer?
                                    nonnegative-exact-integer?)
                              false/c))
                false/c)]{

Like @scheme[regexp-match-peek-positions], but it attempts to match
only bytes that are available from @scheme[input-port] without
blocking. The match fails if not-yet-available characters might be
used to match @scheme[pattern].}

@;{

@;------------------------------------------------------------------------
@section{Regexp Substitution}

@defproc[(regexp-replace [char-pattern any/c][string any/c][insert any/c]) any]{

Performs a match using @scheme[pattern] on @scheme[input] and
then returns a string in which the matching portion of @scheme[input]
is replaced with @scheme[insert-string].  If @scheme[char-pattern]
matches no part of @scheme[string], then @scheme[string] is returned
unmodified.

 The @scheme[char-pattern] must be a string or a character regexp value
 (not a byte string or a byte regexp value).

 If @scheme[insert-string] contains ``\&'', then ``\&'' is replaced with
 the matching portion of @scheme[string] before it is substituted into
 @scheme[string].  If @scheme[insert-string] contains
 ``{\Backslash}@scheme[n]'' (for some integer @scheme[n]), then it is
 replaced with the @scheme[n]th matching sub-expression from
 @scheme[string].\footnote{The backslash is a character in the string, so
 an extra backslash is required to specify the string as a Scheme
 constant. For example, the Scheme constant
 @scheme["\\1"] is ``{\Backslash}1''.} ``\&''
 and ``{\Backslash}0'' are synonymous. If the @scheme[n]th sub-expression
 was not used in the match or if @scheme[n] is greater than the number of
 sub-expressions in @scheme[pattern], then ``{\Backslash}@scheme[n]'' is
 replaced with the empty string.

 A literal ``\&'' or ``{\Backslash}'' is specified as
 ``{\Backslash}\&'' or ``{\Backslash}{\Backslash}'', respectively.  If
 @scheme[insert-string] contains ``{\Backslash}\$'', then
 ``{\Backslash}\$'' is replaced with the empty string. (This can be
 used to terminate a number @scheme[n] following a backslash.) If a
 ``{\Backslash}'' is followed by anything other than a digit, ``\&'',
 ``{\Backslash}'', or ``\$'', then it is treated as ``{\Backslash}0''.}

 @item{@defproc[(regexp-replace [byte-pattern any/c][string-or-bytes any/c][insert-string-or-bytes any/c]) any]
%
  is analogous to @scheme[regexp-replace] on strings, where
  @scheme[byte-pattern] is a byte string or a byte regexp value. The result
  is always a byte string.}

 @item{@defproc[(regexp-replace [char-pattern any/c][string any/c][proc any/c]) any]
%
   is like @scheme[regexp-replace], but instead of an
   @scheme[insert-string] third argument, the third argument is a
   procedure that accepts match strings and produces a string to
   replace the match. The @scheme[proc] must accept the same number of
   arguments as @scheme[regexp-match] produces list elements for a
   successful match with @scheme[char-pattern].}

 @item{@defproc[(regexp-replace [byte-pattern any/c][string-or-bytes any/c][proc any/c]) any]
%
   is analogous to @scheme[regexp-replace] on strings and a procedure
   argument, but the procedure accepts byte strings to produce a byte
   string, instead of character strings.}

 @item{@defproc[(regexp-replace* [pattern any/c][string any/c][insert-string any/c]) any]
%
 is the same as @scheme[regexp-replace], except that every instance of
 @scheme[pattern] in @scheme[string] is replaced with
 @scheme[insert-string]. Only non-overlapping instances of @scheme[pattern]
 in the original @scheme[string] are replaced, so instances of
 @scheme[pattern] within inserted strings are \Em{not} replaced
 recursively. If, in the process of repeating matches, @scheme[pattern]
 matches an empty string, the @exnraise[exn:fail].}

 @item{@defproc[(regexp-replace* [byte-pattern any/c][bytes any/c][insert-bytes any/c]) any]
%
 is analogous to @scheme[regexp-replace*] on strings.}

 @item{@defproc[(regexp-replace* [char-pattern any/c][string any/c][proc any/c]) any]
%
   is like @scheme[regexp-replace] with a procedure argument, but with
   multiple instances replaced. The given @scheme[proc] is called once
   for each match.}

 @item{@defproc[(regexp-replace* [byte-pattern any/c][bytes any/c][proc any/c]) any]
%
   is like @scheme[regexp-replace*] with a string and procedure
   argument, but the procedure accepts and produces byte strings.}

}
