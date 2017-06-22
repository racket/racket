#lang scribble/doc
@(require scribble/bnf 
          "mz.rkt" 
          "rx.rkt"
          (for-syntax racket/base))

@title[#:tag "regexp"]{Regular Expressions}

@section-index{regexps}
@section-index{pattern matching}
@section-index["strings" "pattern matching"]
@section-index["input ports" "pattern matching"]

@(define-syntax (rx-examples stx)
  (syntax-case stx ()
   [(_ [num rx input] ...)
    (with-syntax ([(ex ...)
                   (map (lambda (num rx input)
                          `(eval:alts #,(racket 
                                         (code:line 
                                          (regexp-match ,rx ,input) 
                                          (code:comment @#,t["ex"
                                                             (let ([s (number->string ,num)])
                                                               (elemtag `(rxex ,s) 
                                                                        (racketcommentfont s)))
                                                             ,(if (pregexp? (syntax-e rx))
                                                                  `(list ", uses " (racketmetafont "#px"))
                                                                  "")])))
                                      (regexp-match ,rx ,input)))
                        (syntax->list #'(num ...))
                        (syntax->list #'(rx ...))
                        (syntax->list #'(input ...)))])
      #`(examples ex ...))]))

@guideintro["regexp"]{regular expressions}

@deftech{Regular expressions} are specified as strings or byte
strings, using the same pattern language as either the Unix utility
@exec{egrep} or Perl. A string-specified pattern produces a character
regexp matcher, and a byte-string pattern produces a byte regexp
matcher. If a character regexp is used with a byte string or input
port, it matches UTF-8 encodings (see @secref["encodings"]) of
matching character streams; if a byte regexp is used with a character
string, it matches bytes in the UTF-8 encoding of the string.

A regular expression that is represented as a string or byte string
can be compiled to a @deftech{regexp value}, which can be used more
efficiently by functions such as @racket[regexp-match] compared to the
string or byte string form. The @racket[regexp] and
@racket[byte-regexp] procedures convert a string or byte string
(respectively) into a regexp value using a syntax of regular
expressions that is most compatible to @exec{egrep}. The
@racket[pregexp] and @racket[byte-pregexp] procedures produce a regexp
value using a slightly different syntax of regular expressions that is
more compatible with Perl.

Two @tech{regexp values} are @racket[equal?] if they have the same
source, use the same pattern language, and are both character regexps
or both byte regexps.

A literal or printed @tech{regexp value} starts with @litchar{#rx} or
@litchar{#px}. @see-read-print["regexp"]{regular expressions} Regexp
values produced by the default reader are @tech{interned} in 
@racket[read-syntax] mode.

The internal size of a @tech{regexp value} is limited to 32 kilobytes; this
limit roughly corresponds to a source string with 32,000 literal
characters or 5,000 operators.

@;------------------------------------------------------------------------
@section[#:tag "regexp-syntax"]{Regexp Syntax}

The following syntax specifications describe the content of a string
that represents a regular expression. The syntax of the corresponding
string may involve extra escape characters. For example, the regular
expression @litchar{(.*)\1} can be represented with the string
@racket["(.*)\\1"] or the regexp constant @racket[#rx"(.*)\\1"]; the
@litchar{\} in the regular expression must be escaped to include it
in a string or regexp constant.

The @racket[regexp] and @racket[pregexp] syntaxes share a common core:

@common-table

The following completes the grammar for @racket[regexp], which treats
@litchar["{"] and @litchar["}"] as literals, @litchar{\} as a
literal within ranges, and @litchar{\} as a literal producer
outside of ranges.

@rx-table

The following completes the grammar for @racket[pregexp], which uses
@litchar["{"] and @litchar["}"] bounded repetition and uses
@litchar{\} for meta-characters both inside and outside of ranges.

@px-table

In case-insensitive mode, a backreference of the form
@litchar{\}@nonterm{n} matches case-insensitively only with respect to
ASCII characters.

The Unicode categories follow.

@category-table

@rx-examples[
[1 #rx"a|b" "cat"]
[2 #rx"[at]" "cat"]
[3 #rx"ca*[at]" "caaat"]
[4 #rx"ca+[at]" "caaat"]
[5 #rx"ca?t?" "ct"]
[6 #rx"ca*?[at]" "caaat"]
[7 #px"ca{2}" "caaat"]
[8 #px"ca{2,}t" "catcaat"]
[9 #px"ca{,2}t" "caaatcat"]
[10 #px"ca{1,2}t" "caaatcat"]
[11 #rx"(c*)(a*)" "caat"]
[12 #rx"[^ca]" "caat"]
[13 #rx".(.)." "cat"]
[14 #rx"^a|^c" "cat"]
[15 #rx"a$|t$" "cat"]
[16 #px"c(.)\\1t" "caat"]
[17 #px".\\b." "cat in hat"]
[18 #px".\\B." "cat in hat"]
[19 #px"\\p{Ll}" "Cat"]
[20 #px"\\P{Ll}" "cat!"]
[21 #rx"\\|" "c|t"]
[22 #rx"[a-f]*" "cat"]
[23 #px"[a-f\\d]*" "1cat"]
[24 #px" [\\w]" "cat hat"]
[25 #px"t[\\s]" "cat\nhat"]
[26 #px"[[:lower:]]+" "Cat"]
[27 #rx"[]]" "c]t"]
[28 #rx"[-]" "c-t"]
[29 #rx"[]a[]+" "c[a]t"]
[30 #rx"[a^]+" "ca^t"]
[31 #rx".a(?=p)" "cat nap"]
[32 #rx".a(?!t)" "cat nap"]
[33 #rx"(?<=n)a." "cat nap"]
[34 #rx"(?<!c)a." "cat nap"]
[35 #rx"(?i:a)[tp]" "cAT nAp"]
[36 #rx"(?(?<=c)a|b)+" "cabal"]
]

@;------------------------------------------------------------------------
@section{Additional Syntactic Constraints}

In addition to matching a grammar, regular expressions must meet two
syntactic restrictions:

@itemize[

 @item{In a @nonterm{repeat} other than @nonterm{atom}@litchar{?},
       the @nonterm{atom} must not match an empty sequence.}

 @item{In a @litchar{(?<=}@nonterm{regexp}@litchar{)} or
       @litchar{(?<!}@nonterm{regexp}@litchar{)},
       the @nonterm{regexp} must match a bounded sequence only.}

]

These constraints are checked syntactically by the following type
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

Returns @racket[#t] if @racket[v] is a @tech{regexp value} created by
@racket[regexp] or @racket[pregexp], @racket[#f] otherwise.}


@defproc[(pregexp? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{regexp value} created by
@racket[pregexp] (not @racket[regexp]), @racket[#f] otherwise.}


@defproc[(byte-regexp? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{regexp value} created by
@racket[byte-regexp] or @racket[byte-pregexp], @racket[#f] otherwise.}


@defproc[(byte-pregexp? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{regexp value} created by
@racket[byte-pregexp] (not @racket[byte-regexp]), @racket[#f]
otherwise.}


@defproc*[([(regexp [str string?]) regexp?]
           [(regexp [str string?]
                    [handler (or/c #f (string? -> any))])
            any])]{

Takes a string representation of a regular expression (using the
syntax in @secref["regexp-syntax"]) and compiles it into a @tech{regexp
value}. Other regular expression procedures accept either a string or a
@tech{regexp value} as the matching pattern. If a regular expression string
is used multiple times, it is faster to compile the string once to a
@tech{regexp value} and use it for repeated matches instead of using the
string each time.

If @racket[handler] is provided and not @racket[#f], it is called and
its result is returned when @racket[str] is not a valid representation
of a regular expression; the argument to @racket[handler] is a string
that describes the problem with @racket[str]. If @racket[handler] is
@racket[#f] or not provided, then @exnraise[exn:fail:contract].

The @racket[object-name] procedure returns
the source string for a @tech{regexp value}.

@examples[
(regexp "ap*le")
(object-name #rx"ap*le")
(regexp "+" (λ (s) (list s)))
]

@history[#:changed "6.5.0.1" @elem{Added the @racket[handler] argument.}]}

@defproc*[([(pregexp [str string?]) pregexp?]
           [(pregexp [str string?]
                     [handler (or/c #f (string? -> any))])
            any])]{

Like @racket[regexp], except that it uses a slightly different syntax
(see @secref["regexp-syntax"]). The result can be used with
@racket[regexp-match], etc., just like the result from
@racket[regexp].

@examples[
(pregexp "ap*le")
(regexp? #px"ap*le")
(pregexp "+" (λ (s) (vector s)))
]

@history[#:changed "6.5.0.1" @elem{Added the @racket[handler] argument.}]}

@defproc*[([(byte-regexp [bstr bytes?]) byte-regexp?]
           [(byte-regexp [bstr bytes?]
                         [handler (or/c #f (bytes? -> any))])
            any])]{

Takes a byte-string representation of a regular expression (using the
syntax in @secref["regexp-syntax"]) and compiles it into a
byte-@tech{regexp value}.

If @racket[handler] is provided, it is called and its result is returned
if @racket[str] is not a valid representation of a regular expression.

The @racket[object-name] procedure
returns the source byte string for a @tech{regexp value}.

@examples[
(byte-regexp #"ap*le")
(object-name #rx#"ap*le")
(eval:error (byte-regexp "ap*le"))
(byte-regexp #"+" (λ (s) (list s)))
]

@history[#:changed "6.5.0.1" @elem{Added the @racket[handler] argument.}]}

@defproc*[([(byte-pregexp [bstr bytes?]) byte-pregexp?]
           [(byte-pregexp [bstr bytes?]
                          [handler (or/c #f (bytes? -> any))])
            any])]{

Like @racket[byte-regexp], except that it uses a slightly different
syntax (see @secref["regexp-syntax"]). The result can be used with
@racket[regexp-match], etc., just like the result from
@racket[byte-regexp].

@examples[
(byte-pregexp #"ap*le")
(byte-pregexp #"+" (λ (s) (vector s)))
]

@history[#:changed "6.5.0.1" @elem{Added the @racket[handler] argument.}]}

@defproc*[([(regexp-quote [str string?] [case-sensitive? any/c #t]) string?]
           [(regexp-quote [bstr bytes?] [case-sensitive? any/c #t]) bytes?])]{

Produces a string or byte string suitable for use with @racket[regexp]
to match the literal sequence of characters in @racket[str] or
sequence of bytes in @racket[bstr]. If @racket[case-sensitive?] is
true (the default), the resulting regexp matches letters in
@racket[str] or @racket[bytes] case-sensitively, otherwise it matches
case-insensitively.

@examples[
(regexp-match "." "apple.scm")
(regexp-match (regexp-quote ".") "apple.scm")
]}

@defproc[(regexp-max-lookbehind [pattern (or/c regexp? byte-regexp?)])
         exact-nonnegative-integer?]{

Returns the maximum number of bytes that @racket[pattern] may consult
before the starting position of a match to determine the match. For
example, the pattern @litchar{(?<=abc)d} consults three bytes
preceding a matching @litchar{d}, while @litchar{e(?<=a..)d} consults
two bytes before a matching @litchar{ed}. A @litchar{^} pattern may
consult a preceding byte to determine whether the current position is
the start of the input or of a line.}

@;------------------------------------------------------------------------
@section{Regexp Matching}

@defproc[(regexp-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? path? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? #f) #f]
                       [output-port (or/c output-port? #f) #f]
                       [input-prefix bytes? #""])
         (if (and (or (string? pattern) (regexp? pattern))
                  (or (string? input) (path? input)))
             (or/c #f (cons/c string? (listof (or/c string? #f))))
             (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))]{

Attempts to match @racket[pattern] (a string, byte string,
@tech{regexp value}, or byte-@tech{regexp value}) once to a portion of
@racket[input].  The matcher finds a portion of @racket[input] that
matches and is closest to the start of the input (after
@racket[start-pos]).

If @racket[input] is a path, it is converted to a byte string with
@racket[path->bytes] if @racket[pattern] is a byte string or a
byte-based regexp. Otherwise, @racket[input] is converted to a string
with @racket[path->string].

The optional @racket[start-pos] and @racket[end-pos] arguments select
a portion of @racket[input] for matching; the default is the entire
string or the stream up to an end-of-file. When @racket[input] is a
string, @racket[start-pos] is a character position; when
@racket[input] is a byte string, then @racket[start-pos] is a byte
position; and when @racket[input] is an input port, @racket[start-pos]
is the number of bytes to skip before starting to match. The
@racket[end-pos] argument can be @racket[#f], which corresponds to the
end of the string or an end-of-file in the stream; otherwise, it is a
character or byte position, like @racket[start-pos]. If @racket[input]
is an input port, and if an end-of-file is reached before
@racket[start-pos] bytes are skipped, then the match fails.

In @racket[pattern], a start-of-string @litchar{^} refers to the first
position of @racket[input] after @racket[start-pos], assuming that
@racket[input-prefix] is @racket[#""].  The end-of-input @litchar{$}
refers to the @racket[end-pos]th position or (in the case of an input
port) an end-of-file, whichever comes first.

The @racket[input-prefix] specifies bytes that effectively precede
@racket[input] for the purposes of @litchar{^} and other look-behind
matching. For example, a @racket[#""] prefix means that @litchar{^}
matches at the beginning of the stream, while a @racket[#"\n"]
@racket[input-prefix] means that a start-of-line @litchar{^} can match
the beginning of the input, while a start-of-file @litchar{^} cannot.

If the match fails, @racket[#f] is returned. If the match succeeds, a
list containing strings or byte string, and possibly @racket[#f], is
returned. The list contains strings only if @racket[input] is a string
and @racket[pattern] is not a byte regexp. Otherwise, the list
contains byte strings (substrings of the UTF-8 encoding of
@racket[input], if @racket[input] is a string).

The first [byte] string in a result list is the portion of
@racket[input] that matched @racket[pattern]. If two portions of
@racket[input] can match @racket[pattern], then the match that starts
earliest is found.

Additional [byte] strings are returned in the list if @racket[pattern]
contains parenthesized sub-expressions (but not when the opening
parenthesis is followed by @litchar{?}). Matches for the
sub-expressions are provided in the order of the opening parentheses
in @racket[pattern]. When sub-expressions occur in branches of an
@litchar{|} ``or'' pattern, in a @litchar{*} ``zero or more''
pattern, or other places where the overall pattern can succeed without
a match for the sub-expression, then a @racket[#f] is returned for the
sub-expression if it did not contribute to the final match. When a
single sub-expression occurs within a @litchar{*} ``zero or more''
pattern or other multiple-match positions, then the rightmost match
associated with the sub-expression is returned in the list.

If the optional @racket[output-port] is provided as an output port,
the part of @racket[input] from its beginning (not @racket[start-pos])
that precedes the match is written to the port. All of @racket[input]
up to @racket[end-pos] is written to the port if no match is
found. This functionality is most useful when @racket[input] is an
input port.

When matching an input port, a match failure reads up to
@racket[end-pos] bytes (or end-of-file), even if @racket[pattern]
begins with a start-of-string @litchar{^}; see also
@racket[regexp-try-match]. On success, all bytes up to and including
the match are eventually read from the port, but matching proceeds by
first peeking bytes from the port (using @racket[peek-bytes-avail!]),
and then (re@-~-)reading matching bytes to discard them after the match
result is determined. Non-matching bytes may be read and discarded
before the match is determined. The matcher peeks in blocking mode
only as far as necessary to determine a match, but it may peek extra
bytes to fill an internal buffer if immediately available (i.e.,
without blocking). Greedy repeat operators in @racket[pattern], such
as @litchar{*} or @litchar{+}, tend to force reading the entire
content of the port (up to @racket[end-pos]) to determine a match.

If the input port is read simultaneously by another thread, or if the
port is a custom port with inconsistent reading and peeking procedures
(see @secref["customport"]), then the bytes that are peeked and
used for matching may be different than the bytes read and discarded
after the match completes; the matcher inspects only the peeked
bytes. To avoid such interleaving, use @racket[regexp-match-peek]
(with a @racket[progress-evt] argument) followed by
@racket[port-commit-peeked].

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
                        [input (or/c string? bytes? path? input-port?)]
                        [start-pos exact-nonnegative-integer? 0]
                        [end-pos (or/c exact-nonnegative-integer? #f) #f]
                        [input-prefix bytes? #""]
                        [#:match-select match-select
                         (or/c (list? . -> . (or/c any/c list?))
                               #f)
                         car]
                        [#:gap-select? gap-select any/c #f])
         (if (and (or (string? pattern) (regexp? pattern))
                  (or (string? input) (path? input)))
             (listof (or/c string? (listof (or/c #f string?))))
             (listof (or/c bytes? (listof (or/c #f bytes?)))))]{

Like @racket[regexp-match], but the result is a list of strings or
byte strings corresponding to a sequence of matches of
@racket[pattern] in @racket[input].

The @racket[pattern] is used in order to find matches, where each
match attempt starts at the end of the last match, and @litchar{^} is
allowed to match the beginning of the input (if @racket[input-prefix]
is @racket[#""]) only for the first match.  Empty matches are handled
like other matches, returning a zero-length string or byte sequence
(they are more useful in making this a complement of
@racket[regexp-split]), but @racket[pattern] is restricted from
matching an empty sequence immediately after an empty match.

If @racket[input] contains no matches (in the range @racket[start-pos]
to @racket[end-pos]), @racket[null] is returned. Otherwise, each item
in the resulting list is a distinct substring or byte sequence from
@racket[input] that matches @racket[pattern]. The @racket[end-pos]
argument can be @racket[#f] to match to the end of @racket[input]
(which corresponds to an end-of-file if @racket[input] is an input
port).

@examples[
(regexp-match* #rx"x." "12x4x6")
(regexp-match* #rx"x*" "12x4x6")
]

@racket[match-select] specifies the collected results.  The default of
@racket[car] means that the result is the list of matches without
returning parenthesized sub-patterns.  It can be given as a `selector'
function which chooses an item from a list, or it can choose a list of
items.  For example, you can use @racket[cdr] to get a list of lists
of parenthesized sub-patterns matches, or @racket[values] (as an
identity function) to get the full matches as well.  (Note that the
selector must choose an element of its input list or a list of
elements, but it must not inspect its input as they can be either a
list of strings or a list of position pairs.  Furthermore, the
selector must be consistent in its choice(s).)

@examples[
(regexp-match* #rx"x(.)" "12x4x6" #:match-select cadr)
(regexp-match* #rx"x(.)" "12x4x6" #:match-select values)
]

In addition, specifying @racket[gap-select] as a non-@racket[#f] value
will make the result an interleaved list of the matches as well as the
separators between them matches, starting and ending with a separator.
In this case, @racket[match-select] can be given as @racket[#f] to
return @emph{only} the separators, making such uses equivalent to
@racket[regexp-split].

@examples[
(regexp-match* #rx"x(.)" "12x4x6" #:match-select cadr #:gap-select? #t)
(regexp-match* #rx"x(.)" "12x4x6" #:match-select #f #:gap-select? #t)
]}


@defproc[(regexp-try-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                           [input input-port?]
                           [start-pos exact-nonnegative-integer? 0]
                           [end-pos (or/c exact-nonnegative-integer? #f) #f]
                           [output-port (or/c output-port? #f) #f]
                           [input-prefix bytes? #""])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (or/c #f (cons/c string? (listof (or/c string? #f))))
             (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))]{

Like @racket[regexp-match] on input ports, except that if the match
fails, no characters are read and discarded from @racket[in].

This procedure is especially useful with a @racket[pattern] that
begins with a start-of-string @litchar{^} or with a non-@racket[#f]
@racket[end-pos], since each limits the amount of peeking into the
port. Otherwise, beware that a large portion of the stream may be
peeked (and therefore pulled into memory) before the match succeeds or
fails.}


@defproc[(regexp-match-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                                 [input (or/c string? bytes? path? input-port?)]
                                 [start-pos exact-nonnegative-integer? 0]
                                 [end-pos (or/c exact-nonnegative-integer? #f) #f]
                                 [output-port (or/c output-port? #f) #f]
                                 [input-prefix bytes? #""])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @racket[regexp-match], but returns a list of number pairs (and
@racket[#f]) instead of a list of strings. Each pair of numbers refers
to a range of characters or bytes in @racket[input]. If the result for
the same arguments with @racket[regexp-match] would be a list of byte
strings, the resulting ranges correspond to byte ranges; in that case,
if @racket[input] is a character string, the byte ranges correspond to
bytes in the UTF-8 encoding of the string.

Range results are returned in a @racket[substring]- and
@racket[subbytes]-compatible manner, independent of
@racket[start-pos]. In the case of an input port, the returned
positions indicate the number of bytes that were read, including
@racket[start-pos], before the first matching byte.

@examples[
(regexp-match-positions #rx"x." "12x4x6")
(regexp-match-positions #rx"x." "12x4x6" 3)
(regexp-match-positions #rx"(-[0-9]*)+" "a-12--345b")
]}

@defproc[(regexp-match-positions* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                                  [input (or/c string? bytes? path? input-port?)]
                                  [start-pos exact-nonnegative-integer? 0]
                                  [end-pos (or/c exact-nonnegative-integer? #f) #f]
                                  [input-prefix bytes? #""]
                                  [#:match-select match-select
                                   (list? . -> . (or/c any/c list?))
                                   car])
         (or/c (listof (cons/c exact-nonnegative-integer?
                               exact-nonnegative-integer?))
               (listof (listof (or/c #f (cons/c exact-nonnegative-integer?
                                                exact-nonnegative-integer?)))))]{

Like @racket[regexp-match-positions], but returns multiple matches
like @racket[regexp-match*].

@examples[
(regexp-match-positions* #rx"x." "12x4x6")
(regexp-match-positions* #rx"x(.)" "12x4x6" #:match-select cadr)
]

Note that unlike @racket[regexp-match*], there is no
@racket[#:gap-select?] input keyword, as this information can be easily
inferred from the resulting matches.
}


@defproc[(regexp-match? [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? path? input-port?)]
                        [start-pos exact-nonnegative-integer? 0]
                        [end-pos (or/c exact-nonnegative-integer? #f) #f]
                        [output-port (or/c output-port? #f) #f]
                        [input-prefix bytes? #""])
           boolean?]{

Like @racket[regexp-match], but returns merely @racket[#t] when the
match succeeds, @racket[#f] otherwise.

@examples[
(regexp-match? #rx"x." "12x4x6")
(regexp-match? #rx"y." "12x4x6")
]}


@defproc[(regexp-match-exact? [pattern (or/c string? bytes? regexp? byte-regexp?)]
                              [input (or/c string? bytes? path?)])
          boolean?]{

Like @racket[regexp-match?], but @racket[#t] is only returned when the
entire content of @racket[input] matches @racket[pattern].

@examples[
(regexp-match-exact? #rx"x." "12x4x6")
(regexp-match-exact? #rx"1.*x." "12x4x6")
]}


@defproc[(regexp-match-peek [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""])
          (or/c (cons/c bytes? (listof (or/c bytes? #f)))
                #f)]{

Like @racket[regexp-match] on input ports, but only peeks bytes from
@racket[input] instead of reading them. Furthermore, instead of
an output port, the last optional argument is a progress event for
@racket[input] (see @racket[port-progress-evt]). If @racket[progress]
becomes ready, then the match stops peeking from @racket[input]
and returns @racket[#f]. The @racket[progress] argument can be
@racket[#f], in which case the peek may continue with inconsistent
information if another process meanwhile reads from
@racket[input].

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
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @racket[regexp-match-positions] on input ports, but only peeks
bytes from @racket[input] instead of reading them, and with a
@racket[progress] argument like @racket[regexp-match-peek].}


@defproc[(regexp-match-peek-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""])
          (or/c (cons/c bytes? (listof (or/c bytes? #f)))
                #f)]{

Like @racket[regexp-match-peek], but it attempts to match only bytes
that are available from @racket[input] without blocking.  The
match fails if not-yet-available characters might be used to match
@racket[pattern].}


@defproc[(regexp-match-peek-positions-immediate [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""])
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)]{

Like @racket[regexp-match-peek-positions], but it attempts to match
only bytes that are available from @racket[input] without
blocking. The match fails if not-yet-available characters might be
used to match @racket[pattern].}


@defproc[(regexp-match-peek-positions*
                            [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [input-prefix bytes? #""]
                            [#:match-select match-select
                             (list? . -> . (or/c any/c list?))
                             car])
         (or/c (listof (cons/c exact-nonnegative-integer?
                               exact-nonnegative-integer?))
               (listof (listof (or/c #f (cons/c exact-nonnegative-integer?
                                                exact-nonnegative-integer?)))))]{

Like @racket[regexp-match-peek-positions], but returns multiple matches like
@racket[regexp-match-positions*].}

@defproc[(regexp-match/end [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? path? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? #f) #f]
                       [output-port (or/c output-port? #f) #f]
                       [input-prefix bytes? #""]
                       [count exact-nonnegative-integer? 1])
         (values
          (if (and (or (string? pattern) (regexp? pattern))
                   (or/c (string? input) (path? input)))
              (or/c #f (cons/c string? (listof (or/c string? #f))))
              (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))
          (or/c #f bytes?))]{

Like @racket[regexp-match], but with a second result: a byte
string of up to @racket[count] bytes that correspond to the input
(possibly including the @racket[input-prefix]) leading to the end of
the match; the second result is @racket[#f] if no match is found.

The second result can be useful as an @racket[input-prefix] for
attempting a second match on @racket[input] starting from the end of
the first match. In that case, use @racket[regexp-max-lookbehind]
to determine an appropriate value for @racket[count].}

@deftogether[(
@defproc[(regexp-match-positions/end [pattern (or/c string? bytes? regexp? byte-regexp?)]
                                  [input (or/c string? bytes? path? input-port?)]
                                  [start-pos exact-nonnegative-integer? 0]
                                  [end-pos (or/c exact-nonnegative-integer? #f) #f]
                                  [input-prefix bytes? #""]
                                  [count exact-nonnegative-integer? 1])
         (values (listof (cons/c exact-nonnegative-integer?
                                 exact-nonnegative-integer?))
                 (or/c #f bytes?))]
@defproc[(regexp-match-peek-positions/end [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""]
                            [count exact-nonnegative-integer? 1])
         (values
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)
          (or/c #f bytes?))]
@defproc[(regexp-match-peek-positions-immediate/end [pattern (or/c string? bytes? regexp? byte-regexp?)]
                            [input input-port?]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos (or/c exact-nonnegative-integer? #f) #f]
                            [progress (or/c evt #f) #f]
                            [input-prefix bytes? #""]
                            [count exact-nonnegative-integer? 1])
         (values
          (or/c (cons/c (cons/c exact-nonnegative-integer?
                                exact-nonnegative-integer?)
                        (listof (or/c (cons/c exact-nonnegative-integer?
                                              exact-nonnegative-integer?)
                                      #f)))
                #f)
          (or/c #f bytes?))]
)]{

Like @racket[regexp-match-positions], etc., but with a second result
like @racket[regexp-match/end].}

@;------------------------------------------------------------------------
@section{Regexp Splitting}

@defproc[(regexp-split [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? #f) #f]
                       [input-prefix bytes? #""])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             (cons/c string? (listof string?))
             (cons/c bytes? (listof bytes?)))]{

The complement of @racket[regexp-match*]: the result is a list of
strings (if @racket[pattern] is a string or character regexp and
@racket[input] is a string) or byte strings (otherwise) from
@racket[input] that are separated by matches to
@racket[pattern]. Adjacent matches are separated with @racket[""] or
@racket[#""]. Zero-length matches are treated the same as for
@racket[regexp-match*].

If @racket[input] contains no matches (in the range @racket[start-pos]
to @racket[end-pos]), the result is a list containing @racket[input]'s
content (from @racket[start-pos] to @racket[end-pos]) as a single
element. If a match occurs at the beginning of @racket[input] (at
@racket[start-pos]), the resulting list will start with an empty
string or byte string, and if a match occurs at the end (at
@racket[end-pos]), the list will end with an empty string or byte
string. The @racket[end-pos] argument can be @racket[#f], in which
case splitting goes to the end of @racket[input] (which corresponds to
an end-of-file if @racket[input] is an input port).

@examples[
(regexp-split #rx" +" "12  34")
(regexp-split #rx"." "12  34")
(regexp-split #rx"" "12  34")
(regexp-split #rx" *" "12  34")
(regexp-split #px"\\b" "12, 13 and 14.")
(regexp-split #rx" +" "")
]}

@;------------------------------------------------------------------------
@section{Regexp Substitution}

@defproc[(regexp-replace [pattern (or/c string? bytes? regexp? byte-regexp?)]
                         [input (or/c string? bytes?)]
                         [insert (or/c string? bytes?
                                       ((string?) () #:rest (listof string?) . ->* . string?)
                                       ((bytes?) () #:rest (listof bytes?) . ->* . bytes?))]
                         [input-prefix bytes? #""])
         (if (and (or (string? pattern) (regexp? pattern))
                  (string? input))
             string?
             bytes?)]{

Performs a match using @racket[pattern] on @racket[input], and then
returns a string or byte string in which the matching portion of
@racket[input] is replaced with @racket[insert].  If @racket[pattern]
matches no part of @racket[input], then @racket[input] is returned
unmodified.

The @racket[insert] argument can be either a (byte) string, or a
function that returns a (byte) string. In the latter case, the
function is applied on the list of values that @racket[regexp-match]
would return (i.e., the first argument is the complete match, and then
one argument for each parenthesized sub-expression) to obtain a
replacement (byte) string.

If @racket[pattern] is a string or character regexp and @racket[input]
is a string, then @racket[insert] must be a string or a procedure that
accept strings, and the result is a string. If @racket[pattern] is a
byte string or byte regexp, or if @racket[input] is a byte string,
then @racket[insert] as a string is converted to a byte string,
@racket[insert] as a procedure is called with a byte string, and the
result is a byte string.

If @racket[insert] contains @litchar{&}, then @litchar{&}
is replaced with the matching portion of @racket[input] before it is
substituted into the match's place.  If @racket[insert] contains
@litchar{\}@nonterm{n} for some integer @nonterm{n}, then it is
replaced with the @nonterm{n}th matching sub-expression from
@racket[input]. A @litchar{&} and @litchar{\0} are aliases. If
the @nonterm{n}th sub-expression was not used in the match, or if
@nonterm{n} is greater than the number of sub-expressions in
@racket[pattern], then @litchar{\}@nonterm{n} is replaced with the
empty string.

To substitute a literal @litchar{&} or @litchar{\}, use
@litchar{\&} and @litchar{\\}, respectively, in
@racket[insert]. A @litchar{\$} in @racket[insert] is
equivalent to an empty sequence; this can be used to terminate a
number @nonterm{n} following @litchar{\}. If a @litchar{\} in
@racket[insert] is followed by anything other than a digit,
@litchar{&}, @litchar{\}, or @litchar{$}, then the @litchar{\}
by itself is treated as @litchar{\0}.

Note that the @litchar{\} described in the previous paragraphs is a
character or byte of @racket[input]. To write such an @racket[input]
as a Racket string literal, an escaping @litchar{\} is needed
before the @litchar{\}. For example, the Racket constant
@racket["\\1"] is @litchar{\1}.

@examples[
(regexp-replace #rx"mi" "mi casa" "su")
(regexp-replace #rx"mi" "mi casa" string-upcase)
(regexp-replace #rx"([Mm])i ([a-zA-Z]*)" "Mi Casa" "\\1y \\2")
(regexp-replace #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                "\\1y \\2")
(regexp-replace #rx"x" "12x4x6" "\\\\")
(display (regexp-replace #rx"x" "12x4x6" "\\\\"))
]}

@defproc[(regexp-replace* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                          [input (or/c string? bytes?)]
                          [insert (or/c string? bytes?
                                        ((string?) () #:rest (listof string?) . ->* . string?)
                                        ((bytes?) () #:rest (listof bytes?) . ->* . bytes?))]
                          [start-pos exact-nonnegative-integer? 0]
                          [end-pos (or/c exact-nonnegative-integer? #f) #f]
                          [input-prefix bytes? #""])
         (or/c string? bytes?)]{

Like @racket[regexp-replace], except that every instance of
@racket[pattern] in @racket[input] is replaced with @racket[insert],
instead of just the first match. Only non-overlapping instances of
@racket[pattern] in @racket[input] are replaced, so instances of
@racket[pattern] within inserted strings are @italic{not} replaced
recursively. Zero-length matches are treated the same as in
@racket[regexp-match*].

The optional @racket[start-pos] and @racket[end-pos] arguments select
a portion of @racket[input] for matching; the default is the entire
string or the stream up to an end-of-file.

@examples[
(regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                 "\\1y \\2")
(regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                 (lambda (all one two)
                   (string-append (string-downcase one) "y"
                                  (string-upcase two))))
(regexp-replace* #px"\\w" "hello world" string-upcase 0 5)
(display (regexp-replace* #rx"x" "12x4x6" "\\\\"))
]}

@defproc[(regexp-replaces [input (or/c string? bytes?)]
                          [replacements
                           (listof
                            (list/c (or/c string? bytes? regexp? byte-regexp?)
                                    (or/c string? bytes?
                                        ((string?) () #:rest (listof string?) . ->* . string?)
                                        ((bytes?) () #:rest (listof bytes?) . ->* . bytes?))))])
         (or/c string? bytes?)]{

Performs a chain of @racket[regexp-replace*] operations, where each
element in @racket[replacements] specifies a replacement as a
@racket[(list pattern replacement)].  The replacements are done in
order, so later replacements can apply to previous insertions.

@examples[
(regexp-replaces "zero-or-more?"
                 '([#rx"-" "_"] [#rx"(.*)\\?$" "is_\\1"]))
(regexp-replaces "zero-or-more?"
                 '([#rx"e" "o"] [#rx"o" "oo"]))
]}

@defproc*[([(regexp-replace-quote [str string?]) string?]
           [(regexp-replace-quote [bstr bytes?]) bytes?])]{

Produces a string suitable for use as the third argument to
@racket[regexp-replace] to insert the literal sequence of characters
in @racket[str] or bytes in @racket[bstr] as a replacement.
Concretely, every @litchar{\} and @litchar{&} in @racket[str] or
@racket[bstr] is protected by a quoting @litchar{\}.

@examples[
(regexp-replace #rx"UT" "Go UT!" "A&M")
(regexp-replace #rx"UT" "Go UT!" (regexp-replace-quote "A&M"))
]}
