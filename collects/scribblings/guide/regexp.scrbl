#lang scribble/doc
@(require scribble/manual scribble/eval scribble/core "guide-utils.rkt")

@title[#:tag "regexp" #:style 'toc]{Regular Expressions}

@margin-note{This chapter is a modified version of @cite["Sitaram05"].}

A @deftech{regexp} value encapsulates a pattern that is described by a
string or @tech{byte string}.  The regexp matcher tries to match this
pattern against (a portion of) another string or byte string, which we
will call the @deftech{text string}, when you call functions like
@racket[regexp-match].  The text string is treated as raw text, and
not as a pattern.

@local-table-of-contents[]

@refdetails["regexp"]{regexps}

@; ----------------------------------------

@section[#:tag "regexp-intro"]{Writing Regexp Patterns}

A string or @tech{byte string} can be used directly as a @tech{regexp}
pattern, or it can be prefixed with @litchar{#rx} to form a literal
@tech{regexp} value. For example, @racket[#rx"abc"] is a string-based
@tech{regexp} value, and @racket[#rx#"abc"] is a @tech{byte
string}-based @tech{regexp} value. Alternately, a string or byte
string can be prefixed with @litchar{#px}, as in @racket[#px"abc"],
for a slightly extended syntax of patterns within the string.

Most of the characters in a @tech{regexp} pattern are meant to match
occurrences of themselves in the @tech{text string}.  Thus, the pattern
@racket[#rx"abc"] matches a string that contains the characters
@litchar{a}, @litchar{b}, and @litchar{c} in succession. Other
characters act as @deftech{metacharacters}, and some character
sequences act as @deftech{metasequences}.  That is, they specify
something other than their literal selves.  For example, in the
pattern @racket[#rx"a.c"], the characters @litchar{a} and @litchar{c}
stand for themselves, but the @tech{metacharacter} @litchar{.} can
match @emph{any} character.  Therefore, the pattern @racket[#rx"a.c"]
matches an @litchar{a}, any character, and @litchar{c} in succession.

@margin-note{When we want a literal @litchar{\} inside a Racket string
or regexp literal, we must escape it so that it shows up in the string
at all. Racket strings use @litchar{\} as the escape character, so we
end up with two @litchar{\}s: one Racket-string @litchar{\} to escape
the regexp @litchar{\}, which then escapes the @litchar{.}.  Another
character that would need escaping inside a Racket string is
@litchar{"}.}

If we needed to match the character @litchar{.} itself, we can escape
it by precede it with a @litchar{\}.  The character sequence
@litchar{\.} is thus a @tech{metasequence}, since it doesn't match
itself but rather just @litchar{.}.  So, to match @litchar{a},
@litchar{.}, and @litchar{c} in succession, we use the regexp pattern
@racket[#rx"a\\.c"]; the double @litchar{\} is an artifact of Racket
strings, not the @tech{regexp} pattern itself.

The @racket[regexp] function takes a string or byte string and
produces a @tech{regexp} value. Use @racket[regexp] when you construct
a pattern to be matched against multiple strings, since a pattern is
compiled to a @tech{regexp} value before it can be used in a match.
The @racket[pregexp] function is like @racket[regexp], but using the
extended syntax. Regexp values as literals with @litchar{#rx} or
@litchar{#px} are compiled once and for all when they are read.


The @racket[regexp-quote] function takes an arbitrary string and
returns a string for a pattern that matches exactly the original
string. In particular, characters in the input string that could serve
as regexp metacharacters are escaped with a backslash, so that they
safely match only themselves.

@interaction[
(regexp-quote "cons")
(regexp-quote "list?")
]

The @racket[regexp-quote] function is useful when building a composite
@tech{regexp} from a mix of @tech{regexp} strings and verbatim strings.


@; ----------------------------------------

@section[#:tag "regexp-match"]{Matching Regexp Patterns}

The @racket[regexp-match-positions] function takes a @tech{regexp}
pattern and a @tech{text string}, and it returns a match if the regexp
matches (some part of) the @tech{text string}, or @racket[#f] if the regexp
did not match the string. A successful match produces a list of
@deftech{index pairs}.

@examples[
(regexp-match-positions #rx"brain" "bird")
(regexp-match-positions #rx"needle" "hay needle stack")
]

In the second example, the integers @racket[4] and @racket[10]
identify the substring that was matched. The @racket[4] is the
starting (inclusive) index, and @racket[10] the ending (exclusive)
index of the matching substring:

@interaction[
(substring "hay needle stack" 4 10)
]

In this first example, @racket[regexp-match-positions]'s return list
contains only one index pair, and that pair represents the entire
substring matched by the regexp.  When we discuss @tech{subpatterns}
later, we will see how a single match operation can yield a list of
@tech{submatch}es.

The @racket[regexp-match-positions] function takes optional third and
fourth arguments that specify the indices of the @tech{text string} within
which the matching should take place.

@interaction[
(regexp-match-positions 
 #rx"needle" 
 "his needle stack -- my needle stack -- her needle stack"
 20 39)
]

Note that the returned indices are still reckoned relative to the full
@tech{text string}.

The @racket[regexp-match] function is like
@racket[regexp-match-positions], but instead of returning index pairs,
it returns the matching substrings:

@interaction[
(regexp-match #rx"brain" "bird")
(regexp-match #rx"needle" "hay needle stack")
]

When @racket[regexp-match] is used with byte-string regexp, the result
is a matching byte substring:

@interaction[
(regexp-match #rx#"needle" #"hay needle stack")
]

@margin-note{A byte-string regexp can be applied to a string, and a
             string regexp can be applied to a byte string. In both
             cases, the result is a byte string. Internally, all
             regexp matching is in terms of bytes, and a string regexp
             is expanded to a regexp that matches UTF-8 encodings of
             characters. For maximum efficiency, use byte-string
             matching instead of string, since matching bytes directly
             avoids UTF-8 encodings.}

If you have data that is in a port, there's no need to first read it
into a string. Functions like @racket[regexp-match] can match on the
port directly:

@interaction[
(define-values (i o) (make-pipe))
(write "hay needle stack" o)
(close-output-port o)
(regexp-match #rx#"needle" i)
]

The @racket[regexp-match?] function is like
@racket[regexp-match-positions], but simply returns a boolean
indicating whether the match succeeded:

@interaction[
(regexp-match? #rx"brain" "bird")
(regexp-match? #rx"needle" "hay needle stack")
]

The @racket[regexp-split] function takes two arguments, a
@tech{regexp} pattern and a text string, and it returns a list of
substrings of the text string; the pattern identifies the delimiter
separating the substrings.

@interaction[
(regexp-split #rx":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
(regexp-split #rx" " "pea soup")
]

If the first argument matches empty strings, then the list of all the
single-character substrings is returned.

@interaction[
(regexp-split #rx"" "smithereens")
]

Thus, to identify one-or-more spaces as the delimiter, take care to
use the regexp @racket[#rx"\u20+"], not @racket[#rx"\u20*"].

@interaction[
(regexp-split #rx" +" "split pea     soup")
(regexp-split #rx" *" "split pea     soup")
]

The @racket[regexp-replace] function replaces the matched portion of
the text string by another string.  The first argument is the pattern,
the second the text string, and the third is either the string to be
inserted or a procedure to convert matches to the insert string.

@interaction[
(regexp-replace #rx"te" "liberte" "ty") 
(regexp-replace #rx"." "racket" string-upcase)
]

If the pattern doesn't occur in the text string, the returned string
is identical to the text string.

The @racket[regexp-replace*] function replaces @emph{all} matches in
the text string by the insert string:

@interaction[
(regexp-replace* #rx"te" "liberte egalite fraternite" "ty")
(regexp-replace* #rx"[ds]" "drracket" string-upcase)
]

@; ----------------------------------------

@section[#:tag "regexp-assert"]{Basic Assertions}

The @deftech{assertions} @litchar{^} and @litchar{$} identify the
beginning and the end of the text string, respectively.  They ensure
that their adjoining regexps match at one or other end of the text
string:

@interaction[
(regexp-match-positions #rx"^contact" "first contact")
]

The @tech{regexp} above fails to match because @litchar{contact} does
not occur at the beginning of the text string. In

@interaction[
(regexp-match-positions #rx"laugh$" "laugh laugh laugh laugh")
]

the regexp matches the @emph{last} @litchar{laugh}.

The metasequence @litchar{\b} asserts that a word boundary exists, but
this metasequence works only with @litchar{#px} syntax. In

@interaction[
(regexp-match-positions #px"yack\\b" "yackety yack")
]

the @litchar{yack} in @litchar{yackety} doesn't end at a word boundary
so it isn't matched.  The second @litchar{yack} does and is.

The metasequence @litchar{\B} (also @litchar{#px} only) has the
opposite effect to @litchar{\b}; it asserts that a word boundary does
not exist. In

@interaction[
(regexp-match-positions #px"an\\B" "an analysis")
]

the @litchar{an} that doesn't end in a word boundary is matched.

@; ----------------------------------------

@section[#:tag "regexp-chars"]{Characters and Character Classes}

Typically, a character in the regexp matches the same character in the
text string.  Sometimes it is necessary or convenient to use a regexp
@tech{metasequence} to refer to a single character. For example, the
metasequence @litchar{\.} matches the period character.

The @tech{metacharacter} @litchar{.} matches @emph{any} character
(other than newline in @tech{multi-line mode}; see
@secref["regexp-cloister"]):

@interaction[
(regexp-match #rx"p.t" "pet")
]

The above pattern also matches @litchar{pat}, @litchar{pit},
@litchar{pot}, @litchar{put}, and @litchar{p8t}, but not
@litchar{peat} or @litchar{pfffft}.

A @deftech{character class} matches any one character from a set of
characters.  A typical format for this is the @deftech{bracketed
character class} @litchar{[}...@litchar{]}, which matches any one
character from the non-empty sequence of characters enclosed within
the brackets.  Thus, @racket[#rx"p[aeiou]t"] matches @litchar{pat},
@litchar{pet}, @litchar{pit}, @litchar{pot}, @litchar{put}, and
nothing else.

Inside the brackets, a @litchar{-} between two characters specifies
the Unicode range between the characters.  For example,
@racket[#rx"ta[b-dgn-p]"] matches @litchar{tab}, @litchar{tac},
@litchar{tad}, @litchar{tag}, @litchar{tan}, @litchar{tao}, and
@litchar{tap}.

An initial @litchar{^} after the left bracket inverts the set
specified by the rest of the contents; i.e., it specifies the set of
characters @emph{other than} those identified in the brackets. For
example, @racket[#rx"do[^g]"] matches all three-character sequences
starting with @litchar{do} except @litchar{dog}.

Note that the @tech{metacharacter} @litchar{^} inside brackets means
something quite different from what it means outside.  Most other
@tech{metacharacters} (@litchar{.}, @litchar{*}, @litchar{+},
@litchar{?}, etc.) cease to be @tech{metacharacters} when inside
brackets, although you may still escape them for peace of mind. A
@litchar{-} is a @tech{metacharacter} only when it's inside brackets,
and when it is neither the first nor the last character between the
brackets.

Bracketed character classes cannot contain other bracketed character
classes (although they contain certain other types of character
classes; see below).  Thus, a @litchar{[} inside a bracketed character
class doesn't have to be a metacharacter; it can stand for itself.
For example, @racket[#rx"[a[b]"] matches @litchar{a}, @litchar{[}, and
@litchar{b}.

Furthermore, since empty bracketed character classes are disallowed, a
@litchar{]} immediately occurring after the opening left bracket also
doesn't need to be a metacharacter.  For example, @racket[#rx"[]ab]"]
matches @litchar{]}, @litchar{a}, and @litchar{b}.

@subsection{Some Frequently Used Character Classes}

In @litchar{#px} syntax, some standard character classes can be
conveniently represented as metasequences instead of as explicit
bracketed expressions:  @litchar{\d} matches a digit
(the same as @litchar{[0-9]}); @litchar{\s} matches an ASCII whitespace character; and
@litchar{\w} matches a character that could be part of a
``word''.

@margin-note{Following regexp custom, we identify ``word'' characters
as @litchar{[A-Za-z0-9_]}, although these are too restrictive for what
a Racketeer might consider a ``word.''}

The upper-case versions of these metasequences stand for the
inversions of the corresponding character classes: @litchar{\D}
matches a non-digit, @litchar{\S} a non-whitespace character, and
@litchar{\W} a non-``word'' character.

Remember to include a double backslash when putting these
metasequences in a Racket string:

@interaction[
(regexp-match #px"\\d\\d" 
 "0 dear, 1 have 2 read catch 22 before 9")
]

These character classes can be used inside a bracketed expression. For
example, @racket[#px"[a-z\\d]"] matches a lower-case letter or a
digit.

@subsection{POSIX character classes}

A @deftech{POSIX character class} is a special @tech{metasequence} of
the form @litchar{[:}...@litchar{:]} that can be used only inside a
bracketed expression in @litchar{#px} syntax.  The POSIX classes
supported are

@itemize[#:style (make-style "compact" null)

 @item{@litchar{[:alnum:]} --- ASCII letters and digits}

 @item{@litchar{[:alpha:]} --- ASCII letters}

 @item{@litchar{[:ascii:]} --- ASCII characters}

 @item{@litchar{[:blank:]} --- ASCII widthful whitespace: space and tab}

 @item{@litchar{[:cntrl:]} --- ``control'' characters: ASCII 0 to 32}

 @item{@litchar{[:digit:]} --- ASCII digits, same as @litchar{\d}}

 @item{@litchar{[:graph:]} --- ASCII characters that use ink}

 @item{@litchar{[:lower:]} --- ASCII lower-case letters}

 @item{@litchar{[:print:]} --- ASCII ink-users plus widthful whitespace}

 @item{@litchar{[:space:]} --- ASCII whitespace, same as @litchar{\s}}

 @item{@litchar{[:upper:]} --- ASCII upper-case letters}

 @item{@litchar{[:word:]} --- ASCII letters and @litchar{_}, same as @litchar{\w}}

 @item{@litchar{[:xdigit:]} --- ASCII hex digits}

]

For example, the @racket[#px"[[:alpha:]_]"] matches a letter or
underscore.

@interaction[
(regexp-match #px"[[:alpha:]_]" "--x--")
(regexp-match #px"[[:alpha:]_]" "--_--")
(regexp-match #px"[[:alpha:]_]" "--:--")
]

The POSIX class notation is valid @emph{only} inside a bracketed
expression.  For instance, @litchar{[:alpha:]}, when not inside a
bracketed expression, will not be read as the letter class.  Rather,
it is (from previous principles) the character class containing the
characters @litchar{:}, @litchar{a}, @litchar{l}, @litchar{p},
@litchar{h}.

@interaction[
(regexp-match #px"[:alpha:]" "--a--")
(regexp-match #px"[:alpha:]" "--x--")
]

@; ----------------------------------------

@section[#:tag "regexp-quant"]{Quantifiers}

The @deftech{quantifiers} @litchar{*}, @litchar{+}, and @litchar{?}
match respectively: zero or more, one or more, and zero or one
instances of the preceding subpattern.

@interaction[
(regexp-match-positions #rx"c[ad]*r" "cadaddadddr")
(regexp-match-positions #rx"c[ad]*r" "cr")

(regexp-match-positions #rx"c[ad]+r" "cadaddadddr")
(regexp-match-positions #rx"c[ad]+r" "cr")

(regexp-match-positions #rx"c[ad]?r" "cadaddadddr")
(regexp-match-positions #rx"c[ad]?r" "cr")
(regexp-match-positions #rx"c[ad]?r" "car")
]

In @litchar{#px} syntax, you can use braces to specify much
finer-tuned quantification than is possible with @litchar{*},
@litchar{+}, @litchar{?}:

@itemize[

 @item{The quantifier @litchar["{"]@math{m}@litchar["}"] matches
       @emph{exactly} @math{m} instances of the preceding
       @tech{subpattern}; @math{m} must be a nonnegative integer.}

 @item{The quantifier
       @litchar["{"]@math{m}@litchar{,}@math{n}@litchar["}"] matches
       at least @math{m} and at most @math{n} instances.  @litchar{m}
       and @litchar{n} are nonnegative integers with @math{m} less or
       equal to @math{n}.  You may omit either or both numbers, in
       which case @math{m} defaults to @math{0} and @math{n} to
       infinity.}

]

It is evident that @litchar{+} and @litchar{?} are abbreviations for
@litchar{{1,}} and @litchar{{0,1}} respectively, and @litchar{*}
abbreviates @litchar{{,}}, which is the same as @litchar{{0,}}.

@interaction[
(regexp-match #px"[aeiou]{3}" "vacuous")
(regexp-match #px"[aeiou]{3}" "evolve")
(regexp-match #px"[aeiou]{2,3}" "evolve")
(regexp-match #px"[aeiou]{2,3}" "zeugma")
]

The quantifiers described so far are all @deftech{greedy}: they match
the maximal number of instances that would still lead to an overall
match for the full pattern.

@interaction[
(regexp-match #rx"<.*>" "<tag1> <tag2> <tag3>")
]

To make these quantifiers @deftech{non-greedy}, append a @litchar{?}
to them.  Non-greedy quantifiers match the minimal number of instances
needed to ensure an overall match.

@interaction[
(regexp-match #rx"<.*?>" "<tag1> <tag2> <tag3>")
]

The non-greedy quantifiers are respectively: @litchar{*?},
@litchar{+?}, @litchar{??}, @litchar["{"]@math{m}@litchar["}?"],
@litchar["{"]@math{m}@litchar{,}@math{n}@litchar["}?"].  Note the two
uses of the metacharacter @litchar{?}.

@; ----------------------------------------

@section[#:tag "regexp-clusters"]{Clusters}

@deftech{Clustering}---enclosure within parens
@litchar{(}...@litchar{)}---identifies the enclosed
@deftech{subpattern} as a single entity.  It causes the matcher to
capture the @deftech{submatch}, or the portion of the string matching
the subpattern, in addition to the overall match:

@interaction[
(regexp-match #rx"([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")
]

Clustering also causes a following quantifier to treat the entire
enclosed subpattern as an entity:

@interaction[
(regexp-match #rx"(poo )*" "poo poo platter")
]

The number of submatches returned is always equal to the number of
subpatterns specified in the regexp, even if a particular subpattern
happens to match more than one substring or no substring at all.

@interaction[
(regexp-match #rx"([a-z ]+;)*" "lather; rinse; repeat;")
]

Here, the @litchar{*}-quantified subpattern matches three times, but
it is the last submatch that is returned.

It is also possible for a quantified subpattern to fail to match, even
if the overall pattern matches.  In such cases, the failing submatch
is represented by @racket[#f]

@interaction[
(define date-re
  (code:comment @#,t{match `month year' or `month day, year';})
  (code:comment @#,t{subpattern matches day, if present})
  #rx"([a-z]+) +([0-9]+,)? *([0-9]+)")
(regexp-match date-re "jan 1, 1970")
(regexp-match date-re "jan 1970")
]


@subsection{Backreferences}

@tech{Submatch}es can be used in the insert string argument of the
procedures @racket[regexp-replace] and @racket[regexp-replace*].  The
insert string can use @litchar{\}@math{n} as a @deftech{backreference}
to refer back to the @math{n}th submatch, which is the substring
that matched the @math{n}th subpattern.  A @litchar{\0} refers to the
entire match, and it can also be specified as @litchar{\&}.

@interaction[
(regexp-replace #rx"_(.+?)_" 
  "the _nina_, the _pinta_, and the _santa maria_"
  "*\\1*")
(regexp-replace* #rx"_(.+?)_" 
  "the _nina_, the _pinta_, and the _santa maria_"
  "*\\1*")

(regexp-replace #px"(\\S+) (\\S+) (\\S+)"
  "eat to live"
  "\\3 \\2 \\1")
]

Use @litchar{\\} in the insert string to specify a literal backslash.
Also, @litchar{\$} stands for an empty string, and is useful for
separating a backreference @litchar{\}@math{n} from an immediately
following number.

Backreferences can also be used within a @litchar{#px} pattern to
refer back to an already matched subpattern in the pattern.
@litchar{\}@math{n} stands for an exact repeat of the @math{n}th
submatch. Note that @litchar{\0}, which is useful in an insert string,
makes no sense within the regexp pattern, because the entire regexp
has not matched yet so you cannot refer back to it.}

@interaction[
(regexp-match #px"([a-z]+) and \\1"
              "billions and billions")
]

Note that the @tech{backreference} is not simply a repeat of the
previous subpattern.  Rather it is a repeat of the particular
substring already matched by the subpattern.

In the above example, the @tech{backreference} can only match
@litchar{billions}.  It will not match @litchar{millions}, even though
the subpattern it harks back to---@litchar{([a-z]+)}---would have had
no problem doing so:

@interaction[
(regexp-match #px"([a-z]+) and \\1"
              "billions and millions")
]

The following example marks all immediately repeating patterns in a
number string:

@interaction[
(regexp-replace* #px"(\\d+)\\1"
  "123340983242432420980980234"
  "{\\1,\\1}")
]

The following example corrects doubled words:

@interaction[
(regexp-replace* #px"\\b(\\S+) \\1\\b"
  (string-append "now is the the time for all good men to "
                 "to come to the aid of of the party")
  "\\1")
]

@subsection{Non-capturing Clusters}

It is often required to specify a cluster (typically for
quantification) but without triggering the capture of @tech{submatch}
information.  Such clusters are called @deftech{non-capturing}.  To
create a non-capturing cluster, use @litchar{(?:} instead of
@litchar{(} as the cluster opener.

In the following example, a non-capturing cluster eliminates the
``directory'' portion of a given Unix pathname, and a capturing
cluster identifies the basename.

@margin-note{But don't parse paths with regexps. Use functions like
 @racket[split-path], instead.}

@interaction[
(regexp-match #rx"^(?:[a-z]*/)*([a-z]+)$" 
              "/usr/local/bin/racket")
]

@subsection[#:tag "regexp-cloister"]{Cloisters}

The location between the @litchar{?} and the @litchar{:} of a
non-capturing cluster is called a @deftech{cloister}. You can put
modifiers there that will cause the enclustered @tech{subpattern} to
be treated specially.  The modifier @litchar{i} causes the subpattern
to match case-insensitively:

@margin-note{The term @defterm{cloister} is a useful, if terminally
cute, coinage from the abbots of Perl.}

@interaction[
(regexp-match #rx"(?i:hearth)" "HeartH")
]

The modifier @litchar{m} causes the @tech{subpattern} to match in
@deftech{multi-line mode}, where @litchar{.} does not match a newline
character, @litchar{^} can match just after a newline, and @litchar{$}
can match just before a newline.

@interaction[
(regexp-match #rx"." "\na\n")
(regexp-match #rx"(?m:.)" "\na\n")
(regexp-match #rx"^A plan$" "A man\nA plan\nA canal")
(regexp-match #rx"(?m:^A plan$)" "A man\nA plan\nA canal")
]

You can put more than one modifier in the cloister:

@interaction[
(regexp-match #rx"(?mi:^A Plan$)" "a man\na plan\na canal")
]

A minus sign before a modifier inverts its meaning.  Thus, you can use
@litchar{-i} in a @deftech{subcluster} to overturn the
case-insensitivities caused by an enclosing cluster.

@interaction[
(regexp-match #rx"(?i:the (?-i:TeX)book)"
              "The TeXbook")
]

The above regexp will allow any casing for @litchar{the} and
@litchar{book}, but it insists that @litchar{TeX} not be differently
cased.

@; ----------------------------------------

@section[#:tag "regexp-alternation"]{Alternation}

You can specify a list of @emph{alternate} @tech{subpatterns} by
separating them by @litchar{|}.  The @litchar{|} separates
@tech{subpatterns} in the nearest enclosing cluster (or in the entire
pattern string if there are no enclosing parens).

@interaction[
(regexp-match #rx"f(ee|i|o|um)" "a small, final fee")
(regexp-replace* #rx"([yi])s(e[sdr]?|ing|ation)"
                 (string-append
                  "analyse an energising organisation"
                  " pulsing with noisy organisms")
                 "\\1z\\2")
]
 
Note again that if you wish to use clustering merely to specify a list
of alternate subpatterns but do not want the submatch, use
@litchar{(?:} instead of @litchar{(}.

@interaction[
(regexp-match #rx"f(?:ee|i|o|um)" "fun for all")
]

An important thing to note about alternation is that the leftmost
matching alternate is picked regardless of its length.  Thus, if one
of the alternates is a prefix of a later alternate, the latter may not
have a chance to match.

@interaction[
(regexp-match #rx"call|call-with-current-continuation" 
              "call-with-current-continuation")
]

To allow the longer alternate to have a shot at matching, place it
before the shorter one:

@interaction[
(regexp-match #rx"call-with-current-continuation|call"
              "call-with-current-continuation")
]

In any case, an overall match for the entire regexp is always
preferred to an overall non-match.  In the following, the longer
alternate still wins, because its preferred shorter prefix fails to
yield an overall match.

@interaction[
(regexp-match
 #rx"(?:call|call-with-current-continuation) constrained"
 "call-with-current-continuation constrained")
]

@; ----------------------------------------

@section{Backtracking}

We've already seen that greedy quantifiers match the maximal number of
times, but the overriding priority is that the overall match succeed.
Consider

@interaction[
(regexp-match #rx"a*a" "aaaa")
]

The regexp consists of two subregexps: @litchar{a*} followed by
@litchar{a}.  The subregexp @litchar{a*} cannot be allowed to match
all four @litchar{a}'s in the text string @racket[aaaa], even though
@litchar{*} is a greedy quantifier.  It may match only the first
three, leaving the last one for the second subregexp.  This ensures
that the full regexp matches successfully.

The regexp matcher accomplishes this via a process called
@deftech{backtracking}.  The matcher tentatively allows the greedy
quantifier to match all four @litchar{a}'s, but then when it becomes
clear that the overall match is in jeopardy, it @emph{backtracks} to a
less greedy match of three @litchar{a}'s.  If even this fails, as in
the call

@interaction[
(regexp-match #rx"a*aa" "aaaa")
]

the matcher backtracks even further.  Overall failure is conceded
only when all possible backtracking has been tried with no success.

Backtracking is not restricted to greedy quantifiers.
Nongreedy quantifiers match as few instances as
possible, and progressively backtrack to more and more
instances in order to attain an overall match.  There
is backtracking in alternation too, as the more
rightward alternates are tried when locally successful
leftward ones fail to yield an overall match.

Sometimes it is efficient to disable backtracking.  For example, we
may wish to commit to a choice, or we know that trying alternatives is
fruitless.  A nonbacktracking regexp is enclosed in
@litchar{(?>}...@litchar{)}.

@interaction[
(regexp-match #rx"(?>a+)." "aaaa")
]

In this call, the subregexp @litchar{?>a+} greedily matches all four
@litchar{a}'s, and is denied the opportunity to backtrack.  So, the
overall match is denied.  The effect of the regexp is therefore to
match one or more @litchar{a}'s followed by something that is
definitely non-@litchar{a}.

@; ----------------------------------------

@section{Looking Ahead and Behind}

You can have assertions in your pattern that look @emph{ahead} or
@emph{behind} to ensure that a subpattern does or does not occur.
These ``look around'' assertions are specified by putting the
subpattern checked for in a cluster whose leading characters are:
@litchar{?=} (for positive lookahead), @litchar{?!} (negative
lookahead), @litchar{?<=} (positive lookbehind), @litchar{?<!}
(negative lookbehind).  Note that the subpattern in the assertion does
not generate a match in the final result; it merely allows or
disallows the rest of the match.

@subsection{Lookahead}

Positive lookahead with @litchar{?=} peeks ahead to ensure that
its subpattern @emph{could} match.  

@interaction[
(regexp-match-positions #rx"grey(?=hound)" 
  "i left my grey socks at the greyhound") 
]

The regexp @racket[#rx"grey(?=hound)"] matches @litchar{grey}, but
@emph{only} if it is followed by @litchar{hound}.  Thus, the first
@litchar{grey} in the text string is not matched.

Negative lookahead with @litchar{?!} peeks ahead to ensure that its
subpattern @emph{could not} possibly match.

@interaction[
(regexp-match-positions #rx"grey(?!hound)"
  "the gray greyhound ate the grey socks") 
]

The regexp @racket[#rx"grey(?!hound)"] matches @litchar{grey}, but
only if it is @emph{not} followed by @litchar{hound}.  Thus the
@litchar{grey} just before @litchar{socks} is matched.

@subsection{Lookbehind}

Positive lookbehind with @litchar{?<=} checks that its subpattern
@emph{could} match immediately to the left of the current position in
the text string.

@interaction[
(regexp-match-positions #rx"(?<=grey)hound"
  "the hound in the picture is not a greyhound") 
]

The regexp @racket[#rx"(?<=grey)hound"] matches @litchar{hound}, but
only if it is preceded by @litchar{grey}.

Negative lookbehind with @litchar{?<!} checks that its subpattern
could not possibly match immediately to the left.

@interaction[
(regexp-match-positions #rx"(?<!grey)hound"
  "the greyhound in the picture is not a hound")
]

The regexp @racket[#rx"(?<!grey)hound"] matches @litchar{hound}, but
only if it is @emph{not} preceded by @litchar{grey}.

Lookaheads and lookbehinds can be convenient when they
are not confusing.  

@; ----------------------------------------

@section{An Extended Example}

@(define ex-eval (make-base-eval))

Here's an extended example from Friedl's @italic{Mastering Regular
Expressions}, page 189, that covers many of the features described in
this chapter.  The problem is to fashion a regexp that will match any
and only IP addresses or @emph{dotted quads}: four numbers separated
by three dots, with each number between 0 and 255.

First, we define a subregexp @racket[n0-255] that matches 0 through
255:

@interaction[
#:eval ex-eval
(define n0-255
  (string-append
   "(?:"
   "\\d|"        (code:comment @#,t{  0 through   9})
   "\\d\\d|"     (code:comment @#,t{ 00 through  99})
   "[01]\\d\\d|" (code:comment @#,t{000 through 199})
   "2[0-4]\\d|"  (code:comment @#,t{200 through 249})
   "25[0-5]"     (code:comment @#,t{250 through 255})
   ")"))
]

@margin-note{Note that @racket[n0-255] lists prefixes as preferred
alternates, which is something we cautioned against in
@secref["regexp-alternation"].  However, since we intend to anchor
this subregexp explicitly to force an overall match, the order of the
alternates does not matter.}

The first two alternates simply get all single- and
double-digit numbers.  Since 0-padding is allowed, we
need to match both 1 and 01.  We need to be careful
when getting 3-digit numbers, since numbers above 255
must be excluded.  So we fashion alternates to get 000
through 199, then 200 through 249, and finally 250
through 255.

An IP-address is a string that consists of four @racket[n0-255]s with
three dots separating them.

@interaction[
#:eval ex-eval
(define ip-re1
  (string-append
   "^"        (code:comment @#,t{nothing before})
   n0-255     (code:comment @#,t{the first @racket[n0-255],})
   "(?:"      (code:comment @#,t{then the subpattern of})
   "\\."      (code:comment @#,t{a dot followed by})
   n0-255     (code:comment @#,t{an @racket[n0-255],})
   ")"        (code:comment @#,t{which is})
   "{3}"      (code:comment @#,t{repeated exactly 3 times})
   "$"))      (code:comment @#,t{with nothing following})
]

Let's try it out:

@interaction[
#:eval ex-eval
(regexp-match (pregexp ip-re1) "1.2.3.4")
(regexp-match (pregexp ip-re1) "55.155.255.265")
]

which is fine, except that we also have

@interaction[
#:eval ex-eval
(regexp-match (pregexp ip-re1) "0.00.000.00")
]

All-zero sequences are not valid IP addresses!  Lookahead to the
rescue.  Before starting to match @racket[ip-re1], we look ahead to
ensure we don't have all zeros.  We could use positive lookahead to
ensure there @emph{is} a digit other than zero.

@interaction[
#:eval ex-eval
(define ip-re
  (pregexp
   (string-append
     "(?=.*[1-9])" (code:comment @#,t{ensure there's a non-0 digit})
     ip-re1)))
]

Or we could use negative lookahead to ensure that what's ahead isn't
composed of @emph{only} zeros and dots.

@interaction[
#:eval ex-eval
(define ip-re
  (pregexp
   (string-append
     "(?![0.]*$)" (code:comment @#,t{not just zeros and dots})
                  (code:comment @#,t{(note: @litchar{.} is not metachar inside @litchar{[}...@litchar{]})})
     ip-re1)))
]

The regexp @racket[ip-re] will match all and only valid IP addresses.

@interaction[
#:eval ex-eval
(regexp-match ip-re "1.2.3.4")
(regexp-match ip-re "0.0.0.0")
]

@close-eval[ex-eval]
