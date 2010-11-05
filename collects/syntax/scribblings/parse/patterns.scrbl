#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@(define-syntax-rule (define-dotsplus-names dotsplus def-dotsplus)
   (begin (require (for-label (only-in syntax/parse ...+)))
          (define dotsplus (scheme ...+))
          (define def-dotsplus (defhere ...+))))
@(define-dotsplus-names dotsplus def-dotsplus)

@title[#:tag "stxparse-patterns"]{Syntax patterns}

The grammar of @deftech{syntax patterns} used by
@schememodname[syntax/parse] facilities is given in the following
table. There are four main kinds of syntax pattern:
@itemize[
@item{@tech{@Spatterns}, abbreviated @svar[S-pattern]}
@item{@tech{@Hpatterns}, abbreviated @svar[H-pattern]}
@item{@tech{@EHpatterns}, abbreviated @svar[EH-pattern]}
@item{@tech{@Apatterns}, abbreviated @svar[A-pattern]}
]
A fifth kind, @tech{@Lpatterns} (abbreviated
@svar[L-pattern]), is a just a syntactically restricted subset of
@tech{@Spatterns}.

When a special form in this manual refers to @svar[syntax-pattern]
(eg, the description of the @scheme[syntax-parse] special form), it
means specifically @tech{@Spattern}.

@schemegrammar*[#:literals (_ ~var ~literal ~or ~and ~not ~rest ~datum
                            ~describe ~seq ~optional ~rep ~once ~between
                            ~! ~bind ~fail ~parse ~peek ~peek-not)
                [S-pattern
                 pvar-id
                 pvar-id:syntax-class-id
                 literal-id
                 (@#,ref[~var s-] id)
                 (@#,ref[~var s+] id syntax-class-id)
                 (@#,ref[~var s+] id (syntax-class-id arg ...))
                 (~literal literal-id)
                 atomic-datum
                 (~datum datum)
                 (H-pattern . S-pattern)
                 (A-pattern . S-pattern)
                 (EH-pattern #,ellipses . S-pattern)
                 (H-pattern @#,dotsplus . S-pattern)
                 (@#,ref[~and s] proper-S/A-pattern ...+)
                 (@#,ref[~or s] S-pattern ...+)
                 (~not S-pattern)
                 #((unsyntax @svar[pattern-part]) ...)
                 #s(prefab-struct-key (unsyntax @svar[pattern-part]) ...)
                 #&@#,svar[S-pattern]
                 (~rest S-pattern)
                 (@#,ref[~describe s] maybe-opaque expr S-pattern)
                 (@#,ref[~commit s] S-pattern)
                 (@#,ref[~delimit-cut s] S-pattern)
                 A-pattern]
                [L-pattern
                 ()
                 (A-pattern . L-pattern)
                 (H-pattern . L-pattern)
                 (EH-pattern #,ellipses . L-pattern)
                 (H-pattern @#,dotsplus . L-pattern)
                 (~rest L-pattern)]
                [H-pattern
                 pvar-id:splicing-syntax-class-id
                 (@#,ref[~var h] id splicing-syntax-class-id)
                 (@#,ref[~var h] id (splicing-syntax-class-id arg ...))
                 (~seq . L-pattern)
                 (@#,ref[~and h] proper-H/A-pattern ...+)
                 (@#,ref[~or h] H-pattern ...+)
                 (@#,ref[~optional h] H-pattern maybe-optional-option)
                 (@#,ref[~describe h] maybe-opaque expr H-pattern)
                 (@#,ref[~commit h] H-pattern)
                 (@#,ref[~delimit-cut h] H-pattern)
                 (~peek H-pattern)
                 (~peek-not H-pattern)
                 proper-S-pattern]
                [EH-pattern
                 (@#,ref[~or eh] EH-pattern ...)
                 (~once H-pattern once-option ...)
                 (@#,ref[~optional eh] H-pattern optional-option ...)
                 (~between H min-number max-number between-option)
                 H-pattern]
                [A-pattern
                 ~!
                 (~bind [attr-arity-decl expr] ...)
                 (~fail maybe-fail-condition maybe-message-expr)
                 (~parse S-pattern stx-expr)
                 (@#,ref[~and a] A-pattern ...+)]
                [proper-S-pattern
                 #, @elem{a @svar{S-pattern} that is not a @svar{A-pattern}}]
                [proper-H-pattern
                 #, @elem{a @svar{H-pattern} that is not a @svar{S-pattern}}]]

The following pattern keywords can be used in multiple pattern
variants:

@defidform[~var]{ 

One of @ref[~var s-], @ref[~var s+], or @ref[~var h].
}

@defidform[~and]{

One of @ref[~and s], @ref[~and h], or @ref[~and a]:
@itemize[
@item{@ref[~and a] if all of the conjuncts are @tech{@Apatterns}}
@item{@ref[~and h] if any of the conjuncts is a @tech{proper
@Hpattern}}
@item{@ref[~and s] otherwise}
]
}

@defidform[~or]{

One of @ref[~or s], @ref[~or h], or @ref[~or eh]:
@itemize[
@item{@ref[~or eh] if the pattern occurs directly before ellipses
  (@ellipses) or immediately within another @ref[~or eh] pattern}
@item{@ref[~or h] if any of the disjuncts is a @tech{proper @Hpattern}}
@item{@ref[~or s] otherwise}
]
}

@defidform[~describe]{

One of @ref[~describe s] or @ref[~describe h]:
@itemize[
@item{@ref[~describe h] if the subpattern is a @tech{proper @Hpattern}}
@item{@ref[~describe s] otherwise}
]
}

@defidform[~commit]{

One of @ref[~commit s] or @ref[~commit h]:
@itemize[
@item{@ref[~commit h] if the subpattern is a @tech{proper @Hpattern}}
@item{@ref[~commit s] otherwise}
]
}

@defidform[~delimit-cut]{

One of @ref[~delimit-cut s] or @ref[~describe h]:
@itemize[
@item{@ref[~delimit-cut h] if the subpattern is a @tech{proper @Hpattern}}
@item{@ref[~delimit-cut s] otherwise}
]
}

@defidform[~optional]{

One of @ref[~optional h] or @ref[~optional eh]:
@itemize[
@item{@ref[~optional eh] if it is an immediate disjunct of a @ref[~or
eh] pattern}
@item{@ref[~optional h] otherwise}
]
}


@;{--------}

@section{Single-term patterns}

A @deftech{@Spattern} (abbreviated @svar[S-pattern]) is a pattern that
describes a single term. These are like the traditional patterns used
in @scheme[syntax-rules] and @scheme[syntax-case], but with additional
variants that make them more expressive.

``Single-term'' does not mean ``atomic''; a @Spattern can have
complex structure, and it can match terms that have many parts. For
example, @scheme[(17 ...)] is a @Spattern that matches any
term that is a proper list of repeated @schemeresult[17] numerals. 

A @deftech{proper @Spattern} is one that is not an @tech{@Apattern}.

The @deftech{@Lpatterns} (for ``list pattern'') are @Spatterns
having a restricted structure that guarantees that they match only
terms that are proper lists.

Here are the variants of @elem{@Spattern}:

@specsubform[id]{

An identifier can be either a @tech{pattern variable}, an
@tech{annotated pattern variable}, or a @tech{literal}:

@itemize[

@item{If @scheme[id] is the ``pattern'' name of an entry in the
  literals list, it is a @tech{literal} pattern that behaves
  like @scheme[(~literal id)].

  @myexamples[
    (syntax-parse #'(define x 12)
      #:literals (define)
      [(define var:id body:expr) 'ok])
    (syntax-parse #'(lambda x 12)
      #:literals (define)
      [(define var:id body:expr) 'ok])
    (syntax-parse #'(define x 12)
      #:literals ([def define])
      [(def var:id body:expr) 'ok])
    (syntax-parse #'(lambda x 12)
      #:literals ([def define])
      [(def var:id body:expr) 'ok])
  ]
}

@item{If @scheme[id] is of the form @scheme[_pvar-id:syntax-class-id]
  (that is, two names joined by a colon character), it is an
  @tech{annotated pattern variable}, and the pattern is equivalent to
  @scheme[(~var pvar-id syntax-class-id)].

  @myexamples[
    (syntax-parse #'a
      [var:id (syntax-e #'var)])
    (syntax-parse #'12
      [var:id (syntax-e #'var)])
    (define-syntax-class two
      #:attributes (x y)
      (pattern (x y)))
  (syntax-parse #'(a b)
    [t:two (syntax->datum #'(t t.x t.y))])
  (syntax-parse #'(a b)
    [t
     #:declare t two
     (syntax->datum #'(t t.x t.y))])
  ]
}

@item{Otherwise, @scheme[id] is a @tech{pattern variable}, and the
pattern is equivalent to @scheme[(~var id)].
}
]
}

@specsubform[(@#,def[~var s-] pvar-id)]{

A @deftech{pattern variable}. If @scheme[pvar-id] has no syntax class
(by @scheme[#:convention]), the pattern variable matches anything. The
pattern variable is bound to the matched subterm, unless the pattern
variable is the wildcard (@scheme[_]), in which case no binding
occurs.

If @scheme[pvar-id] does have an associated syntax class, it behaves
like an @tech{annotated pattern variable} with the implicit syntax
class inserted.
}

@specsubform/subs[(@#,def[~var s+] pvar-id syntax-class-use)
                  ([syntax-class-use syntax-class-id
                                     (syntax-class-id arg ...)])]{

An @deftech{annotated pattern variable}. The pattern matches only
terms accepted by @svar[syntax-class-id] (parameterized by the
@scheme[arg-expr]s, if present).

In addition to binding @svar[pvar-id], an annotated pattern
variable also binds @deftech{nested attributes} from the syntax
class. The names of the nested attributes are formed by prefixing
@svar[pvar-id.] (that is, @svar[pvar-id] followed by a ``dot''
character) to the name of the syntax class's attribute.

If @svar[pvar-id] is @scheme[_], no attributes are bound.

@myexamples[
(syntax-parse #'a
  [(~var var id) (syntax-e #'var)])
(syntax-parse #'12
  [(~var var id) (syntax-e #'var)])
(define-syntax-class two
  #:attributes (x y)
  (pattern (x y)))
(syntax-parse #'(a b)
  [(~var t two) (syntax->datum #'(t t.x t.y))])
(define-syntax-class (nat-less-than n)
  (pattern x:nat #:when (< (syntax-e #'x) n)))
(syntax-parse #'(1 2 3 4 5)
  [((~var small (nat-less-than 4)) ... large:nat ...)
   (list #'(small ...) #'(large ...))])
]
}

@specsubform[(@#,defhere[~literal] literal-id)]{

A @deftech{literal} identifier pattern. Matches any identifier
@scheme[free-identifier=?] to @scheme[literal-id].

@myexamples[
(syntax-parse #'(define x 12)
  [((~literal define) var:id body:expr) 'ok])
(syntax-parse #'(lambda x 12)
  [((~literal define) var:id body:expr) 'ok])
]
}

@specsubform[atomic-datum]{

Numbers, strings, booleans, keywords, and the empty list match as
literals.

@myexamples[
(syntax-parse #'(a #:foo bar)
  [(x #:foo y) (syntax->datum #'y)])
(syntax-parse #'(a foo bar)
  [(x #:foo y) (syntax->datum #'y)])
]
}

@specsubform[(@#,defhere[~datum] datum)]{

Matches syntax whose S-expression contents (obtained by
@scheme[syntax->datum]) is @scheme[equal?] to the given
@scheme[datum].

@myexamples[
(syntax-parse #'(a #:foo bar)
  [(x (~datum #:foo) y) (syntax->datum #'y)])
(syntax-parse #'(a foo bar)
  [(x (~datum #:foo) y) (syntax->datum #'y)])
]

The @scheme[~datum] form is useful for recognizing identifiers
symbolically, in contrast to the @scheme[~literal] form, which
recognizes them by binding.

@myexamples[
(syntax-parse (let ([define 'something-else]) #'(define x y))
  [((~datum define) var:id e:expr) 'yes]
  [_ 'no])
(syntax-parse (let ([define 'something-else]) #'(define x y))
  [((~literal define) var:id e:expr) 'yes]
  [_ 'no])
]
}

@specsubform[(H-pattern . S-pattern)]{

Matches any term that can be decomposed into a list prefix matching
@scheme[H-pattern] and a suffix matching @scheme[S-pattern].

Note that the pattern may match terms that are not even improper
lists; if the head pattern can match a zero-length head, then the
whole pattern matches whatever the tail pattern accepts.

The first pattern can be a @tech{@Spattern}, in which case the whole
pattern matches any pair whose first element matches the first pattern
and whose rest matches the second.

See @tech{@Hpatterns} for more information.
}

@specsubform[(A-pattern . S-pattern)]{

Performs the actions specified by @scheme[A-pattern], then matches
any term that matches @scheme[S-pattern].

Pragmatically, one can throw an @tech{@Apattern} into any list
pattern. Thus, @scheme[(x y z)] is a pattern matching a list of three
terms, and @scheme[(x y ~! z)] is a pattern matching a list of three
terms, with a @tech{cut} performed after the second one. In other
words, @Apatterns ``don't take up space.''

See @tech{@Apatterns} for more information.
}

@specsubform[(EH-pattern #,ellipses . S-pattern)]{

Matches any term that can be decomposed into a list head matching some
number of repetitions of the @scheme[EH-pattern] alternatives (subject
to its repetition constraints) followed by a list tail matching
@scheme[S-pattern].

In other words, the whole pattern matches either the second pattern
(which need not be a list) or a term whose head matches one of the
alternatives of the first pattern and whose tail recursively matches
the whole sequence pattern.

See @tech{@EHpatterns} for more information.
}

@specsubform[(H-pattern @#,def-dotsplus . S-pattern)]{

Like an ellipses (@ellipses) pattern, but requires at one occurrence
of the head pattern to be present.

That is, the following patterns are equivalent:
@itemize[
@item[@scheme[(H @#,dotsplus . S)]]
@item[@scheme[((~between H 1 +inf.0) ... . S)]]
]

@myexamples[
(syntax-parse #'(1 2 3)
  [(n:nat ...+) 'ok])
(syntax-parse #'()
  [(n:nat ...+) 'ok]
  [_ 'none])
]

}

@specsubform[(@#,def[~and s] S/A-pattern ...)]{

Matches any term that matches all of the subpatterns.

The subpatterns can contain a mixture of @tech{@Spatterns} and
@tech{@Apatterns}, but must contain at least one @tech{@Spattern}.

Attributes bound in subpatterns are available to subsequent
subpatterns. The whole pattern binds all of the subpatterns'
attributes.

One use for @scheme[~and]-patterns is preserving a whole
term (including its lexical context, source location, etc) while also
examining its structure. Syntax classes are useful for the same
purpose, but @scheme[~and] can be lighter weight.

@myexamples[
(define-syntax (import stx)
  (raise-syntax-error #f "illegal use of import" stx))
(eval:alts
  (define (check-imports stx) ....)
  (define (check-imports . _) #f))
(syntax-parse #'(m (import one two))
  #:literals (import)
  [(_ (~and import-clause (import i ...)))
   (let ([bad (check-imports
               (syntax->list #'(i ...)))])
     (when bad
       (raise-syntax-error
        #f "bad import" #'import-clause bad))
     'ok)])
]
}

@specsubform[(@#,def[~or s] S-pattern ...)]{

Matches any term that matches one of the included patterns. The
alternatives are tried in order.

The whole pattern binds @emph{all} of the subpatterns' attributes. An
attribute that is not bound by the ``chosen'' subpattern has a value
of @scheme[#f]. The same attribute may be bound by multiple
subpatterns, and if it is bound by all of the subpatterns, it is sure
to have a value if the whole pattern matches.

@myexamples[
(syntax-parse #'a
  [(~or x:id y:nat) (values (attribute x) (attribute y))])
(syntax-parse #'(a 1)
  [(~or (x:id y:nat) (x:id)) (values #'x (attribute y))])
(syntax-parse #'(b)
  [(~or (x:id y:nat) (x:id)) (values #'x (attribute y))])
]
}

@specsubform[(@#,defhere[~not] S-pattern)]{

Matches any term that does not match the subpattern. None of the
subpattern's attributes are bound outside of the
@scheme[~not]-pattern.

@myexamples[
(syntax-parse #'(x y z => u v)
  #:literals (=>)
  [((~and before (~not =>)) ... => after ...)
   (list #'(before ...) #'(after ...))])
]
}

@specsubform[#(#, @svar[pattern-part] ...)]{

Matches a term that is a vector whose elements, when considered as a
list, match the @tech{@Spattern} corresponding to
@scheme[(pattern-part ...)].

@myexamples[
(syntax-parse #'#(1 2 3)
  [#(x y z) (syntax->datum #'z)])
(syntax-parse #'#(1 2 3)
  [#(x y ...) (syntax->datum #'(y ...))])
(syntax-parse #'#(1 2 3)
  [#(x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[#s(prefab-struct-key #, @svar[pattern-part] ...)]{

Matches a term that is a prefab struct whose key is exactly the given
key and whose sequence of fields, when considered as a list, match the
@tech{@Spattern} corresponding to @scheme[(pattern-part ...)].

@myexamples[
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y z) 'ok])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y ...) (syntax->datum #'(y ...))])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[#&@#,svar[S-pattern]]{ 

Matches a term that is a box whose contents matches the inner
@tech{@Spattern}.

@myexamples[
(syntax-parse #'#&5
  [#&n:nat 'ok])
]
}

@specsubform[(#, @defhere[~rest] S-pattern)]{

Matches just like @scheme[S-pattern]. The @scheme[~rest] pattern form
is useful in positions where improper (``dotted'') lists are not
allowed by the reader, such as vector and structure patterns (see
above).

@myexamples[
(syntax-parse #'(1 2 3)
  [(x ~rest y) (syntax->datum #'y)])
(syntax-parse #'#(1 2 3)
  [#(x ~rest y) (syntax->datum #'y)])
]
}

@specsubform/subs[(@#,def[~describe s] maybe-opaque expr S-pattern)
                  ([maybe-opaque (code:line)
                                 (code:line #:opaque)])
                  #:contracts ([expr (or/c string? #f)])]{

The @scheme[~describe] pattern form annotates a pattern with a
description, a string expression that is evaluated in the scope of all
prior attribute bindings. If parsing the inner pattern fails, then the
description is used to synthesize the error message.

A @scheme[~describe] pattern has no effect on backtracking.
}

@specsubform[(@#,def[~commit s] S-pattern)]{

The @scheme[~commit] pattern form affects backtracking in two ways:

@itemize[

@item{If the pattern succeeds, then all choice points created within
the subpattern are discarded, and a failure @emph{after} the
@scheme[~commit] pattern backtracks only to choice points
@emph{before} the @scheme[~commit] pattern, never one @emph{within}
it.}

@item{A cut (@scheme[~!]) within a @scheme[~commit] pattern only
eliminates choice-points created within the @scheme[~commit]
pattern. In this sense, it acts just like @scheme[~delimit-cut].}
]
}

@specsubform[(@#,def[~delimit-cut s] S-pattern)]{

The @scheme[~delimit-cut] pattern form affects backtracking in the
following way:

@itemize[

@item{A cut (@scheme[~!]) within a @scheme[~delimit-cut] pattern only
eliminates choice-points created within the @scheme[~delimit-cut]
pattern.}

]
}

@specsubform[A-pattern]{

An @tech{@Apattern} is considered a @Spattern when there is no
ambiguity; it matches any term.
}


@section{Head patterns}

A @deftech{@Hpattern} (abbreviated @svar[H-pattern]) is a pattern that
describes some number of terms that occur at the head of some list
(possibly an improper list). A @Hpattern's usefulness comes from being
able to match heads of different lengths, such as optional forms like
keyword arguments.

A @deftech{proper @Hpattern} is a @Hpattern that is not a @elem{@Spattern}.

Here are the variants of @elem{@Hpattern}:

@specsubform[pvar-id:splicing-syntax-class-id]{

Equivalent to @scheme[(~var pvar-id splicing-syntax-class-id)].

}

@specsubform/subs[(@#,def[~var h] pvar-id splicing-syntax-class-use)
                  ([splicing-syntax-class-use splicing-syntax-class-id
                                              (splicing-syntax-class-id arg ...)])]{

Pattern variable annotated with a @tech{splicing syntax
class}. Similar to a normal @tech{annotated pattern variable}, except
matches a head pattern.
}

@specsubform[(@#,defhere[~seq] . L-pattern)]{

Matches a sequence of terms whose elements, if put in a list, would
match @scheme[L-pattern].

@myexamples[
(syntax-parse #'(1 2 3 4)
  [((~seq 1 2 3) 4) 'ok])
]

See also the section on @tech{@EHpatterns} for more interesting
examples of @scheme[~seq].
}

@specsubform[(@#,def[~and h] H-pattern ...)]{

Like the @Spattern version, @ref[~and s], but matches a sequence of
terms instead.

@myexamples[
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
]

The @Hpattern variant of @scheme[~and] requires that all of the
subpatterns be @tech{proper @Hpatterns} (not @tech{@Spatterns}). This
is to prevent typos like the following, a variant of the previous
example with the second @scheme[~seq] omitted:

@myexamples[
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
(code:comment "If the example above were allowed, it would be equivalent to this:")
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq (keyword-stuff ...)))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
]
}

@specsubform[(@#,def[~or h] H-pattern ...)]{

Like the @Spattern version, @ref[~or s], but matches a sequence of
terms instead.

@myexamples[
(syntax-parse #'(m #:foo 2 a b c)
  [(_ (~or (~seq #:foo x) (~seq)) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~or (~seq #:foo x) (~seq)) y:id ...)
   (attribute x)])
]
}

@specsubform/subs[(@#,def[~optional h] H-pattern maybe-optional-option)
                  ([maybe-optional-option
                    (code:line)
                    (code:line #:defaults ([attr-id expr] ...))])]{

Matches either the given head subpattern or an empty sequence of
terms. If the @scheme[#:defaults] option is given, the subsequent
attribute bindings are used if the subpattern does not match. The
default attributes must be a subset of the subpattern's attributes.

@myexamples[
(syntax-parse #'(m #:foo 2 a b c)
  [(_ (~optional (~seq #:foo x) #:defaults ([x #'#f])) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~optional (~seq #:foo x) #:defaults ([x #'#f])) y:id ...)
   (attribute x)])
(syntax-parse #'(m a b c)
  [(_ (~optional (~seq #:foo x)) y:id ...)
   (attribute x)])
]

}

@specsubform[(@#,def[~describe h] expr H-pattern)]{

Like the @Spattern version, @ref[~describe s], but matches a head
pattern instead.
}

@specsubform[(@#,def[~commit h] H-pattern)]{

Like the @Spattern version, @ref[~commit s], but matches a head
pattern instead.
}

@specsubform[(@#,def[~delimit-cut h] H-pattern)]{

Like the @Spattern version, @ref[~delimit-cut s], but matches a head
pattern instead.
}

@specsubform[(@#,defhere[~peek] H-pattern)]{

Matches the @racket[H-pattern] but then resets the matching position,
so the @racket[~peek] pattern consumes no input. Used to look ahead in
a sequence.

@examples[#:eval the-eval
(define-splicing-syntax-class nf-id (code:comment "non-final id")
  (pattern (~seq x:id (~peek another:id))))

(syntax-parse #'(a b c 1 2 3)
  [(n:nf-id ... rest ...)
   (printf "nf-ids are ~s\n" (syntax->datum #'(n.x ...)))
   (printf "rest is ~s\n" (syntax->datum #'(rest ...)))])
]
}

@specsubform[(@#,defhere[~peek-not] H-pattern)]{

Like @racket[~peek], but succeeds if the subpattern fails and fails if
the subpattern succeeds. On success, the @racket[~peek-not] resets the
matching position, so the pattern consumes no input. Used to look
ahead in a sequence. None of the subpattern's attributes are bound
outside of the @scheme[~peek-not]-pattern.

@myexamples[
(define-splicing-syntax-class final (code:comment "final term")
  (pattern (~seq x (~peek-not _))))

(syntax-parse #'(a b c)
  [((~or f:final o:other) ...)
   (printf "finals are ~s\n" (syntax->datum #'(f.x ...)))
   (printf "others are ~s\n" (syntax->datum #'(o ...)))])
]
}

@specsubform[S-pattern]{

Matches a sequence of one element, which must be a term matching
@scheme[S-pattern].
}


@;{--------}

@section{Ellipsis-head patterns}

An @deftech{@EHpattern} (abbreviated @svar[EH-pattern]) is pattern
that describes some number of terms, like a @tech{@Hpattern}, but also
places contraints on the number of times it occurs in a
repetition. They are useful for matching, for example, keyword
arguments where the keywords may come in any order. Multiple
alternatives are grouped together via @ref[~or eh].

@myexamples[
(define parser1
  (syntax-parser
   [((~or (~once (~seq #:a x) #:name "#:a keyword")
          (~optional (~seq #:b y) #:name "#:b keyword")
          (~seq #:c z)) ...)
    'ok]))
(parser1 #'(#:a 1))
(parser1 #'(#:b 2 #:c 3 #:c 25 #:a 'hi))
(parser1 #'(#:a 1 #:a 2))
]

The pattern requires exactly one occurrence of the @scheme[#:a]
keyword and argument, at most one occurrence of the @scheme[#:b]
keyword and argument, and any number of @scheme[#:c] keywords and
arguments. The ``pieces'' can occur in any order.

Here are the variants of @elem{@EHpattern}:

@specsubform[(@#,def[~or eh] EH-pattern ...)]{

Matches if any of the inner @scheme[EH-pattern] alternatives match.
}

@specsubform/subs[(@#,defhere[~once] H-pattern once-option ...)
                  ([once-option (code:line #:name name-expr)
                                (code:line #:too-few too-few-message-expr)
                                (code:line #:too-many too-many-message-expr)])
                  #:contracts ([name-expr (or/c string? #f)]
                               [too-few-message-expr (or/c string? #f)]
                               [too-many-message-expr (or/c string? #f)])]{

Matches if the inner @scheme[H-pattern] matches. This pattern must be
matched exactly once in the match of the entire repetition sequence.

If the pattern is not matched in the repetition sequence, then the
ellipsis pattern fails with the message either
@scheme[too-few-message-expr] or @schemevalfont{"missing required
occurrence of @scheme[name-expr]"}.

If the pattern is chosen more than once in the repetition sequence,
then the ellipsis pattern fails with the message either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.
}

@specsubform/subs[(@#,def[~optional eh] H-pattern optional-option ...)
                  ([optional-option (code:line #:name name-expr)
                                    (code:line #:too-many too-many-message-expr)
                                    (code:line #:defaults ([attr-id expr] ...))])
                  #:contracts ([name-expr (or/c string? #f)]
                               [too-many-message-expr (or/c string? #f)])]{

Matches if the inner @scheme[H-pattern] matches. This pattern may be used at
most once in the match of the entire repetition.

If the pattern is matched more than once in the repetition sequence,
then the ellipsis pattern fails with the message either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.

If the @scheme[#:defaults] option is given, the following attribute
bindings are used if the subpattern does not match at all in the
sequence. The default attributes must be a subset of the subpattern's
attributes.
}

@specsubform/subs[(@#,defhere[~between] H-pattern min-number max-number between-option ...)
                  ([reps-option (code:line #:name name-expr)
                                (code:line #:too-few too-few-message-expr)
                                (code:line #:too-many too-many-message-expr)])
                  #:contracts ([name-expr (or/c syntax? #f)]
                               [too-few-message-expr (or/c syntax? #f)])]{

Matches if the inner @scheme[H-pattern] matches. This pattern must be
matched at least @scheme[min-number] and at most @scheme[max-number]
times in the entire repetition.

If the pattern is matched too few times, then the ellipsis pattern
fails with the message either @scheme[too-few-message-expr] or
@schemevalfont{"too few occurrences of @scheme[name-expr]"}.

If the pattern is chosen too many times, then the ellipsis pattern
fails with the message either @scheme[too-many-message-expr] or
@schemevalfont{"too few occurrences of @scheme[name-expr]"}.
}



@;{--------}

@section{Action patterns}

An @deftech{@Apattern} (abbreviated @svar[A-pattern]) does not
describe any syntax; rather, it has an effect such as the binding of
attributes or the modification of the matching process.

@specsubform[@#,defhere[~!]]{

The @deftech{cut} operator, written @scheme[~!], eliminates
backtracking choice points and commits parsing to the current branch
of the pattern it is exploring.

Common opportunities for cut-patterns come from recognizing special
forms based on keywords. Consider the following expression:

@interaction[#:eval the-eval
(syntax-parse #'(define-values a 123)
  #:literals (define-values define-syntaxes)
  [(define-values (x:id ...) e) 'define-values]
  [(define-syntaxes (x:id ...) e) 'define-syntaxes]
  [e 'expression])]

Given the ill-formed term @scheme[(define-values a 123)], the
expression tries the first clause, fails to match @scheme[a] against
the pattern @scheme[(x:id ...)], and then backtracks to the second
clause and ultimately the third clause, producing the value
@scheme['expression]. But the term is not an expression; it is an
ill-formed use of @scheme[define-values]. The proper way to write the
@scheme[syntax-parse] expression follows:

@interaction[#:eval the-eval
(syntax-parse #'(define-values a 123)
  #:literals (define-values define-syntaxes)
  [(define-values ~! (x:id ...) e) 'define-values]
  [(define-syntaxes ~! (x:id ...) e) 'define-syntaxes]
  [e 'expression])]

Now, given the same term, @scheme[syntax-parse] tries the first
clause, and since the keyword @scheme[define-values] matches, the
cut-pattern commits to the current pattern, eliminating the choice
points for the second and third clauses. So when the clause fails to
match, the @scheme[syntax-parse] expression raises an error.

The effect of a @scheme[~!] pattern is delimited by the nearest
enclosing @scheme[~delimit-cut] or @scheme[~commit] pattern. If there
is no enclosing @scheme[~describe] pattern but the cut occurs within a
syntax class definition, then only choice points within the syntax
class definition are discarded. A @scheme[~!]  pattern is not allowed
within a @scheme[~not] pattern unless there is an intervening
@scheme[~delimit-cut] or @scheme[~commit] pattern.
}

@specsubform/subs[(@#,defhere[~bind] [attr-arity-decl expr] ...)
                  ([attr-arity-decl
                    attr-name-id
                    (attr-name-id depth)])]{

Evaluates the @scheme[expr]s and binds them to the given
@scheme[attr-id]s as attributes.
}

@specsubform/subs[(@#,defhere[~fail] maybe-fail-condition maybe-message-expr)
                  ([maybe-fail-condition (code:line)
                                         (code:line #:when condition-expr)
                                         (code:line #:unless condition-expr)]
                   [maybe-message-expr (code:line)
                                       (code:line message-expr)])
                  #:contracts ([message-expr (or/c string? #f)])]{

If the condition is absent, or if the @scheme[#:when] condition
evaluates to a true value, or if the @scheme[#:unless] condition
evaluates to @scheme[#f], then the pattern fails with the given
message. If the message is omitted, the default value @scheme[#f] is
used, representing ``no message.''

Fail patterns can be used together with cut patterns to recognize
specific ill-formed terms and address them with custom failure
messages.
}

@specsubform[(@#,defhere[~parse] S-pattern stx-expr)
             #:contracts ([stx-expr syntax?])]{

Evaluates @scheme[stx-expr] to a syntax object and matches it against
@scheme[S-pattern].
}

@specsubform[(@#,def[~and a] A-pattern ...+)]{

Performs the actions of each @scheme[A-pattern].
}
