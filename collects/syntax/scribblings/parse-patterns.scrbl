#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          scheme/sandbox
          (for-label scheme/base
                     scheme/contract
                     syntax/parse
                     syntax/kerncase))

@(define ellipses @scheme[...])
@(define-syntax-rule (defhere id) (defidentifier #'id #:form? #t))

@(begin
   (define-syntax ref
     (syntax-rules ()
       [(ref id suffix ...)
        (elemref (list 'pattern-link (list 'id 'suffix ...))
                 (schemekeywordfont (symbol->string 'id))
                 (superscript (symbol->string 'suffix)) ...
                 #:underline? #f)]))
   (define-syntax def
     (syntax-rules ()
       [(def id suffix ...)
        (elemtag (list 'pattern-link (list 'id 'suffix ...))
                 (scheme id)
                 #|(superscript (symbol->string 'suffix)) ...|# )])))

@(begin
   (define (fixup exn)
     (let ([src (ormap values (exn:fail:syntax-exprs exn))])
       (if src
           (make-exn:fail:syntax
            (format "~a at: ~a" (exn-message exn) (syntax->datum src))
            (exn-continuation-marks exn)
            (exn:fail:syntax-exprs exn))
           exn)))
   (define the-eval
     (parameterize ((sandbox-output 'string)
                    (sandbox-error-output 'string)
                    (sandbox-make-code-inspector current-code-inspector)
                    (sandbox-eval-handlers
                     (list #f
                           (lambda (thunk)
                             (with-handlers ([exn:fail:syntax?
                                              (lambda (e) (raise (fixup e)))])
                               (thunk))))))
       (make-evaluator 'scheme/base
                       #:requires '(syntax/parse (for-syntax scheme/base)))))
   (the-eval '(error-print-source-location #f))
   (define-syntax-rule (myexamples e ...)
     (examples #:eval the-eval e ...)))

@title[#:tag "syntax-patterns"]{Syntax patterns}

The grammar of @deftech{syntax patterns} used by
@schememodname[syntax/parse] facilities is given in the following
table. There are three main kinds of syntax pattern: @tech{S-patterns}
(for ``single-term patterns''), @tech{H-patterns} (for ``head
patterns''), and @tech{EH-patterns} (for ``ellipsis head
patterns''). A fourth kind, @tech{L-patterns} (for ``list patterns''),
is a restricted subset of @tech{S-patterns}. 

When a special form in this manual refers to @svar[syntax-pattern]
(eg, the description of the @scheme[syntax-parse] special form), it
means specifically @tech{S-pattern}.

@schemegrammar*[#:literals (_ ~var ~literal ~or ~and ~not ~seq 
                            ~rep ~once ~optional
                            ~rest ~struct ~! ~describe ~bind ~fail)
                [S-pattern
                 pvar-id
                 pvar-id:syntax-class-id
                 literal-id
                 (@#,ref[~var s-] id)
                 (@#,ref[~var s+] id syntax-class)
                 (~literal literal-id)
                 atomic-datum
                 (H-pattern . S-pattern)
                 ((@#,ref[~or eh] EH-pattern ...+) #,ellipses . S-pattern)
                 (EH-pattern #,ellipses . S-pattern)
                 (@#,ref[~and s] S-pattern ...+)
                 (@#,ref[~or s] S-pattern ...+)
                 (~not S-pattern)
                 #((unsyntax @svar[pattern-part]) ...)
                 #s(prefab-struct-key (unsyntax @svar[pattern-part]) ...)
                 (~rest S-pattern)
                 (@#,ref[~describe s] expr S-pattern)
                 (~! . S-pattern)
                 (~bind [attr-id expr] ...)
                 (~fail maybe-fail-condition message-expr)]
                [L-pattern
                 ()
                 (H-pattern . L-pattern)
                 ((@#,ref[~or eh] EH-pattern ...+) #,ellipses . L-pattern)
                 (EH-pattern #,ellipses . L-pattern)
                 (~rest L-pattern)
                 (~! . L-pattern)]
                [H-pattern
                 pvar-id:splicing-syntax-class-id
                 (@#,ref[~var h] id splicing-syntax-class)
                 (~seq . L-pattern)
                 (@#,ref[~and h] strict-H-pattern ...+)
                 (@#,ref[~or h] H-pattern ...+)
                 (@#,ref[~optional h] H-pattern maybe-optional-option)
                 (@#,ref[~describe h] expr H-pattern)
                 S-pattern]
                [EH-pattern
                 (~once H-pattern once-option ...)
                 (@#,ref[~optional eh] H-pattern optional-option ...)
                 H-pattern]]

The following pattern keywords can be used in multiple pattern
variants:

@defidform[~var]{ 

Pattern keyword; see @ref[~var s-], @ref[~var s+], or @ref[~var h].
}

@defidform[~and]{

Pattern keyword; see @ref[~and s] or @ref[~and h].
}

@defidform[~or]{

Pattern keyword; see @ref[~or s], @ref[~or h]), or @ref[~or eh].
}

@defidform[~describe]{

Pattern keyword; see @ref[~describe s] or @ref[~describe h].
}

@defidform[~optional]{

Pattern keyword; see @ref[~optional h] or @ref[~optional eh].
}


@;{--------}

@section{S-pattern variants}

An @deftech{S-pattern} (for ``single-term pattern'') is a pattern that
describes a single term. The pattern may, of course, consist of other
parts. For example, @scheme[(17 ...)] is an @tech{S-pattern}
that matches any term that is a proper list of repeated
@schemeresult[17] numerals. The @deftech{L-pattern}s (for ``list
pattern'') are @tech{S-pattern} having a restricted structure that
constrains it to match only terms that are proper lists.

Here are the variants of @tech{S-pattern}:

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

@specsubform/subs[(@#,def[~var s+] pvar-id syntax-class)
                  ([syntax-class syntax-class-id
                                 (syntax-class-id arg-expr ...)])]{

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

@specsubform[(H-pattern . S-pattern)]{

Matches any term that can be decomposed into a list prefix matching
the @tech{H-pattern} and a suffix matching the S-pattern.

Note that the pattern may match terms that are not even improper
lists; if the head pattern can match a zero-length head, then the
whole pattern matches whatever the tail pattern accepts.

The first pattern can be an @tech{S-pattern}, in which case the whole
pattern matches any pair whose first element matches the first pattern
and whose rest matches the second.

See @tech{H-patterns} for more information.
}

@specsubform[((@#,def[~or eh] EH-pattern ...+) #,ellipses . S-pattern)]{

Matches any term that can be decomposed into a list head matching some
number of repetitions of the @tech{EH-pattern} alternatives (subject
to its repetition constraints) followed by a list tail matching the
S-pattern.

In other words, the whole pattern matches either the second pattern
(which need not be a list) or a term whose head matches one of the
alternatives of the first pattern and whose tail recursively matches
the whole sequence pattern.

See @tech{EH-patterns} for more information.
}

@specsubform[(EH-pattern #,ellipses . S-pattern)]{

The @scheme[~or]-free variant of ellipses (@ellipses) pattern is
equivalent to the @scheme[~or] variant with just one alternative.
}

@specsubform[(@#,def[~and s] S-pattern ...)]{

Matches any term that matches all of the subpatterns.

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
  [(~or x:id (~and x #f)) (syntax->datum #'x)])
(syntax-parse #'#f
  [(~or x:id (~and x #f)) (syntax->datum #'x)])
]
}

@specsubform[(@#,defhere[~not] S-pattern)]{

Matches any term that does not match the subpattern. The subpattern's
attributes are @emph{not} bound outside of the @scheme[~not]-pattern.

@myexamples[
(syntax-parse #'(x y z => u v)
  #:literals (=>)
  [((~and before (~not =>)) ... => after ...)
   (list #'(before ...) #'(after ...))])
]
}

@specsubform[#(#, @svar[pattern-part] ...)]{

Matches a term that is a vector whose elements, when considered as a
list, match the @tech{S-pattern} corresponding to
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
@tech{S-pattern} corresponding to @scheme[(pattern-part ...)].

@myexamples[
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y z) 'ok])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x y ...) (syntax->datum #'(y ...))])
(syntax-parse #'#s(point 1 2 3)
  [#s(point x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[(#, @defhere[~rest] S-pattern)]{

Matches just like the inner @scheme[S-pattern]. The @scheme[~rest]
pattern form is useful in positions where improper lists (``dots'')
are not allowed by the reader, such as vector and structure patterns
(see above).

@myexamples[
(syntax-parse #'(1 2 3)
  [(x ~rest y) (syntax->datum #'y)])
(syntax-parse #'#(1 2 3)
  [#(x ~rest y) (syntax->datum #'y)])
]
}

@specsubform[(@#,def[~describe s] expr S-pattern)]{

The @scheme[~describe] pattern form annotates a pattern with a
description, a string expression that is evaluated in the scope of all
prior attribute bindings. If parsing the inner pattern fails, then the
description is used to synthesize the error message.

A describe-pattern also affects backtracking in two ways:

@itemize{

@item{A cut-pattern (@scheme[~!]) within a describe-pattern only
eliminates choice-points created within the describe-pattern.}

@item{If a describe-pattern succeeds, then all choice points created
within the describe-pattern are discarded, and a failure @emph{after}
the describe-pattern backtracks to a choice point @emph{before} the
describe-pattern, never one @emph{within} it.}}}

@specsubform[(@#,defhere[~!] . S-pattern)]{

The @scheme[~!] operator, pronounced ``cut'', eliminates backtracking
choice points and commits parsing to the current branch of the pattern
it is exploring.

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
ill-formed use of @scheme[define-values]! The proper way to write the
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
enclosing @scheme[~describe] pattern. If there is no enclosing
@scheme[~describe] pattern but the cut occurs within a syntax class
definition, then only choice points within the syntax class definition
are discarded.
}

@specsubform[(@#,defhere[~bind] [attr-id expr] ...)]{

This pattern matches any term. Its effect is to evaluate the
@scheme[expr]s and bind them to the given @scheme[attr-id]s as
attributes.
}

@specsubform/subs[(@#,defhere[~fail] maybe-fail-condition message-expr)
                  ([maybe-fail-condition (code:line)
                                         (code:line #:when condition-expr)
                                         (code:line #:unless condition-expr)])]{

This pattern succeeds or fails independent of the term being matched
against. If the condition is absent, or if the @scheme[#:when]
condition evaluates to a true value, or if the @scheme[#:unless]
condition evaluates to @scheme[#f], then the pattern fails with the
given message. Otherwise the pattern succeeds.

Fail patterns can be used together with cut patterns to recognize
specific ill-formed terms and address them with specially-created
failure messages.
}


@section{H-pattern variants}

An @deftech{H-pattern} (for ``head pattern'') is a pattern that
describes some number of terms that occur at the head of some list
(possibly an improper list). An H-pattern's usefulness comes from
being able to match heads of different lengths.  H-patterns are useful
for specifying optional forms such as keyword arguments.

Here are the variants of @tech{H-pattern}:

@specsubform[pvar-id:splicing-syntax-class-id]{

Equivalent to @scheme[(~var pvar-id
splicing-syntax-class-id)].

}

@specsubform/subs[(@#,def[~var h] pvar-id splicing-syntax-class)
                  ([splicing-syntax-class splicing-syntax-class-id
                                          (splicing-syntax-class-id arg-expr ...)])]{

Pattern variable annotated with a @tech{splicing syntax
class}. Similar to a normal @tech{annotated pattern variable}, except
matches a head pattern.
}

@specsubform[(@#,defhere[~seq] . L-pattern)]{

Matches a head whose elements, if put in a list, would match the given
@tech{L-pattern}.

@myexamples[
(syntax-parse #'(1 2 3 4)
  [((~seq 1 2 3) 4) 'ok])
]

See also the section on @tech{EH-patterns} for more interesting
examples of @scheme[~seq].

}

@specsubform[(@#,def[~and h] H-pattern ...)]{

Like the S-pattern version of @scheme[~and], but matches a term head
instead.

@myexamples[
(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
]

The H-pattern variant of @scheme[~and] requires that all of the
subpatterns be strictly @tech{H-patterns} and not
@tech{S-patterns}. This is to prevent typos like the following, a
variant of the previous example with the second @scheme[~seq] omitted:

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

Like the S-pattern version of @scheme[~or], but matches a term head
instead.

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

Matches either the given head subpattern or an empty head. If the
@scheme[#:defaults] option is given, the subsequent attribute bindings
are used if the subpattern does not match. The default attributes must
be a subset of the subpattern's attributes.

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

Like the S-pattern version of @scheme[~describe], but matches a head
pattern instead.
}

@specsubform[S-pattern]{

Matches a head of one element, which must be a term matching the given
@tech{S-pattern}.
}


@;{--------}

@section{EH-pattern forms}

An @deftech{EH-pattern} (for ``ellipsis-head pattern'') is pattern
that describes some number of terms, like an @tech{H-pattern}, but may
also place contraints on the number of times it occurs in a
repetition. EH-patterns (and ellipses) are useful for matching keyword
arguments where the keywords may come in any order.

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

Here are the variants of @tech{EH-pattern}:

@specsubform/subs[(@#,defhere[~once] H-pattern once-option ...)
                  ([once-option (code:line #:name name-expr)
                                (code:line #:too-few too-few-message-expr)
                                (code:line #:too-many too-many-message-expr)])]{

Matches if the inner H-pattern matches. This pattern must be selected
exactly once in the match of the entire repetition sequence.

If the pattern is not chosen in the repetition sequence, then an error
is raised with a message, either @scheme[too-few-message-expr] or
@schemevalfont{"missing required occurrence of @scheme[name-expr]"}.

If the pattern is chosen more than once in the repetition sequence,
then an error is raised with a message, either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.
}

@specsubform/subs[(@#,def[~optional eh] H-pattern optional-option ...)
                  ([optional-option (code:line #:name name-expr)
                                    (code:line #:too-many too-many-message-expr)
                                    (code:line #:defaults ([attr-id expr] ...))])]{

Matches if the inner H-pattern matches. This pattern may be used at
most once in the match of the entire repetition.

If the pattern is chosen more than once in the repetition sequence,
then an error is raised with a message, either
@scheme[too-many-message-expr] or @schemevalfont{"too many occurrences
of @scheme[name-expr]"}.

If the @scheme[#:defaults] option is given, the following attribute
bindings are used if the subpattern does not match at all in the
sequence. The default attributes must be a subset of the subpattern's
attributes.
}
