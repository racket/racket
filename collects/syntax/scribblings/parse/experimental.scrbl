#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title{Experimental}

The following facilities are experimental.

@section{Contracts for Macro Sub-expressions}

@defmodule[syntax/parse/experimental/contract]

This module is deprecated; it reprovides @racket[expr/c] for backward
compatibility.

@section{Contracts for Syntax Classes}

@defmodule[syntax/parse/experimental/provide]

@defform[#:literals (syntax-class/c)
         (provide-syntax-class/contract
           [syntax-class-id syntax-class-contract] ...)
         #:grammar
         ([syntax-class-contract
           (syntax-class/c (mandatory-arg ...))
           (syntax-class/c (mandatory-arg ...)
                           (optional-arg ...))]
          [arg contract-expr (code:line keyword contract-expr)])
         #:contracts ([contract-expr contract?])]{

Provides the syntax class (or splicing syntax class)
@racket[syntax-class-id] with the given contracts imposed on its
formal parameters.
}

@defidform[syntax-class/c]{

Keyword recognized by @racket[provide-syntax-class/contract].
}

@section{Reflection}

@defmodule[syntax/parse/experimental/reflect]

A syntax class can be reified into a run-time value, and a reified
syntax class can be used in a pattern via the @racket[~reflect] and
@racket[~splicing-reflect] pattern forms.

@defform[(reify-syntax-class syntax-class-id)]{

Reifies the syntax class named @racket[syntax-class-id] as a run-time
value. The same form also handles splicing syntax classes. Syntax
classes with the @racket[#:no-delimit-cut] option cannot be reified.
}

@deftogether[(
@defproc[(reified-syntax-class? [x any/c]) boolean?]
@defproc[(reified-splicing-syntax-class? [x any/c]) boolean?])]{

Returns @racket[#t] if @racket[x] is a reified (normal) syntax class
or a reified splicing syntax class, respectively.
}

@defproc[(reified-syntax-class-attributes
           [r (or/c reified-syntax-class? reified-splicing-syntax-class?)])
         (listof (list/c symbol? exact-nonnegative-integer?))]{

Returns the reified syntax class's attributes.
}

@deftogether[[
@defproc[(reified-syntax-class-arity
           [r (or/c reified-syntax-class? reified-splicing-syntax-class?)])
         procedure-arity?]
@defproc[(reified-syntax-class-keywords
           [r (or/c reified-syntax-class? reified-splicing-syntax-class?)])
         (values (listof keyword?) (listof keyword?))]]]{

Returns the reified syntax class's arity and keywords,
respectively. Compare with @racket[procedure-arity] and
@racket[procedure-keywords].
}

@defproc[(reified-syntax-class-curry
           [r (or/c reified-syntax-class? reified-splicing-syntax-class?)]
           [arg any/c] ...
           [#:<kw> kw-arg any/c] ...)
         (or/c reified-syntax-class? reified-splicing-syntax-class?)]{

Partially applies the reified syntax class to the given arguments. If
more arguments are given than the reified syntax class accepts, an
error is raised.
}

@racketgrammar*[#:literals (~reflect ~splicing-reflect)
                [S-pattern ....
                           (~reflect var-id (reified-expr arg-expr ...) maybe-attrs)]
                [H-pattern ....
                           (~splicing-reflect var-id (reified-expr arg-expr ...)
                                              maybe-attrs)]]

@specsubform[(@#,(defhere ~reflect) var-id (reified-expr arg-expr ...) maybe-attrs)
             #:grammar
         ([maybe-attrs (code:line)
                                  (code:line #:attributes (attr-arity-decl ...))])]{

Like @racket[~var], except that the syntax class position is an
expression evaluating to a reified syntax object, not a syntax class
name, and the attributes bound by the reified syntax class (if any)
must be specified explicitly.
}

@specsubform[(@#,(defhere ~splicing-reflect) var-id (reified-expr arg-expr ...) maybe-attrs)]{

Like @racket[~reflect] but for reified splicing syntax classes.
}

@myexamples[
(define-syntax-class (nat> x)
  #:description (format "natural number greater than ~s" x)
  #:attributes (diff)
  (pattern n:nat
           #:when (> (syntax-e #'n) x)
           #:with diff (- (syntax-e #'n) x)))
(define-syntax-class (nat/mult x)
  #:description (format "natural number multiple of ~s" x)
  #:attributes (quot)
  (pattern n:nat
           #:when (zero? (remainder (syntax-e #'n) x))
           #:with quot (quotient (syntax-e #'n) x)))

(define r-nat> (reify-syntax-class nat>))
(define r-nat/mult (reify-syntax-class nat/mult))

(define (partition/r stx r n)
  (syntax-parse stx
    [((~or (~reflect yes (r n)) no) ...)
     #'((yes ...) (no ...))]))

(partition/r #'(1 2 3 4 5) r-nat> 3)
(partition/r #'(1 2 3 4 5) r-nat/mult 2)

(define (bad-attrs r)
  (syntax-parse #'6
    [(~reflect x (r 3) #:attributes (diff))
     #'x.diff]))

(bad-attrs r-nat>)
(bad-attrs r-nat/mult)
]

@;{--------}

@section{Procedural Splicing Syntax Classes}

@defmodule[syntax/parse/experimental/splicing]

@defform[(define-primitive-splicing-syntax-class (name-id param-id ...) 
           maybe-description maybe-attrs
           parser-expr)
          #:contracts ([parser (-> syntax?
                                   (->* () ((or/c string? #f) -> any))
                                   (cons/c exact-positive-integer? list?))])]{

Defines a splicing syntax via a procedural parser.

The parser procedure is given two arguments, the syntax to parse and a
failure procedure. To signal a successful parse, the parser procedure
returns a list of @racket[N]+1 elements, where @racket[N] is the
number of attributes declared by the splicing syntax class. The first
element is the size of the prefix consumed. The rest of the list
contains the values of the attributes.

To indicate failure, the parser calls the failure procedure with an
optional message argument.
}

@;{--------}

@section{Ellipsis-head Alternative Sets}

@defmodule[syntax/parse/experimental/eh]

Unlike @tech{@Spatterns} and @tech{@Hpatterns}, @tech{@EHpatterns}
cannot be encapsulated by syntax classes, since they describe not only
sets of terms but also repetition constraints.

This module provides @deftech{ellipsis-head alternative sets},
reusable encapsulations of @|EHpatterns|.

@defform[#:literals (pattern)
         (define-eh-alternative-set name eh-alternative ...)
         #:grammar
         ([alternative (pattern EH-pattern)])]{

Defines @racket[name] as an ellipsis-head alternative set. Using
@racket[name] (via @racket[~eh-var]) in an ellipsis-head pattern is
equivalent to including each of the alternatives in the pattern via
@ref[~or eh], except that the attributes bound by the alternatives are
prefixed with the name given to @racket[~eh-var].

Unlike syntax classes, ellipsis-head alternative sets must be defined
before they are referenced.
}

@racketgrammar*[#:literals (~eh-var)
                [EH-pattern ....
                            (~eh-var name eh-alternative-set-id)]]

@specsubform[(@#,(defhere ~eh-var) name eh-alternative-set-id)]{

Includes the alternatives of @racket[eh-alternative-set-id], prefixing
their attributes with @racket[name].
}

@myexamples[
(define-eh-alternative-set options
  (pattern (~once (~seq #:a a:expr) #:name "#:a option"))
  (pattern (~seq #:b b:expr)))
(define (parse/options stx)
  (syntax-parse stx
    [(_ (~eh-var s options) ...)
     #'(s.a (s.b ...))]))
(parse/options #'(m #:a 1 #:b 2 #:b 3))
(parse/options #'(m #:a 1 #:a 2))

(define (parse/more-options stx)
  (syntax-parse stx
    [(_ (~or (~eh-var s options)
             (~seq #:c c1:expr c2:expr))
        ...)
     #'(s.a (s.b ...) ((c1 c2) ...))]))
(parse/more-options #'(m #:a 1 #:b 2 #:c 3 4 #:c 5 6))

(define-eh-alternative-set ext-options
  (pattern (~eh-var s options))
  (pattern (~seq #:c c1 c2)))

(syntax-parse #'(m #:a 1 #:b 2 #:c 3 4 #:c 5 6)
  [(_ (~eh-var x ext-options) ...)
   #'(x.s.a (x.s.b ...) ((x.c1 x.c2) ...))])
]


@section{Syntax Class Specialization}

@defmodule[syntax/parse/experimental/specialize]

@defform[(define-syntax-class/specialize header syntax-class-use)
         #:grammar
         ([header id
                  (id . kw-formals)]
          [syntax-class-use target-stxclass-id
                            (target-stxclass-id arg ...)])]{

Defines @racket[id] as a syntax class with the same attributes,
options (eg, @racket[#:commit], @racket[#:no-delimit-cut]), and
patterns as @racket[target-stxclass-id] but with the given
@racket[arg]s supplied.

@examples[#:eval the-eval
(define-syntax-class/specialize nat>10 (nat> 10))

(syntax-parse #'(11 12) [(n:nat>10 ...) 'ok])
(syntax-parse #'(8 9) [(n:nat>10 ...) 'ok])
]
}


@section{Syntax Templates}

@defmodule[syntax/parse/experimental/template]

@(define literal-ellipsis (racket ...))

@defform[#:literals (?? ?@)
         (template tmpl)
         #:grammar
         ([tmpl pattern-variable-id
                (head-tmpl . tmpl)
                (head-tmpl ellipsis ...+ . tmpl)
                (metafunction-id . tmpl)
                (?? tmpl tmpl)
                #(@#,svar[head-tmpl] ...)
                #s(prefab-struct-key @#,svar[head-tmpl] ...)
                #&@#,svar[tmpl]
                constant-term]
          [head-templ tmpl
                      (?? head-tmpl)
                      (?? head-tmpl head-tmpl)
                      (?@ . tmpl)]
          [ellipsis @#,literal-ellipsis])]{

Constructs a syntax object from a syntax template, like
@racket[syntax], but provides additional templating forms for dealing
with optional terms and splicing sequences of terms. Only the
additional forms are described here; see @racket[syntax] for
descriptions of pattern variables, etc.

@specsubform[#:literals (??)
             (?? tmpl alt-tmpl)]{

Produces @racket[tmpl] unless any attribute used in @racket[tmpl] has
an absent value; in that case, @racket[alt-tmpl] is used instead.

@examples[#:eval the-eval
(syntax-parse #'(m 1 2 3)
  [(_ (~optional (~seq #:op op:expr)) arg:expr ...)
   (template ((?? op +) arg ...))])
(syntax-parse #'(m #:op max 1 2 3)
  [(_ (~optional (~seq #:op op:expr)) arg:expr ...)
   (template ((?? op +) arg ...))])
]

If @racket[??] is used as a head-template, then its sub-templates may
also be head-templates.

@examples[#:eval the-eval
(syntax-parse #'(m 1)
  [(_ x:expr (~optional y:expr))
   (template (m2 x (?? (?@ #:y y) (?@ #:z 0))))])
(syntax-parse #'(m 1 2)
  [(_ x:expr (~optional y:expr))
   (template (m2 x (?? (?@ #:y y) (?@ #:z 0))))])
]
}

@specsubform[#:literals (??)
             (?? head-tmpl)]{

Produces @racket[head-tmpl] unless any attribute used in
@racket[head-tmpl] has an absent value; in that case, the term is
omitted.  Can only occur in head position in a template.

Equivalent to @racket[(?? head-tmpl (?@))].

@examples[#:eval the-eval
(syntax-parse #'(m 1)
  [(_ x:expr (~optional y:expr))
   (template (m2 x (?? y)))])
(syntax-parse #'(m 1 2)
  [(_ x:expr (~optional y:expr))
   (template (m2 x (?? y)))])
(syntax-parse #'(m 1 2)
  [(_ x:expr (~optional y:expr))
   (template (m2 x (?? (?@ #:y y))))])
]
}

@specsubform[#:literals (?@)
             (?@ . tmpl)]{

Similar to @racket[unquote-splicing], splices the result of
@racket[tmpl] (which must produce a syntax list) into the surrounding
template. Can only occur in head position in a template.

@examples[#:eval the-eval
(syntax-parse #'(m #:a 1 #:b 2 3 4 #:e 5)
  [(_ (~or pos:expr (~seq kw:keyword kwarg:expr)) ...)
   (template (m2 (?@ kw kwarg) ... pos ...))])
]

The @racket[tmpl] must produce a proper syntax list, but it does not
need to be expressed as a proper list. For example, to unpack pattern
variables that contain syntax lists, use a ``dotted'' template:

@examples[#:eval the-eval
(with-syntax ([x #'(a b c)])
  (template ((?@ . x) d)))
(with-syntax ([(x ...) #'((1 2 3) (4 5))])
  (template ((?@ . x) ...)))
]
}

@specsubform[(metafunction-id . tmpl)]{

Applies the template metafunction named @racket[metafunction-id] to
the result of the template (including @racket[metafunction-id]
itself). See @racket[define-template-metafunction] for examples.
}

The @racket[??] and @racket[?@] forms and metafunction applications
are disabled in an ``escaped template'' (see @racket[_stat-template]
under @racket[syntax]).

@examples[#:eval the-eval
(template (... ((?@ a b c) d)))
]
}

@deftogether[[
@defidform[??]
@defidform[?@]
]]{

Auxiliary forms used by @racket[template]. They may not be used as
expressions.
}

@defform*[[(define-template-metafunction metafunction-id expr)
           (define-template-metafunction (metafunction-id . formals) body ...+)]]{

Defines @racket[metafunction-id] as a @deftech{template
metafunction}. A metafunction application in a @racket[template]
expression (but not a @racket[syntax] expression) is evaluated by
applying the metafunction to the result of processing the ``argument''
part of the template.

@examples[#:eval the-eval
(define-template-metafunction (join stx)
  (syntax-parse stx
    [(join (~optional (~seq #:lctx lctx)) a:id b:id ...)
     (datum->syntax (or (attribute lctx) #'a)
                    (string->symbol
                     (apply string-append
                            (map symbol->string
                                 (syntax->datum #'(a b ...)))))
                    stx)]))
(template (join a b c))
(with-syntax ([(x ...) #'(a b c)])
  (template ((x (join tmp- x)) ...)))
]

Metafunctions are useful for performing transformations in contexts
where macro expansion does not occur, such as binding occurrences. For
example:

@interaction[#:eval the-eval
(syntax->datum 
 (with-syntax ([name #'posn]
               [(field ...) #'(x y)])
   (template (let-values ([((join name ?) 
                            (join #:lctx name make- name)
                            (join name - field) ...)
                           (make-struct-type ___)])
               ___))))
]

If @racket[join] were defined as a macro, it would not be usable in
the context above; instead, @racket[let-values] would report an
invalid binding list.
}
