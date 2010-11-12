#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title{Experimental}

The following facilities are experimental.

@section{Contracts for macro sub-expressions}

@defmodule[syntax/parse/experimental/contract]

Macros can apply contracts to their sub-expressions using the
@defidentifier[#'expr/c #:form? #t] syntax class.

@defproc[(expr/c [contract-expr syntax?]
                 [#:positive pos-blame 'use-site]
                 [#:negative neg-blame 'from-macro]
                 [#:name expr-name #f]
                 [#:macro macro-name #f]
                 [#:context ctx #f])
         (attributes c)]{

Accepts an expression (@scheme[expr]) and computes an attribute
@scheme[c] that represents the expression wrapped with the contract
represented by @scheme[contract-expr].

See @secref{exprc} for an example.
}

@section{Contracts for syntax classes}

@defmodule[syntax/parse/experimental/provide]

@defform/subs[#:literals (syntax-class/c)
              (provide-syntax-class/contract
                [syntax-class-id syntax-class-contract] ...)
              ([syntax-class-contract
                (syntax-class/c (mandatory-arg ...))
                (syntax-class/c (mandatory-arg ...)
                                (optional-arg ...))]
               [arg contract-expr (code:line keyword contract-expr)])
              #:contracts ([contract-expr contract?])]{

Provides the syntax class (or splicing syntax class)
@scheme[syntax-class-id] with the given contracts imposed on its
formal parameters.
}

@defidform[syntax-class/c]{

Keyword recognized by @scheme[provide-syntax-class/contract].
}

@section{Reflection}

@defmodule[syntax/parse/experimental/reflect]

A syntax class can be reified into a run-time value, and a reified
syntax class can be used in a pattern via the @scheme[~reflect] and
@scheme[~splicing-reflect] pattern forms.

@defform[(reify-syntax-class syntax-class-id)]{

Reifies the syntax class named @scheme[syntax-class-id] as a run-time
value. The same form also handles splicing syntax classes. Syntax
classes with the @scheme[#:no-delimit-cut] option cannot be reified.
}

@deftogether[(
@defproc[(reified-syntax-class? [x any/c]) boolean?]
@defproc[(reified-splicing-syntax-class? [x any/c]) boolean?])]{

Returns @scheme[#t] if @scheme[x] is a reified (normal) syntax class
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
respectively. Compare with @scheme[procedure-arity] and
@scheme[procedure-keywords].
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

@schemegrammar*[#:literals (~reflect ~splicing-reflect)
                [S-pattern ....
                           (~reflect var-id (reified-expr arg-expr ...) maybe-attrs)]
                [H-pattern ....
                           (~splicing-reflect var-id (reified-expr arg-expr ...)
                                              maybe-attrs)]]

@specsubform/subs[(@#,(defhere ~reflect) var-id (reified-expr arg-expr ...) maybe-attrs)
                  ([maybe-attrs (code:line)
                                (code:line #:attributes (attr-arity-decl ...))])]{

Like @scheme[~var], except that the syntax class position is an
expression evaluating to a reified syntax object, not a syntax class
name, and the attributes bound by the reified syntax class (if any)
must be specified explicitly.
}

@specsubform[(@#,(defhere ~splicing-reflect) var-id (reified-expr arg-expr ...) maybe-attrs)]{

Like @scheme[~reflect] but for reified splicing syntax classes.
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

@section{Procedural splicing syntax classes}

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
returns a list of @scheme[N]+1 elements, where @scheme[N] is the
number of attributes declared by the splicing syntax class. The first
element is the size of the prefix consumed. The rest of the list
contains the values of the attributes.

To indicate failure, the parser calls the failure procedure with an
optional message argument.
}

@;{--------}

@section{Ellipsis-head alternative sets}

@defmodule[syntax/parse/experimental/eh]

Unlike @tech{@Spatterns} and @tech{@Hpatterns}, @tech{@EHpatterns}
cannot be encapsulated by syntax classes, since they describe not only
sets of terms but also repetition constraints.

This module provides @deftech{ellipsis-head alternative sets},
reusable encapsulations of @|EHpatterns|.

@defform/subs[#:literals (pattern)
              (define-eh-alternative-set name eh-alternative ...)
              ([alternative (pattern EH-pattern)])]{

Defines @scheme[name] as an ellipsis-head alternative set. Using
@scheme[name] (via @scheme[~eh-var]) in an ellipsis-head pattern is
equivalent to including each of the alternatives in the pattern via
@ref[~or eh], except that the attributes bound by the alternatives are
prefixed with the name given to @scheme[~eh-var].

Unlike syntax classes, ellipsis-head alternative sets must be defined
before they are referenced.
}

@schemegrammar*[#:literals (~eh-var)
                [EH-pattern ....
                            (~eh-var name eh-alternative-set-id)]]

@specsubform[(@#,(defhere ~eh-var) name eh-alternative-set-id)]{

Includes the alternatives of @scheme[eh-alternative-set-id], prefixing
their attributes with @scheme[name].
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
