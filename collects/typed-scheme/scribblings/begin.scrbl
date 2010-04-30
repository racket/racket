#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 typed/scheme)) scribble/eval
		"utils.rkt" (only-in "quick.scrbl" typed-mod))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/scheme))

@title[#:tag "beginning"]{Beginning Typed Scheme}

Recall the typed module from @secref["quick"]:                 

@|typed-mod|

Let us consider each element of this program in turn.

@schememod[typed/scheme]

This specifies that the module is written in the
@schememodname[typed/scheme] language, which is a typed version of the
@schememodname[scheme] language.  Typed versions of other languages
are provided as well; for example, the
@schememodname[typed/scheme/base] language corresponds to
@schememodname[scheme/base].   

@schemeblock[(define-struct: pt ([x : Real] [y : Real]))]

@margin-note{Many forms in Typed Scheme have the same name as the
untyped forms, with a @scheme[:] suffix.}
This defines a new structure, name @scheme[pt], with two fields,
@scheme[x] and @scheme[y].  Both fields are specified to have the type
@scheme[Real], which corresponds to the @rtech{real numbers}.
 The
@scheme[define-struct:] form corresponds to the @scheme[define-struct]
form from @schememodname[scheme]---when porting a program from
@schememodname[scheme] to @schememodname[typed/scheme], uses of
@scheme[define-struct] should be changed to @scheme[define-struct:].

@schemeblock[(: mag (pt -> Number))]

This declares that @scheme[mag] has the type @scheme[(pt -> Number)].
@;{@scheme[mag] must be defined at the top-level of the module containing
the declaration.}

The type @scheme[(pt -> Number)] is a function type, that is, the type
of a procedure.  The input type, or domain, is a single argument of
type @scheme[pt], which refers to an instance of the @scheme[pt]
structure.  The @scheme[->] both indicates that this is a function
type and separates the domain from  the range, or output type, in this
case @scheme[Number].

@schemeblock[
(define (mag p)
  (sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
]

This definition is unchanged from the untyped version of the code.
The goal of Typed Scheme is to  allow almost all definitions to be
typechecked without change.  The typechecker verifies that the body of
the function has the type @scheme[Real], under the assumption that
@scheme[p] has the type @scheme[pt], taking these types from the
earlier type declaration.  Since the body does have this type, the
program is accepted.


@section{Datatypes and Unions}

Many data structures involve multiple variants.  In Typed Scheme, we
represent these using @italic{union types}, written @scheme[(U t1 t2 ...)].

@schememod[
typed/scheme
(define-type Tree (U leaf node))
(define-struct: leaf ([val : Number]))
(define-struct: node ([left : Tree] [right : Tree]))

(: tree-height (Tree -> Number))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))

(: tree-sum (Tree -> Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))
]

In this module, we have defined two new datatypes: @scheme[leaf] and
@scheme[node].  We've also defined the type name @scheme[Tree] to be
@scheme[(U node leaf)], which represents a binary tree of numbers.  In
essence, we are saying that the @scheme[tree-height] function accepts
a @scheme[Tree], which is either a @scheme[node] or a @scheme[leaf],
and produces a number.

In order to calculate interesting facts about trees, we have to take
them apart and get at their contents.  But since accessors such as
@scheme[node-left] require a @scheme[node] as input, not a
@scheme[Tree], we have to determine which kind of input we
were passed.  

For this purpose, we use the predicates that come with each defined
structure.  For example, the @scheme[leaf?] predicate distinguishes
@scheme[leaf]s from all other Typed Scheme values.  Therefore, in the
first branch of the @scheme[cond] clause in @scheme[tree-sum], we know
that @scheme[t] is a @scheme[leaf], and therefore we can get its value
with the @scheme[leaf-val] function.

In the else clauses of both functions, we know that @scheme[t] is not
a @scheme[leaf], and since the type of @scheme[t] was @scheme[Tree] by
process of elimination we can determine that @scheme[t] must be a
@scheme[node].  Therefore, we can use accessors such as
@scheme[node-left] and @scheme[node-right] with @scheme[t] as input.


@section{Type Errors}

When Typed Scheme detects a type error in the module, it raises an
error before running the program.  

@examples[#:eval the-eval
(add1 "not a number")
]

@;{
Typed Scheme also attempts to detect more than one error in the module.

@examples[#:eval the-eval
(string-append "a string" (add1 "not a number"))
]
}