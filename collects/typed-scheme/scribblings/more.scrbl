#lang scribble/manual

@begin[(require "utils.ss"
		scribble/core scribble/eval
		(for-label typed/scheme mzlib/etc))]

@title[#:tag "more"]{More Features}

@(define the-eval (make-base-eval))
@(the-eval '(require typed/scheme))


The previous section introduced the basics of the Typed Scheme type
system.  In this section, we will see several new features of the
language and the type system.  The next
subsequent section will explain these features in more detail.

@section{Type Annotation and Binding Forms}

In general, variables in Typed Scheme must be annotated with their
type.  

@subsection{Annotating Definitions}

We have already seen the @scheme[:] type annotation form.  This is
useful for definitions, at both the top level of a module

@schemeblock[
(: x Number)
(define x 7)]

and in an internal definition

@schemeblock[
(let ()
  (: x Number)
  (define x 7)
  (add1 x))
]

In addition to the @scheme[:] form, almost all binding forms from
@schememodname[scheme] have counterparts which allow the specification
of types. The @scheme[define:] form allows the definition of variables
in both top-level and internal contexts.

@schemeblock[
(define: x : Number 7)
(define: (id [z : Number]) z)]

Here, @scheme[x] has the type @scheme[Number], and @scheme[id] has the
type @scheme[(Number -> Number)].  In the body of @scheme[id],
@scheme[z] has the type @scheme[Number]. 

@subsection{Annotating Local Binding}

@schemeblock[
(let: ([x : Number 7])
  (add1 x))
]

The @scheme[let:] form is exactly like @scheme[let], but type
annotations are provided for each variable bound.  Here, @scheme[x] is
given the type @scheme[Number].  The @scheme[let*:] and
@scheme[letrec:] are similar.

@schemeblock[
(let-values: ([([x : Number] [y : String]) (values 7 "hello")])
  (+ x (string-length y)))
]

The @scheme[let*-values:] and @scheme[letrec-values:] forms are similar.

@subsection{Annotating Functions}

Function expressions also bind variables, which can be annotated with
types. This function expects two arguments, a @scheme[Number] and a
@scheme[String]: 

@schemeblock[(lambda: ([x : Number] [y : String]) (+ x 5))]

This function accepts at least one @scheme[String], followed by
arbitrarily many @scheme[Number]s.  In the body, @scheme[y] is a list
of @scheme[Number]s.

@schemeblock[(lambda: ([x : String] (unsyntax @tt["."]) [y : Number #,**]) (apply + y))]

This function has the type @scheme[(String Number #,** -> Number)].
Functions defined by cases may also be annotated:

@schemeblock[(case-lambda: [() 0]
			   [([x : Number]) x])]

This function has the type 
@scheme[(case-lambda (-> Number) (Number -> Number))].   

@subsection{Annotating Single Variables}

When a single variable binding needs annotation, the annotation can be
applied to a single variable using a reader extension:

@schemeblock[
(let ([#,(annvar x Number) 7]) (add1 x))]

This is equivalent to the earlier use of @scheme[let:]. This is
especially useful for binding forms which do not have counterparts
provided by Typed Scheme, such as @scheme[let+]:

@schemeblock[
(let+ ([val #,(annvar x Number) (+ 6 1)]) 
  (* x x))]

@subsection{Annotating Expressions}

It is also possible to provide an expected type for a particular
expression.   

@schemeblock[(ann (+ 7 1) Number)]

This ensures that the expression, here @scheme[(+ 7 1)], has the
desired type, here @scheme[Number].  Otherwise, the type checker
signals an error.  For example:

@interaction[#:eval the-eval
(ann "not a number" Number)]

@section{Type Inference}

@section{Subtyping}

@section{Occurrence Typing}

@section{Recursive Types}

@section{Polymorphism}

@include-section["varargs.scrbl"]

@section{Refinement Types}
