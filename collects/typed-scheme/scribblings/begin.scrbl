#lang scribble/manual

@begin[(require (for-label typed/scheme) scribble/eval
		"utils.ss" (only-in "quick.scrbl" typed-mod))]

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

@schemeblock[(: mag (pt -> Real))]

This declares that @scheme[mag] has the type @scheme[(pt -> Real)].
@;{@scheme[mag] must be defined at the top-level of the module containing
the declaration.}

The type @scheme[(pt -> Real)] is a function type, that is, the type
of a procedure.  The input type, or domain, is a single argument of
type @scheme[pt], which refers to an instance of the @scheme[pt]
structure.  The @scheme[->] both indicates that this is a function
type and separates the domain from  the range, or output type, in this
case @scheme[Real].

@schemeblock[
(define (mag p)
  (sqrt (sqr (pt-x p)) (sqr (pt-y p))))
]

This definition is unchanged from the untyped version of the code.
The goal of Typed Scheme is to  allow almost all definitions to be
typechecked without change.  The typechecker verifies that the body of
the function has the type @scheme[Real], under the assumption that
@scheme[p] has the type @scheme[pt], taking these types from the
earlier type declaration.  Since the body does have this type, the
program is accepted.


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