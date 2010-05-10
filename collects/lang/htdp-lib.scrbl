#lang scribble/doc
@(require scribble/manual
	  scribble/eval
          (for-label scheme/base
                     scheme/contract
                     scheme/class
                     scheme/gui/base
                     lang/posn
                     lang/imageeq
                     lang/prim))

@(define htdp @italic{How to Design Programs})
@(define (htdp-ref s) @secref[#:doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl") s])

@title{@bold{HtDP}: Languages as Libraries}

@; ------------------------------------------------------------
@section{@italic{HtDP} Beginning Student}

@defmodule[lang/htdp-beginner]

The @schememodname[lang/htdp-beginner] module provides the Beginning
Student language for @|htdp|; see @htdp-ref["beginner"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Beginning Student with Abbreviations}

@defmodule[lang/htdp-beginner-abbr]

The @schememodname[lang/htdp-beginner-abbr] module provides the
Beginning Student with Abbreviations language for @|htdp|; see
@htdp-ref["beginner-abbr"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Intermediate Student}

@defmodule[lang/htdp-intermediate]

The @schememodname[lang/htdp-intermediate] module provides the
Intermediate Student language for @|htdp|; see
@htdp-ref["intermediate"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Intermediate Student with Lambda}

@defmodule[lang/htdp-intermediate-lambda]

The @schememodname[lang/htdp-intermediate-lambda] module provides the
Intermediate Student with Lambda language for @|htdp|; see
@htdp-ref["intermediate-lam"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Advanced Student}

@defmodule[lang/htdp-advanced]

The @schememodname[lang/htdp-advanced] module provides the Advanced
Student language for @|htdp|; see @htdp-ref["advanced"].

@; ------------------------------------------------------------
@section{Pretty Big Text (Legacy Language)}

@defmodule[lang/plt-pretty-big-text]

The @schememodname[lang/plt-pretty-big-text] module is similar to the
@italic{HtDP} Advanced Student language, but with more of Racket's
libraries in legacy form. It provides the bindings of 
@schememodname[mzscheme],
@schememodname[mzlib/etc], @schememodname[mzlib/file],
@schememodname[mzlib/list], @schememodname[mzlib/class],
@schememodname[mzlib/unit], @schememodname[mzlib/include],
@schememodname[mzlib/defmacro], @schememodname[mzlib/pretty],
@schememodname[mzlib/string], @schememodname[mzlib/thread],
@schememodname[mzlib/math], @schememodname[mzlib/match],
@schememodname[mzlib/shared], and @schememodname[lang/posn].

@; ------------------------------------------------------------

@section{Pretty Big (Legacy Language)}

@defmodule[lang/plt-pretty-big]

The @schememodname[lang/plt-pretty-big] module extends
@scheme[lang/plt-pretty-big-text] with @schememodname[scheme/gui/base]
and @schememodname[lang/imageeq].  This language corresponds to the
@onscreen{Pretty Big} legacy language in DrRacket.

@; ----------------------------------------------------------------------

@section{@scheme[posn]s in @italic{HtDP} Languages}

@defmodule[lang/posn]

@defstruct[posn ([x any/c] [y any/c])]{

The @scheme[posn] structure type that is also provided by
@scheme[lang/htdp-beginner].}


@; ----------------------------------------------------------------------

@section{Image Equality in @italic{HtDP} Languages}

@defmodule[lang/imageeq]

@defproc[(image=? [i1 (is-a?/c image-snip%)]
                  [i2 (is-a?/c image-snip%)])
         boolean?]{

The image-comparison operator that is also provided by
@scheme[lang/htdp-beginner].}

@; ----------------------------------------------------------------------

@section{Primitives in @italic{HtDP} Beginner}

@defmodule[lang/prim]

The @schememodname[lang/prim] module several syntactic forms for
use by the implementors of teachpacks, when the teachpack is to be
used with the @|htdp| Beginner Student
languages. In Beginner Student, primitive names (for built-in
procedures) are distinguished from other types of expressions, so that
they can be syntactically restricted to application positions.

@defform[(define-primitive id proc-id)]{

  Defines @scheme[id] to be a primitive operator whose implementation
  is @scheme[proc-id], and that takes no procedures as
  arguments. Normally, @scheme[id] is exported from the teachpack and
  @scheme[proc-id] is not.}

@defform[(provide-primitive id)]{

  Like @scheme[define-primitive], but the existing function @scheme[id] is
  exported as the primitive operator named @scheme[id]. An alternative
  to @scheme[define-primitive].}

@defform[(provide-primitives id ...)]{

  Multiple-identifier version of @scheme[provide-primitive].}

@defform[(define-higher-order-primitive id proc-id (arg ...))]{

  Defines @scheme[id] to be a primitive operator whose implementation is
  @scheme[proc-id]. Normally, @scheme[id] is exported from the teachpack and
  @scheme[proc-id] is not.

  For each non-procedure argument, the corresponding @scheme[arg] should be
  an underscore. For each procedure argument, the corresponding @scheme[arg]
  should be the usual name of the procedure.

  @as-examples[
  @schemeblock[
   (define-higher-order-primitive convert-gui convert-gui/proc (f2c))
  ]]
}

@defform[(provide-higher-order-primitive id (arg ...))]{

  Like @scheme[define-higher-order-primitive], but the existing function
  @scheme[id] is exported as the primitive operator named
  @scheme[id]. An alternative to @scheme[define-higher-order-primitive].}

@defform[(first-order->higher-order expr)]{

If @scheme[expr] is an identifier for a first-order function (either a
primitive or a function defined within Beginner Student), produces the
function as a value; otherwise, the form is equivalent to
@scheme[expr].

This form is mainly useful for implementing syntactic forms that, like
the application of a higher-order primitive, allow first-order bindings
to be used in an expression position.}
