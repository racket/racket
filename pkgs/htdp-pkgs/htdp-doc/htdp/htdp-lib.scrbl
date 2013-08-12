#lang scribble/doc

@(require scribble/manual scribble/eval
          (for-label lang/prim lang/imageeq lang/posn racket/gui/base
                     (only-in racket/contract any/c)
                     (only-in racket/class is-a?/c)))
@(define (htdp-ref s) @secref[#:doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl") s])

@title{HtDP Languages as Libraries}

@; ------------------------------------------------------------
@section{@italic{HtDP} Beginning Student}

@defmodule[lang/htdp-beginner]

The @racketmodname[lang/htdp-beginner] module provides the Beginning
Student Language; see @htdp-ref["beginner"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Beginning Student with Abbreviations}

@defmodule[lang/htdp-beginner-abbr]

The @racketmodname[lang/htdp-beginner-abbr] module provides the
Beginning Student with Abbreviations language; see
@htdp-ref["beginner-abbr"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Intermediate Student}

@defmodule[lang/htdp-intermediate]

The @racketmodname[lang/htdp-intermediate] module provides the
Intermediate Student language; see
@htdp-ref["intermediate"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Intermediate Student with Lambda}

@defmodule[lang/htdp-intermediate-lambda]

The @racketmodname[lang/htdp-intermediate-lambda] module provides the
Intermediate Student with Lambda language; see
@htdp-ref["intermediate-lam"].

@; ------------------------------------------------------------
@section{@italic{HtDP} Advanced Student}

@defmodule[lang/htdp-advanced]

The @racketmodname[lang/htdp-advanced] module provides the Advanced
Student language; see @htdp-ref["advanced"].

@; ------------------------------------------------------------
@section{Pretty Big Text (Legacy Language)}

@defmodule[lang/plt-pretty-big-text]

The @racketmodname[lang/plt-pretty-big-text] module is similar to the
@italic{HtDP} Advanced Student language, but with more of Racket's
libraries in legacy form. It provides the bindings of 
@racketmodname[mzscheme],
@racketmodname[mzlib/etc], @racketmodname[mzlib/file],
@racketmodname[mzlib/list], @racketmodname[mzlib/class],
@racketmodname[mzlib/unit], @racketmodname[mzlib/include],
@racketmodname[mzlib/defmacro], @racketmodname[mzlib/pretty],
@racketmodname[mzlib/string], @racketmodname[mzlib/thread],
@racketmodname[mzlib/math], @racketmodname[mzlib/match],
@racketmodname[mzlib/shared], and @racketmodname[lang/posn].

@; ------------------------------------------------------------

@section{Pretty Big (Legacy Language)}

@defmodule[lang/plt-pretty-big]

The @racketmodname[lang/plt-pretty-big] module extends
@racket[lang/plt-pretty-big-text] with @racketmodname[racket/gui/base]
and @racketmodname[lang/imageeq].  This language corresponds to the
@onscreen{Pretty Big} legacy language in DrRacket.

@; ----------------------------------------------------------------------

@section{@racket[posn]s in @italic{HtDP} Languages}

@defmodule[lang/posn]

@defstruct[posn ([x any/c] [y any/c])]{

The @racket[posn] structure type that is also provided by
@racket[lang/htdp-beginner].}


@; ----------------------------------------------------------------------

@section{Image Equality in @italic{HtDP} Languages}

@defmodule[lang/imageeq]

@defproc[(image=? [i1 (is-a?/c image-snip%)]
                  [i2 (is-a?/c image-snip%)])
         boolean?]{

The image-comparison operator that is also provided by
@racket[lang/htdp-beginner].}

@; ----------------------------------------------------------------------

@section{Primitives in @italic{HtDP} Beginner}

@defmodule[lang/prim]

The @racketmodname[lang/prim] module several syntactic forms for
use by the implementors of teachpacks, when the teachpack is to be
used with the Beginner Student
Language. In Beginner Student, primitive names (for built-in
procedures) are distinguished from other types of expressions, so that
they can be syntactically restricted to application positions.

@defform[(define-primitive id proc-id)]{

  Defines @racket[id] to be a primitive operator whose implementation
  is @racket[proc-id], and that takes no procedures as
  arguments. Normally, @racket[id] is exported from the teachpack and
  @racket[proc-id] is not.}

@defform[(provide-primitive id)]{

  Like @racket[define-primitive], but the existing function @racket[id] is
  exported as the primitive operator named @racket[id]. An alternative
  to @racket[define-primitive].}

@defform[(provide-primitives id ...)]{

  Multiple-identifier version of @racket[provide-primitive].}

@defform[(define-higher-order-primitive id proc-id (arg ...))]{

  Defines @racket[id] to be a primitive operator whose implementation is
  @racket[proc-id]. Normally, @racket[id] is exported from the teachpack and
  @racket[proc-id] is not.

  For each non-procedure argument, the corresponding @racket[arg] should be
  an underscore. For each procedure argument, the corresponding @racket[arg]
  should be the usual name of the procedure.

  @as-examples[
  @racketblock[
   (define-higher-order-primitive convert-gui convert-gui/proc (f2c))
  ]]
}

@defform[(provide-higher-order-primitive id (arg ...))]{

  Like @racket[define-higher-order-primitive], but the existing function
  @racket[id] is exported as the primitive operator named
  @racket[id]. An alternative to @racket[define-higher-order-primitive].}

@defform[(first-order->higher-order expression)]{

If @racket[expression] is the name of a first-order function (either a
primitive or a function defined within Beginner Student), produces the
function as a value; otherwise, the form is equivalent to
@racket[expression].

This form is mainly useful for implementing syntactic forms that, like
the application of a higher-order primitive, allow first-order bindings
to be used in an expression position.}
