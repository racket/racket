#lang scribble/doc
@(require scribblings/reference/mz (for-label compatibility/package))

@(define pack-eval (make-base-eval))
@examples[#:hidden #:eval pack-eval (require compatibility/package)]

@title[#:tag "compatibility-package"]{Limiting Scope: @racket[define-package], @racket[open-package], ...}

@defmodule[compatibility/package]

This @racketmodname[compatibility/package] library provides support for
the Chez Scheme module system. Support for packages is provided
primarily to help porting code.

Use of packages for modern Racket code is discouraged.
Instead, consider using
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{submodules}.

@deftogether[(
@defform[(define-package package-id exports form ...)]
@defform/subs[(open-package package-id)
              ([exports (id ...)
                        (code:line #:only (id ...))
                        #:all-defined
                        (code:line #:all-defined-except (id ...))])]
)]{

@margin-note{The @racket[define-package] form is based on the @racketidfont{module}
             form of Chez Scheme @cite["Waddell99"].}

The @racket[define-package] form is similar to @racket[module], except
that it can appear in any definition context. The @racket[form]s
within a @racket[define-package] form can be definitions or
expressions; definitions are not visible outside the
@racket[define-package] form, but @racket[exports] determines a subset
of the bindings that can be made visible outside the package using
the definition form @racket[(open-package package-id)].

The @racket[(id ...)] and @racket[#:only (id ...)] @racket[exports]
forms are equivalent: exactly the listed @racket[id]s are
exported. The @racket[#:all-defined] form exports all definitions from
the package body, and @racket[#:all-defined-except (id ...)] exports
all definitions except the listed @racket[id]s.

All of the usual definition forms work within a
@racket[define-package] body, and such definitions are visible to all
expressions within the body (and, in particular, the definitions can
refer to each other). However, @racket[define-package] handles
@racket[define*], @racket[define*-syntax], @racket[define*-values],
@racket[define*-syntaxes], and
@racket[open*-package] specially: the bindings introduced by those
forms within a @racket[define-package] body are visible only to
@racket[form]s that appear later in the body, and they can shadow any
binding from preceding @racket[form]s (even if the preceding binding
did not use one of the special @racketidfont{*} definition forms).  If
an exported identifier is defined multiple times, the last definition
is the exported one.

@examples[
#:eval pack-eval
(define-package presents (doll)
  (define doll "Molly Coddle")
  (define robot "Destructo"))
(eval:error doll)
(eval:error robot)
(open-package presents)
doll
(eval:error robot)
(define-package big-russian-doll (middle-russian-doll)
  (define-package middle-russian-doll (little-russian-doll)
    (define little-russian-doll "Anastasia")))
(open-package big-russian-doll)
(open-package middle-russian-doll)
little-russian-doll
]}


@defform[(package-begin form ...)]{

Similar to @racket[define-package], but it only limits the visible of
definitions without binding a package name. If the last @racket[form]
is an expression, then the expression is in
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{tail position}
for the @racket[package-begin] form, so that its result is the
@racket[package-begin] result.

A @racket[package-begin] form can be used as an expression, but if it
is used in a context where definitions are allowed, then the
definitions are essentially spliced into the enclosing context (though
the defined bindings remain hidden outside the
@racket[package-begin]).

@examples[
#:eval pack-eval
(package-begin
  (define secret "mimi")
  (list secret))
(eval:error secret)
]}

@deftogether[(
@defidform[define*]
@defidform[define*-values]
@defidform[define*-syntax]
@defidform[define*-syntaxes]
@defidform[open*-package]
)]{

Equivalent to @racket[define], @racket[define-values],
@racket[define-syntax], @racket[define-syntaxes],
and @racket[open-package], except within a
@racket[define-package] or @racket[package-begin] form, where they
create bindings that are visible only to later body forms.

@examples[
#:eval pack-eval
(define-package mail (cookies)
  (define* cookies (list 'sugar))
  (define* cookies (cons 'chocolate-chip cookies)))
(open-package mail)
cookies
(define-syntax-rule (define-seven id) (define id 7))
(define-syntax-rule (define*-seven id)
  (begin
    (define-package p (id) (define-seven id))
    (open*-package p)))
(package-begin
  (define vii 8)
  (define*-seven vii)
  vii)]}

@deftogether[(
@defproc[(package? [v any/c]) boolean?]
@defproc[(package-exported-identifiers [id identifier?]) (listof identifier?)]
@defproc[(package-original-identifiers [id identifier?]) (listof identifier?)]
)]{

The @racket[package?], @racket[package-exported-identifiers], and
@racket[package-original-identifiers] functions are exported
@racket[for-syntax] by @racketmodname[compatibility/package].

The @racket[package?] predicate returns @racket[#t] if @racket[v] is a
package value as obtained by @racket[syntax-local-value] on an
identifier that is bound to a package.

Given such an identifier, the @racket[package-exported-identifiers]
function returns a list of identifiers that correspond to the
bindings that would be introduced by opening the package in the
lexical context being expanded. The
@racket[package-original-identifiers] function returns a parallel list
of identifiers for existing bindings of package's exports.}

@; ----------------------------------------------------------------------

@section[#:style '(hidden)]{Legacy Racket Package Library}

@defmodule[racket/package]{The @racket[racket/package] library
re-exports @racketmodname[compatibility/package] for backward
compatibility.}

@; ----------------------------------------------------------------------

@close-eval[pack-eval]
