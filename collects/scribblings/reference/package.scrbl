#lang scribble/doc
@(require "mz.ss"
          (for-label racket/package))

@(define pack-eval (make-base-eval))
@interaction-eval[#:eval pack-eval (require racket/package)]

@title[#:tag "package"]{Limiting Scope: @scheme[define-package], @scheme[open-package], ...}

@note-lib-only[racket/package]

@deftogether[(
@defform[(define-package package-id exports form ...)]
@defform/subs[(open-package package-id)
              ([exports (id ...)
                        (code:line #:only (id ...))
                        #:all-defined
                        (code:line #:all-defined-except (id ...))])]
)]{

@margin-note{The @scheme[define-package] form is based on the @schemeidfont{module}
             form of Chez Scheme @cite["Waddell99"].}

The @scheme[define-package] form is similar to @scheme[module], except
that it can appear in any definition context. The @scheme[form]s
within a @scheme[define-package] form can be definitions or
expressions; definitions are not visible outside the
@scheme[define-package] form, but @scheme[exports] determines a subset
of the bindings that can be made visible outside the package using
the definition form @scheme[(open-package package-id)].

The @scheme[(id ...)] and @scheme[#:only (id ...)] @scheme[exports]
forms are equivalent: exactly the listed @scheme[id]s are
exported. The @scheme[#:all-defined] form exports all definitions from
the package body, and @scheme[#:all-defined-except (id ...)] exports
all definitions except the listed @scheme[id]s.

All of the usual definition forms work within a
@scheme[define-package] body, and such definitions are visible to all
expressions within the body (and, in particular, the definitions can
refer to each other). However, @scheme[define-package] handles
@scheme[define*], @scheme[define*-syntax], @scheme[define*-values],
@scheme[define*-syntaxes], and
@scheme[open*-package] specially: the bindings introduced by those
forms within a @scheme[define-package] body are visible only to
@scheme[form]s that appear later in the body, and they can shadow any
binding from preceding @scheme[form]s (even if the preceding binding
did not use one of the special @schemeidfont{*} definition forms).  If
an exported identifier is defined multiple times, the last definition
is the exported one.

@examples[
#:eval pack-eval
(define-package presents (doll)
  (define doll "Molly Coddle")
  (define robot "Destructo"))
doll
robot
(open-package presents)
doll
robot
(define-package big-russian-doll (middle-russian-doll)
  (define-package middle-russian-doll (little-russian-doll)
    (define little-russian-doll "Anastasia")))
(open-package big-russian-doll)
(open-package middle-russian-doll)
little-russian-doll
]}


@defform[(package-begin form ...)]{

Similar to @scheme[define-package], but it only limits the visible of
definitions without binding a package name. If the last @scheme[form]
is an expression, then the expression is in @tech{tail position} for
the @scheme[package-begin] form, so that its result is the
@scheme[package-begin] result.

A @scheme[package-begin] form can be used as an expression, but if it
is used in a context where definitions are allowed, then the
definitions are essentially spliced into the enclosing context (though
the defined bindings remain hidden outside the
@scheme[package-begin]).

@examples[
#:eval pack-eval
(package-begin
  (define secret "mimi")
  (list secret))
secret
]}

@deftogether[(
@defidform[define*]
@defidform[define*-values]
@defidform[define*-syntax]
@defidform[define*-syntaxes]
@defidform[open*-package]
)]{

Equivalent to @scheme[define], @scheme[define-values],
@scheme[define-syntax], @scheme[define-syntaxes],
and @scheme[open-package], except within a
@scheme[define-package] or @scheme[package-begin] form, where they
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

The @scheme[package?], @scheme[package-exported-identifiers], and
@scheme[package-original-identifiers] functions are exported
@scheme[for-syntax] by @schememodname[racket/package].

The @scheme[package?] predicate returns @scheme[#t] if @scheme[v] is a
package value as obtained by @scheme[syntax-local-value] on an
identifier that is bound to a package.

Given such an identifier, the @scheme[package-exported-identifiers]
function returns a list of identifiers that corresponding to the
bindings that would be introduced by opening the package in the
lexical context being expanded. The
@scheme[package-original-identifiers] function returns a parallel list
of identifiers for existing bindings of package's exports.}

@; ----------------------------------------------------------------------

@close-eval[pack-eval]
