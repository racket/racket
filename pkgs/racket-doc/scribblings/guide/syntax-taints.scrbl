#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "stx-certs" #:style 'quiet]{Tainted Syntax}

Modules often contain definitions that are meant only for use within
the same module and not exported with @racket[provide]. Still, a use
of a macro defined in the module can expand into a reference of an
unexported identifier. In general, such an identifier must not be
extracted from the expanded expression and used in a different
context, because using the identifier in a different context may break
invariants of the macro's module.

For example, the following module exports a macro @racket[go] that
expands to a use of @racket[unchecked-go]:

@racketmod[
#:file "m.rkt"
racket
(provide go)

(define (unchecked-go n x) 
  (code:comment @#,t{to avoid disaster, @racket[n] must be a number})
  (+ n 17))

(define-syntax (go stx)
  (syntax-case stx ()
    [(_ x)
     #'(unchecked-go 8 x)]))
]

If the reference to @racket[unchecked-go] is extracted from the
expansion of @racket[(go 'a)], then it might be inserted into a new
expression, @racket[(unchecked-go #f 'a)], leading to disaster. The
@racket[datum->syntax] procedure can be used similarly to construct
references to an unexported identifier, even when no macro expansion
includes a reference to the identifier.

Ultimately, protection of a module's private bindings depends on
changing the current @tech{code inspector} by setting the
@racket[current-code-inspector] parameter. @margin-note*{See also
@secref["code-inspectors+protect"].} That's because a code inspector
controls access to a module's internal state through functions like
@racket[module->namespace]. The current code inspector also gates
access to the @tech{protected} exports of unsafe modules like
@racketmodname[racket/unsafe/ops].

Since the result of macro expansion can be abused to gain access to
protected bindings, macro functions like @racket[local-expand] are
also @tech{protected}: references to @racket[local-expand] and similar
are allowed only within modules that are declared while the original
code inspector is the current code inspector. Functions like
@racket[expand], which are not used to implement macros but are used
to inspect the result of macro expansion, are protected in a different
way: the expansion result is @deftech{tainted} so that it cannot be
compiled or expanded again. More precisely, functions like
@racket[expand] accept an optional inspector argument that determines
whether the result is tainted, but the default value of the argument
is @racket[(current-code-inspector)].

@margin-note{In previous versions of Racket, a macro was responsible
for protecting expansion using @racket[syntax-protect]. The use of
@racket[syntax-protect] is no longer required or recommended.}
