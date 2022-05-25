#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "protect-out"]{Protected Exports}

Sometimes, a module needs to export bindings to other modules that are
at the same trust level as the exporting module, while at the same
time preventing access from untrusted modules. Such exports should use
the @racket[protect-out] form in @racket[provide]. For example,
@racketmodname[ffi/unsafe] exports all of its unsafe bindings as
@deftech{protected} in this sense.

Levels of trust are implemented with @tech{code inspectors} (see
@secref["code-inspectors+protect"]).
Only modules loaded with an equally strong code inspector as an
exporting module can use protected bindings from the exporting module.
Operations like @racket[dynamic-require] are granted access depending
on the current code inspector as determined by
@racket[current-code-inspector].

When a module re-exports a protected binding, it does not need to use
@racket[protect-out] again. Access is always determined by the code
inspector of the module that originally defines a protected binding.
When using a protected binding within a module, take care to either
provide new bindings from the module with @racket[protect-out] or
ensure that no provided bindings expose functionality that was meant
to be protected in the first place.
