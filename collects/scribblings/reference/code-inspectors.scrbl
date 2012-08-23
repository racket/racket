#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "modprotect"]{Code Inspectors}

In the same way that inspectors control access to structure fields
(see @secref["inspectors"]), inspectors also control access to
@tech{module bindings}. The default inspector for @tech{module
bindings} is determined by the @racket[current-code-inspector]
parameter, instead of the @racket[current-inspector] parameter.

When a @racket[module] declaration is evaluated, the value of the
@racket[current-code-inspector] parameter is associated with the
module declaration. When the module is invoked via @racket[require] or
@racket[dynamic-require], a sub-inspector of the module's
declaration-time inspector is created, and this sub-inspector is
associated with the module invocation. Any inspector that controls the
sub-inspector (including the declaration-time inspector and its
superior) controls the module invocation. In particular, if the value
of @racket[current-code-inspector] never changes, then no control is
lost for any module invocation, since the module's invocation is
associated with a sub-inspector of @racket[current-code-inspector].

When an inspector that controls a module invocation is installed
@racket[current-code-inspector], it enables the following
@racket[module->namespace] on the module, and it enables access to the
module's protected exports (i.e., those identifiers exported from the
module with @racket[protect-out]) via @racket[dynamic-require].

When a @racket[module] form is expanded or a @tech{namespace} is
created, the value of @racket[current-code-inspector] is associated
with the module or namespace's top-level @tech{lexical information}.
Syntax objects with that @tech{lexical information} gain access to the
protected and unexported bindings of any module that the inspector
controls. In the case of a @racket[module], the inspector sticks with
such syntax objects even the syntax object is used in the expansion of
code in a less powerful context; furthermore, if the syntax object is
an identifier that is compiled as a variable reference, the inspector
sticks with the variable reference even if it appears in a module form
that is evaluated (i.e., declared) with a weaker inspector. When a
syntax object or variable reference is within compiled code that is
printed (see @secref["print-compiled"]), the associated inspector is
not preserved.

When compiled code in printed form is @racket[read] back in, no
inspectors are associated with the code. When the code is
@racket[eval]uated, the instantiated syntax-object literals and
module-variable references acquire value of
@racket[current-code-inspector] as their inspector.

When a module instantiation is attached to multiple @tech{namespaces},
each with its own @tech{module registry}, the inspector for the module
invocation can be registry-specific. The invocation inspector in a
particular module registry can be changed via
@racket[namespace-unprotect-module] (but changing the inspector
requires control over the old one).

@defparam[current-code-inspector insp inspector?]{

A @tech{parameter} that determines an inspector to control access to module
bindings and redefinitions.}
