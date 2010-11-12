#lang scribble/doc
@(require "mz.ss")

@title[#:tag "modprotect"]{Code Inspectors}

In the same way that inspectors control access to structure fields
(see @secref["inspectors"]), inspectors also control access to
@tech{module bindings}. The default inspector for @tech{module
bindings} is determined by the @scheme[current-code-inspector]
parameter, instead of the @scheme[current-inspector] parameter.

When a @scheme[module] declaration is evaluated, the value of the
@scheme[current-code-inspector] parameter is associated with the
module declaration. When the module is invoked via @scheme[require] or
@scheme[dynamic-require], a sub-inspector of the module's
declaration-time inspector is created, and this sub-inspector is
associated with the module invocation. Any inspector that controls the
sub-inspector (i.e., the declaration-time inspector and its superior)
controls the module invocation.

Control over a module invocation enables

@itemize[

 @item{the use of @scheme[module->namespace] on the module;}

 @item{access to the module's protected identifiers, i.e. those
 identifiers exported from the module with @scheme[protect]; and}

 @item{access to the module's protected and unexported variables
 within compiled code from @scheme[read] (see @scheme[current-compile]).}

]

If the value of @scheme[current-code-inspector] never changes, then no
control is lost for any module invocation, since the module's
invocation is associated with a sub-inspector of
@scheme[current-code-inspector].

The inspector for a module invocation is specific to a particular
module registry, in case a module is attached to a new registry via
@scheme[namespace-attach-module]. The invocation inspector in a
particular registry can be changed via
@scheme[namespace-unprotect-module] (but changing the inspector
requires control over the old one).

Control over a module declaration (as opposed to a mere invocation)
enables the reconstruction of syntax objects that contain references
to the module's protected and unexported identifiers. Otherwise, the
compiler and macro expander prevent any reference to a protected or
unexported identifier,
unless the reference appears within an expression that was generated
by the module's macros (or, more precisely, a macro from a module
whose declaration inspector controls the invocation of the
identifier's module). See @secref["stxcerts"] for further
information.

@defparam[current-code-inspector insp inspector?]{

A parameter that determines an inspector to control access to module
bindings and redefinitions.}
