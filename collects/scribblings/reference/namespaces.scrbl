#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "Namespaces"]{Namespaces}

See @secref["namespace-model"] for basic information on the
@tech{namespace} model.

A new @tech{namespace} is created with procedures like
@racket[make-empty-namespace], and @racket[make-base-namespace], which
return a first-class namespace value. A namespace is used by setting
the @racket[current-namespace] parameter value, or by providing the
namespace to procedures such as @racket[eval] and
@racket[eval-syntax].

@defproc[(namespace? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a namespace value, @racket[#f]
otherwise.}


@defproc[(make-empty-namespace) namespace?]{

Creates a new namespace that is empty, and whose @tech{module
registry} contains no mappings. The namespace's @tech{base phase} is
the same as the @tech{base phase} of the @tech{current
namespace}. Attach modules from an existing namespace to the new one
with @racket[namespace-attach-module].}


@defproc[(make-base-empty-namespace) namespace?]{

Creates a new empty namespace, but with @racketmodname[racket/base]
attached. The namespace's @tech{base phase} is the same as the
@tech{phase} in which the @racket[make-base-empty-namespace]
function was created.}


@defproc[(make-base-namespace) namespace?]{

Creates a new namespace with @racketmodname[racket/base] attached and
@racket[require]d into the top-level environment. The namespace's
@tech{base phase} is the same as the @tech{phase} in which the
@racket[make-base-namespace] function was created.}


@defform[(define-namespace-anchor id)]{

Binds @racket[id] to a namespace anchor that can be used with
@racket[namespace-anchor->empty-namespace] and
@racket[namespace-anchor->namespace].

This form can be used only in a @tech{top-level context} or in a
@tech{module-context}.}


@defproc[(namespace-anchor? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a namespace-anchor value,
@racket[#f] otherwise.}


@defproc[(namespace-anchor->empty-namespace [a namespace-anchor?]) namespace?]{

Returns an empty namespace that shares a @tech{module registry} with
the source of the anchor, and whose @tech{base phase} the
@tech{phase} in which the anchor was created.

If the anchor is from a @racket[define-namespace-anchor] form in a
module context, then the source is the namespace in which the
containing module is instantiated. If the anchor is from a
@racket[define-namespace-anchor] form in a top-level content, then the
source is the namespace in which the anchor definition was evaluated.}


@defproc[(namespace-anchor->namespace [a namespace-anchor?]) namespace?]{

Returns a namespace corresponding to the source of the anchor.

If the anchor is from a @racket[define-namespace-anchor] form in a
module context, then the result is a namespace for the module's body
in the anchor's phase. The result is the same as a namespace obtained
via @racket[module->namespace].

If the anchor is from a @racket[define-namespace-anchor] form in a
top-level content, then the result is the namespace in which the
anchor definition was evaluated.}


@defparam[current-namespace n namespace?]{

A @tech{parameter} that determines the @techlink{current namespace}.}


@defproc[(namespace-symbol->identifier [sym symbol?]) identifier?]{

Similar to @racket[datum->syntax] restricted to symbols. The
@tech{lexical information} of the resulting identifier corresponds to
the top-level environment of the current namespace; the identifier has
no source location or properties.}


@defproc[(namespace-base-phase [namespace namespace? (current-namespace)]) exact-integer?]{

Returns the @tech{base phase} of @racket[namespace].}


@defproc[(namespace-module-identifier [where (or/c namespace? exact-integer? #f)
                                             (current-namespace)])
         identifier?]{

Returns an identifier whose binding is @racket[module] in the
@tech{base phase} of @racket[where] if it is a namespace, or in the
@racket[where] @tech{phase level} otherwise.

The @tech{lexical information} of the identifier includes bindings (in
the same @tech{phase level}) for all syntactic forms that appear in
fully expanded code (see @secref["fully-expanded"]), but using the
name reported by the second element of @racket[identifier-binding] for
the binding; the @tech{lexical information} may also include other
bindings.}


@defproc[(namespace-variable-value [sym symbol?]
                                   [use-mapping? any/c #t]
                                   [failure-thunk (or/c (-> any) #f) #f]
                                   [namespace namespace? (current-namespace)])
         any]{

Returns a value for @racket[sym] in @racket[namespace], using
@racket[namespace]'s @tech{base phase}. The returned value depends on
@racket[use-mapping?]:

 @itemize[

   @item{If @racket[use-mapping?] is true (the default), and if
   @racket[sym] maps to a top-level variable or an imported variable
   (see @secref["namespace-model"]), then the result is the same as
   evaluating @racket[sym] as an expression. If @racket[sym] maps to
   syntax or imported syntax, then @racket[failure-thunk] is called or
   the @exnraise[exn:fail:syntax]. If @racket[sym] is mapped to an
   undefined variable or an uninitialized module variable, then
   @racket[failure-thunk] is called of the
   @exnraise[exn:fail:contract:variable].}

   @item{If @racket[use-mapping?] is @racket[#f], the namespace's
   syntax and import mappings are ignored. Instead, the value of the
   top-level variable named @racket[sym] in namespace is returned. If
   the variable is undefined, then @racket[failure-thunk] is called or
   the @exnraise[exn:fail:contract:variable].}

 ]

If @racket[failure-thunk] is not @racket[#f],
@racket[namespace-variable-value] calls @racket[failure-thunk] to
produce the return value in place of raising an
@racket[exn:fail:contract:variable] or @racket[exn:fail:syntax]
exception.}
 

@defproc[(namespace-set-variable-value! [sym symbol?]
                                        [v any/c]
                                        [map? any/c #f]
                                        [namespace namespace? (current-namespace)])
         void?]{

Sets the value of @racket[sym] in the top-level environment of
@racket[namespace] in the @tech{base phase}, defining @racket[sym] if
it is not already defined.

If @racket[map?] is supplied as true, then the namespace's
@tech{identifier} mapping is also adjusted (see
@secref["namespace-model"]) in the @tech{phase level} corresponding to
the @tech{base phase}, so that @racket[sym] maps to the variable.}


@defproc[(namespace-undefine-variable! [sym symbol?]
                                       [namespace namespace? (current-namespace)])
         void?]{

Removes the @racket[sym] variable, if any, in the top-level
environment of @racket[namespace] in its @tech{base phase}. The
namespace's @tech{identifier} mapping (see @secref["namespace-model"])
is unaffected.}

 
@defproc[(namespace-mapped-symbols [namespace namespace? (current-namespace)])
         (listof symbol?)]{

Returns a list of all symbols that are mapped to variables, syntax,
and imports in @racket[namespace] for the @tech{phase level}
corresponding to the @tech{namespace}'s @tech{base phase}.}



@defproc[(namespace-require [quoted-raw-require-spec any/c])
         void?]{

Performs the import corresponding to @racket[quoted-raw-require-spec]
in the top-level environment of the current namespace, like a
top-level @racket[#%require]. The @racket[quoted-raw-require-spec]
argument must be a datum that corresponds to a quoted
@racket[_raw-require-spec] for @racket[#%require], which includes
module paths.

Module paths in @racket[quoted-raw-require-spec] are resolved with respect
to @racket[current-load-relative-directory] or
@racket[current-directory] (if the former is @racket[#f]), even if the
current namespace corresponds to a module body.}


@defproc[(namespace-require/copy [quoted-raw-require-spec any/c])
         void?]{

Like @racket[namespace-require] for syntax exported from the module,
but exported variables at the namespace's @tech{base phase} are
treated differently: the export's current value is copied to a
top-level variable in the current namespace.}


@defproc[(namespace-require/constant [quoted-raw-require-spec any/c])
         void?]{

Like @racket[namespace-require], but for each exported variable at the
@tech{namespace}'s @tech{base phase}, the export's value is copied to
a corresponding top-level variable that is made immutable. Despite
setting the top-level variable, the corresponding identifier is bound
as imported.}


@defproc[(namespace-require/expansion-time [quoted-raw-require-spec any/c])
         void?]{

Like @racket[namespace-require], but only the transformer part of the
module is executed relative to the @tech{namespace}'s @tech{base
phase}; that is, the module is merely @tech{visit}ed, and not
@tech{instantiate}d (see @secref["mod-parse"]). If the required module
has not been instantiated before, the module's variables remain
undefined.}


@defproc[(namespace-attach-module [src-namespace namespace?]
                                  [modname module-path?]
                                  [dest-namespace namespace? (current-namespace)])
         void?]{

Attaches the instantiated module named by @racket[modname] in
@racket[src-namespace] (at its @tech{base phase}) to the @tech{module
registry} of @racket[dest-namespace].

In addition to @racket[modname], every module that it imports
(directly or indirectly) is also recorded in the current namespace's
@tech{module registry}, and instances at the same @tech{phase} or
lower are also attached to @racket[dest-namespace] (while
@tech{visits} at the module's phase and instances at higher phases are
not attached, nor even made @tech{available} for on-demand
@tech{visits}). The inspector of the module invocation in
@racket[dest-namespace] is the same as inspector of the invocation in
@racket[src-namespace].

If @racket[modname] is not a symbol, the current module name resolver
is called to resolve the path, but no module is loaded; the resolved
form of @racket[modname] is used as the module name in
@racket[dest-namespace].

If @racket[modname] refers to a submodule or a module with submodules,
unless the module was loaded from bytecode (i.e., a @filepath{.zo}
file) independently from submodules within the same top-level module,
then declarations for all submodules within the module's top-level
module are also attached to @racket[dest-namespace].

If @racket[modname] does not refer to an @tech{instantiate}d module in
@racket[src-namespace], or if the name of any module to be attached
already has a different declaration or same-@tech{phase} instance in
@racket[dest-namespace], then the @exnraise[exn:fail:contract].

If @racket[src-namespace] and @racket[dest-namespace] do not have the
same @tech{base phase}, then the @exnraise[exn:fail:contract].}


@defproc[(namespace-attach-module-declaration [src-namespace namespace?]
                                              [modname module-path?]
                                              [dest-namespace namespace? (current-namespace)])
         void?]{

Like @racket[namespace-attach-module], but the module
specified by @racket[modname] need only be declared (and not
necessarily @tech{instantiate}d) in @racket[src-namespace], and the
module is merely declared in @racket[dest-namespace].}


@defproc[(namespace-unprotect-module [inspector inspector?]
                                     [modname module-path?]
                                     [namespace namespace? (current-namespace)])
         void?]{

Changes the inspector for the instance of the module referenced by
@racket[modname] in @racket[namespace]'s @tech{module registry} so
that it is controlled by the current code inspector. The given
@racket[inspector] must currently control the invocation of the module
in @racket[namespace]'s @tech{module registry}, otherwise the
@exnraise[exn:fail:contract]. See also @secref["modprotect"].}


@defproc[(namespace-module-registry [namespace namespace?])
         any]{

Returns the @tech{module registry} of the given namespace. This value
is useful only for identification via @racket[eq?].}


@defproc[(module->namespace [modname module-path?]) namespace?]{

Returns a namespace that corresponds to the body of an instantiated
module in the current namespace's @tech{module registry} and in the
current namespace's @tech{base phase}, making the module at the
@tech{available} for on-demand @tech{visits} at the namespace's
@tech{base phase}. The returned namespace has the same @tech{module
registry} as the current namespace. Modifying a binding in the
namespace changes the binding seen in modules that require the
namespace's module.

Module paths in a top-level @racket[require] expression are resolved
with respect to the namespace's module. New @racket[provide]
declarations are not allowed.

If the current code inspector does not control the invocation of the
module in the current namespace's @tech{module registry}, the
@exnraise[exn:fail:contract]; see also @secref["modprotect"].

Bindings in the namespace cannot be modified if the
@racket[compile-enforce-module-constants] parameter was true when the
module was declared, unless the module declaration itself included
assignments to the binding via @racket[set!].}


@defproc[(namespace-syntax-introduce [stx syntax-object?]) syntax-object?]{

Returns a syntax object like @racket[stx], except that the current
namespace's bindings are included in the @tech{syntax object}'s
@tech{lexical information} (see @secref["stxobj-model"]). The
additional context is overridden by any existing @tech{top-level
bindings} in the @tech{syntax object}'s @tech{lexical information}, or
by any existing or future @tech{module bindings} in the @tech{lexical
information}.}


@defproc[(module-provide-protected? [module-path-index (or/c symbol? module-path-index?)]
                                    [sym symbol?])
         boolean?]{

Returns @racket[#f] if the module declaration for
@racket[module-path-index] defines @racket[sym] and exports it
unprotected, @racket[#t] otherwise (which may mean that the symbol
corresponds to an unexported definition, a protected export, or an
identifier that is not defined at all within the module).

The @racket[module-path-index] argument can be a symbol; see
@secref["modpathidx"] for more information on module path
indices.

Typically, the arguments to @racket[module-provide-protected?]
correspond to the first two elements of a list produced by
@racket[identifier-binding].}


@defproc[(variable-reference? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a @tech{variable reference}
produced by @racket[#%variable-reference], @racket[#f] otherwise.}


@defproc[(variable-reference-constant? [varref variable-reference?]) boolean?]{

Returns @racket[#t] if the variable represented by @racket[varref]
will retain its current value (i.e., @racket[varref] refers to a
variable that cannot be further modified by @racket[set!] or
@racket[define]), @racket[#f] otherwise.}


@defproc[(variable-reference->empty-namespace [varref variable-reference?])
         namespace?]{

Returns an empty namespace that shares module declarations and
instances with the namespace in which @racket[varref] is instantiated,
and with the same phase as @racket[varref].}


@defproc[(variable-reference->namespace [varref variable-reference?])
         namespace?]{

If @racket[varref] refers to a @tech{module-level variable}, then the
result is a namespace for the module's body in the referenced
variable's @tech{phase}; the result is the same as a namespace
obtained via @racket[module->namespace].

If @racket[varref] refers to a @tech{top-level variable}, then the
result is the namespace in which the referenced variable is defined.}


@defproc[(variable-reference->resolved-module-path [varref variable-reference?])
         (or/c resolved-module-path? #f)]{

If @racket[varref] refers to a @tech{module-level variable}, the
result is a @tech{resolved module path} naming the module.

If @racket[varref] refers to a @tech{top-level variable}, then the
result is @racket[#f].}


@defproc[(variable-reference->module-path-index [varref variable-reference?])
         (or/c module-path-index? #f)]{

If @racket[varref] refers to a @tech{module-level variable}, the
result is a @tech{module path index} naming the module.

If @racket[varref] refers to a @tech{top-level variable}, then the
result is @racket[#f].}


@defproc[(variable-reference->module-source [varref variable-reference?])
         (or/c symbol? (and/c path? complete-path?) #f)]{

If @racket[varref] refers to a @tech{module-level variable}, the
result is a path or symbol naming the module's source (which is
typically, but not always, the same as in the @tech{resolved module
path}).  If the relevant module is a @tech{submodule}, the result
corresponds to the enclosing top-level module's source.

If @racket[varref] refers to a @tech{top-level variable}, then the
result is @racket[#f].}


@defproc[(variable-reference->phase [varref variable-reference?])
         exact-nonnegative-integer?]{

Returns the @tech{phase} of the variable referenced by @racket[varref].}


@defproc[(variable-reference->module-base-phase [varref variable-reference?])
         exact-integer?]{

Returns the @tech{phase} in which the module is instantiated for the
variable referenced by @racket[varref], or @racket[0] if the variable
for @racket[varref] is not within a module.

For a variable with a module, the result is less than the result of
@racket[(variable-reference->phase varref)] by @math{n} when the
variable is bound at @tech{phase level} @math{n} within the module.}


@defproc[(variable-reference->module-declaration-inspector [varref variable-reference?])
         inspector?]{

Returns the declaration @tech{inspector} (see @secref["modprotect"])
for the module of @racket[varref], where @racket[varref] must refer to
an anonymous module variable as produced by
@racket[(#%variable-reference)].}
