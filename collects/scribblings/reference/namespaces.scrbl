#lang scribble/doc
@(require "mz.ss")

@title[#:tag "Namespaces"]{Namespaces}

See @secref["namespace-model"] for basic information on the
@tech{namespace} model.

A new @tech{namespace} is created with procedures like
@scheme[make-empty-namespace], and @scheme[make-base-namespace], which
return a first-class namespace value. A namespace is used by setting
the @scheme[current-namespace] parameter value, or by providing the
namespace to procedures such as @scheme[eval] and
@scheme[eval-syntax].

@defproc[(namespace? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a namespace value, @scheme[#f]
otherwise.}


@defproc[(make-empty-namespace) namespace?]{

Creates a new namespace that is empty, and whose @tech{module
registry} contains no mappings. The namespace's @tech{base phase} is
the same as the @tech{base phase} of the @tech{current
namespace}. Attach modules from an existing namespace to the new one
with @scheme[namespace-attach-module].}


@defproc[(make-base-empty-namespace) namespace?]{

Creates a new empty namespace, but with @schememodname[scheme/base]
attached. The namespace's @tech{base phase} is the same as the
@tech{phase} in which the @scheme[make-base-empty-namespace]
function was created.}


@defproc[(make-base-namespace) namespace?]{

Creates a new namespace with @schememodname[scheme/base] attached and
@scheme[require]d into the top-level environment. The namespace's
@tech{base phase} is the same as the @tech{phase} in which the
@scheme[make-base-namespace] function was created.}


@defform[(define-namespace-anchor id)]{

Binds @scheme[id] to a namespace anchor that can be used with
@scheme[namespace-anchor->empty-namespace] and
@scheme[namespace-anchor->namespace].

This form can be used only in a @tech{top-level context} or in a
@tech{module-context}.}


@defproc[(namespace-anchor? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a namespace-anchor value,
@scheme[#f] otherwise.}


@defproc[(namespace-anchor->empty-namespace [a namespace-anchor?]) namespace?]{

Returns an empty namespace that shares a @tech{module registry} with
the source of the anchor, and whose @tech{base phase} the
@tech{phase} in which the anchor was created.

If the anchor is from a @scheme[define-namespace-anchor] form in a
module context, then the source is the namespace in which the
containing module is instantiated. If the anchor is from a
@scheme[define-namespace-anchor] form in a top-level content, then the
source is the namespace in which the anchor definition was evaluated.}


@defproc[(namespace-anchor->namespace [a namespace-anchor?]) namespace?]{

Returns a namespace corresponding to the source of the anchor.

If the anchor is from a @scheme[define-namespace-anchor] form in a
module context, then the result is a namespace for the module's body
in the anchor's phase. The result is the same as a namespace obtained
via @scheme[module->namespace].

If the anchor is from a @scheme[define-namespace-anchor] form in a
top-level content, then the result is the namespace in which the
anchor definition was evaluated.}


@defparam[current-namespace n namespace?]{

A parameter that determines the @techlink{current namespace}.}


@defproc[(namespace-symbol->identifier [sym symbol?]) identifier?]{

Similar to @scheme[datum->syntax] restricted to symbols. The
@tech{lexical information} of the resulting identifier corresponds to
the top-level environment of the current namespace; the identifier has
no source location or properties.}


@defproc[(namespace-base-phase [namespace namespace? (current-namespace)]) exact-integer?]{

Returns the @tech{base phase} of @scheme[namespace].}


@defproc[(namespace-module-identifier [where (or/c namespace? exact-integer? #f)
                                             (current-namespace)])
         identifier?]{

Returns an identifier whose binding is @scheme[module] in the
@tech{base phase} of @scheme[where] if it is a namespace, or in the
@scheme[where] @tech{phase level} otherwise.

The @tech{lexical information} of the identifier includes bindings (in
the same @tech{phase level}) for all syntactic forms that appear in
fully expanded code (see @secref["fully-expanded"]), but using the
name reported by the second element of @scheme[identifier-binding] for
the binding; the @tech{lexical information} may also include other
bindings.}


@defproc[(namespace-variable-value [sym symbol?]
                                   [use-mapping? any/c #t]
                                   [failure-thunk (or/c (-> any) #f) #f]
                                   [namespace namespace? (current-namespace)])
         any]{

Returns a value for @scheme[sym] in @scheme[namespace], using
@scheme[namespace]'s @tech{base phase}. The returned value depends on
@scheme[use-mapping?]:

 @itemize[

   @item{If @scheme[use-mapping?] is true (the default), and if
   @scheme[sym] maps to a top-level variable or an imported variable
   (see @secref["namespace-model"]), then the result is the same as
   evaluating @scheme[sym] as an expression. If @scheme[sym] maps to
   syntax or imported syntax, then @scheme[failure-thunk] is called or
   the @exnraise[exn:fail:syntax]. If @scheme[sym] is mapped to an
   undefined variable or an uninitialized module variable, then
   @scheme[failure-thunk] is called of the
   @exnraise[exn:fail:contract:variable].}

   @item{If @scheme[use-mapping?] is @scheme[#f], the namespace's
   syntax and import mappings are ignored. Instead, the value of the
   top-level variable named @scheme[sym] in namespace is returned. If
   the variable is undefined, then @scheme[failure-thunk] is called or
   the @exnraise[exn:fail:contract:variable].}

 ]

If @scheme[failure-thunk] is not @scheme[#f],
@scheme[namespace-variable-value] calls @scheme[failure-thunk] to
produce the return value in place of raising an
@scheme[exn:fail:contract:variable] or @scheme[exn:fail:syntax]
exception.}
 

@defproc[(namespace-set-variable-value! [sym symbol?]
                                        [v any/c]
                                        [map? any/c #f]
                                        [namespace namespace? (current-namespace)])
         void?]{

Sets the value of @scheme[sym] in the top-level environment of
@scheme[namespace] in the @tech{base phase}, defining @scheme[sym] if
it is not already defined.

If @scheme[map?] is supplied as true, then the namespace's
@tech{identifier} mapping is also adjusted (see
@secref["namespace-model"]) in the @tech{phase level} corresponding to
the @tech{base phase}, so that @scheme[sym] maps to the variable.}


@defproc[(namespace-undefine-variable! [sym symbol?]
                                       [namespace namespace? (current-namespace)])
         void?]{

Removes the @scheme[sym] variable, if any, in the top-level
environment of @scheme[namespace] in its @tech{base phase}. The
namespace's @tech{identifier} mapping (see @secref["namespace-model"])
is unaffected.}

 
@defproc[(namespace-mapped-symbols [namespace namespace? (current-namespace)])
         (listof symbol?)]{

Returns a list of all symbols that are mapped to variables, syntax,
and imports in @scheme[namespace] for the @tech{phase level}
corresponding to the @tech{namespace}'s @tech{base phase}.}



@defproc[(namespace-require [quoted-raw-require-spec any/c])
         void?]{

Performs the import corresponding to @scheme[quoted-raw-require-spec]
in the top-level environment of the current namespace, like a
top-level @scheme[#%require]. The @scheme[quoted-raw-require-spec]
argument must be a datum that corresponds to a quoted
@scheme[_raw-require-spec] for @scheme[#%require], which includes
module paths.

Module paths in @scheme[quoted-raw-require-spec] are resolved with respect
to @scheme[current-load-relative-directory] or
@scheme[current-directory] (if the former is @scheme[#f]), even if the
current namespace corresponds to a module body.}


@defproc[(namespace-require/copy [quoted-raw-require-spec any/c])
         void?]{

Like @scheme[namespace-require] for syntax exported from the module,
but exported variables at the namespace's @tech{base phase} are
treated differently: the export's current value is copied to a
top-level variable in the current namespace.}


@defproc[(namespace-require/constant [quoted-raw-require-spec any/c])
         void?]{

Like @scheme[namespace-require], but for each exported variable at the
@tech{namespace}'s @tech{base phase}, the export's value is copied to
a corresponding top-level variable that is made immutable. Despite
setting the top-level variable, the corresponding identifier is bound
as imported.}


@defproc[(namespace-require/expansion-time [quoted-raw-require-spec any/c])
         void?]{

Like @scheme[namespace-require], but only the transformer part of the
module is executed relative to the @tech{namespace}'s @tech{base
phase}; that is, the module is merely @tech{visit}ed, and not
@tech{instantiate}d (see @secref["mod-parse"]). If the required module
has not been instantiated before, the module's variables remain
undefined.}


@defproc[(namespace-attach-module [src-namespace namespace?]
                                  [modname module-path?]
                                  [dest-namespace namespace? (current-namespace)])
         any]{

Attaches the instantiated module named by @scheme[modname] in
@scheme[src-namespace] (at its @tech{base phase}) to the @tech{module
registry} of @scheme[dest-namespace]. If @scheme[modname] is not a
symbol, the current module name resolver is called to resolve the
path, but no module is loaded; the resolved form of @scheme[modname]
is used as the module name in @scheme[dest-namespace]. In addition to
@scheme[modname], every module that it imports (directly or
indirectly) is also recorded in the current namespace's @tech{module
registry}, and instances at the same @tech{phase} or lower are also
attached to @scheme[dest-namespace] (while @tech{visits} at the
module's phase and instances at higher phases are not attached, nor
even made @tech{available} for on-demand @tech{visits}). The inspector
of the module invocation in @scheme[dest-namespace] is the same as
inspector of the invocation in @scheme[src-namespace].

If @scheme[modname] does not refer to an @tech{instantiate}d module in
@scheme[src-namespace], or if the name of any module to be attached
already has a different declaration or same-@tech{phase} instance in
@scheme[dest-namespace], then the @exnraise[exn:fail:contract].

If @scheme[src-namespace] and @scheme[dest-namespace] do not have the
same @tech{base phase}, then the @exnraise[exn:fail:contract].}


@defproc[(namespace-unprotect-module [inspector inspector?]
                                     [modname module-path?]
                                     [namespace namespace? (current-namespace)])
         void?]{

Changes the inspector for the instance of the module referenced by
@scheme[modname] in @scheme[namespace]'s @tech{module registry} so
that it is controlled by the current code inspector. The given
@scheme[inspector] must currently control the invocation of the module
in @scheme[namespace]'s @tech{module registry}, otherwise the
@exnraise[exn:fail:contract]. See also @secref["modprotect"].}


@defproc[(namespace-module-registry [namespace namespace?])
         any]{

Returns the @tech{module registry} of the given namespace. This value
is useful only for identification via @scheme[eq?].}


@defproc[(module->namespace [modname module-path?]) namespace?]{

Returns a namespace that corresponds to the body of an instantiated
module in the current namespace's @tech{module registry} and in the
current namespace's @tech{base phase}, making the module at the
@tech{available} for on-demand @tech{visits} at the namespace's
@tech{base phase}. The returned namespace has the same @tech{module
registry} as the current namespace. Modifying a binding in the
namespace changes the binding seen in modules that require the
namespace's module.

Module paths in a top-level @scheme[require] expression are resolved
with respect to the namespace's module. New @scheme[provide]
declarations are not allowed.

If the current code inspector does not control the invocation of the
module in the current namespace's @tech{module registry}, the
@exnraise[exn:fail:contract]; see also @secref["modprotect"].

Bindings in the namespace cannot be modified if the
@scheme[compile-enforce-module-constants] parameter was true when the
module was declared, unless the module declaration itself included
assignments to the binding via @scheme[set!].}


@defproc[(namespace-syntax-introduce [stx syntax-object?]) syntax-object?]{

Returns a syntax object like @scheme[stx], except that the current
namespace's bindings are included in the @tech{syntax object}'s
@tech{lexical information} (see @secref["stxobj-model"]). The
additional context is overridden by any existing @tech{top-level
bindings} in the @tech{syntax object}'s @tech{lexical information}, or
by any existing or future @tech{module bindings} in the @tech{lexical
information}.}


@defproc[(module-provide-protected? [module-path-index (or/c symbol? module-path-index?)]
                                    [sym symbol?])
         boolean?]{

Returns @scheme[#f] if the module declaration for
@scheme[module-path-index] defines @scheme[sym] and exports it
unprotected, @scheme[#t] otherwise (which may mean that the symbol
corresponds to an unexported definition, a protected export, or an
identifier that is not defined at all within the module).

The @scheme[module-path-index] argument can be a symbol; see
@secref["modpathidx"] for more information on module path
indices.

Typically, the arguments to @scheme[module-provide-protected?]
correspond to the first two elements of a list produced by
@scheme[identifier-binding].}


@defproc[(variable-reference? [v any/c]) boolean?]{

Return @scheme[#t] if @scheme[v] is a @tech{variable reference}
produced by @scheme[#%variable-reference], @scheme[#f] otherwise.}

@defproc[(variable-reference->empty-namespace [varref variable-reference?])
         namespace?]{

Returns an empty namespace that shares module declarations and
instances with the namespace in which @scheme[varref] is instantiated,
and with the same phase as @scheme[varref].}


@defproc[(variable-reference->namespace [varref variable-reference?])
         namespace?]{

If @scheme[varref] refers to a @tech{module-level variable}, then the
result is a namespace for the module's body in the referenced
variable's @tech{phase}; the result is the same as a namespace
obtained via @scheme[module->namespace].

If @scheme[varref] refers to a @tech{top-level variable}, then the
result is the namespace in which the referenced variable is defined.}


@defproc[(variable-reference->resolved-module-path [varref variable-reference?])
         (or/c resolved-module-path? #f)]{

If @scheme[varref] refers to a @tech{module-level variable}, the
result is a @tech{resolved module path} naming the module.

If @scheme[varref] refers to a @tech{top-level variable}, then the
result is @scheme[#f].}

@defproc[(variable-reference->module-source [varref variable-reference?])
         (or/c symbol? (and/c path? complete-path?) #f)]{

If @scheme[varref] refers to a @tech{module-level variable}, the
result is a path or symbol naming the module's source (which is
typically, but not always, the same as in the resolved module path).

If @scheme[varref] refers to a @tech{top-level variable}, then the
result is @scheme[#f].}

@defproc[(variable-reference->phase [varref variable-reference?])
         exact-nonnegative-integer?]{

Returns the @tech{phase} of the variable referenced by @scheme[varref].}
