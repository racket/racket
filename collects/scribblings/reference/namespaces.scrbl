#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{Namespaces}

See @secref["mz:namespace-model"] for basic information on the
namespace model.

A new namespace is created with the @scheme[make-namespace] procedure,
which returns a first-class namespace value. A namespace is used by
setting the @scheme[current-namespace] parameter value, or by
providing the namespace to procedures such as @scheme[eval] and
@scheme[eval-syntax].

[FIXME: define the initial namespace.]


@defproc[(namespace? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a namespace value, @scheme[#f]
otherwise.}


@defproc[(make-namespace [flag (one-of/c 'initial 'empty) 'initial]) namespace?]{

Creates a new namespace with a new module registry. The @scheme[flag]
is an option that determines the initial bindings in the namespace:

@itemize{

   @item{@indexed-scheme['initial] --- the new namespace contains the
   module declarations of the initial namespace, and the new
   namespace's @tech{phase-level} 1 top-level environment contains
   bindings and imports as in the initial namespace. However, the
   namespace's @tech{phase-level} 1 top-level environment is empty.}

   @item{@indexed-scheme['empty] --- creates a namespace with no
   initial bindings or module declarations.}

}}


@defproc[(namespace-symbol->identifier [sym symbol?]) identifier?]{

Similar to @scheme[datum->syntax-object] restricted to symbols. The
lexical context of the resulting identifier corresponds to the
top-level environment of the current namespace; the identifier has no
source location or properties.}

@defproc[(namespace-variable-value [sym symbol?]
                                   [use-mapping? any/c #t]
                                   [failure-thunk (or/c (-> any) false/c) #f]
                                   [namespace namespace? (current-namespace)])
         any]{

Returns a value for @scheme[sym] in @scheme[namespace]. The returned value
depends on @scheme[use-mapping?]:

 @itemize{

   @item{If @scheme[use-mapping?] is true (the default), and if
   @scheme[sym] maps to a top-level variable or an imported variable
   (see @secref["mz:namespace-model"]), then the result is the same as
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

 }

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
@scheme[namespace] for @tech{phase level} 0, defining @scheme[sym] if
it is not already defined.

If @scheme[map?] is supplied as true, then the namespace's identifier
mapping is also adjusted (see @secref["mz:namespace-model"]) so that
@scheme[sym] maps to the variable.}


@defproc[(namespace-undefine-variable! [sym symbol?]
                                       [namespace namespace? (current-namespace)])
         void?]{

Removes the @scheme[sym] variable, if any, in the top-level
environment of @scheme[namespace] at @tech{phase level} 0. The
namespace's identifier mapping (see @secref["mz:namespace-model"]) is
unaffected.}

 
@defproc[(namespace-mapped-symbols [namespace namespace? (current-namespace)])
         (listof symbol?)]{

Returns a list of all symbols that are mapped to variables, syntax,
and imports in @scheme[namespace] for @tech{phase level} 0.}



@defproc[(namespace-require [quoted-require-spec any/c])
         void?]{

Performs the import corresponding to @scheme[quoted-require-spec] in
the top-level environment of the current namespace, like a top-level
@scheme[require]. The @scheme[quoted-require-spec] argument must be a
datum that corresponds to a quoted @scheme[_require-spec] for
@scheme[require].

Module paths in @scheme[quoted-require-spec] are resolved with respect
to @scheme[current-load-relative-directory] or
@scheme[current-directory] (if the former is @scheme[#f]), even if the
current namespace corresponds to a module body.}


@defproc[(namespace-transformer-require [quoted-require-spec any/c])
         void?]{

Like @scheme[namespace-require], but analogous to a top-level
@scheme[require-for-syntax].}


@defproc[(namespace-require/copy [quoted-require-spec any/c])
         void?]{

Like @scheme[namespace-require] for syntax exported from the module,
but exported variables are treated differently: the export's current
value is copied to a top-level variable in the current namespace.}


@defproc[(namespace-require/expansion-time [quoted-require-spec any/c])
         void?]{

Like @scheme[namespace-require], but only the transformer part of the
module is executed; that is, the module is merely @tech{visit}ed, and
not @tech{instantiate}d (see @secref["mz:mod-parse"]). If the required
module has not been instantiated before, the module's variables remain
undefined.}


@defproc[(namespace-attach-module [src-namespace namespace?]
                                  [modname module-path?]
                                  [dest-namespace namespace? (current-namespace)])
         any]{

Attaches the instantiated module named by @scheme[modname] in
@scheme[src-namespace] to the registry of @scheme[dest-namespace]. If
@scheme[modname] is not a symbol, the current module name resolver is
called to resolve the path, but no module is loaded; the resolved form
of @scheme[modname] is used as the module name in
@scheme[dest-namespace]. In addition to @scheme[modname], every module
that it imports (directly or indirectly) is also recorded in the
current namespace's registry. If @scheme[modname] does not refer to an
instantiated module in @scheme[src-namespace], or if the name of any
module to be attached already has a different declaration or instance
in @scheme[dest-namespace], then the @exnraise[exn:fail:contract].
The inspector of the module invocation in @scheme[dest-namespace] is
the same as inspector of the invocation in @scheme[src-namespace].}


@defproc[(namespace-unprotect-module [inspector inspector?]
                                     [modname module-path?]
                                     [namespace namespace? (current-namespace)])
         void?]{

Changes the inspector for the instance of the module referenced by
@scheme[modname] in @scheme[namespace]'s registry so that it is
controlled by the current code inspector. The given @scheme[inspector]
must currently control the invocation of the module in
@scheme[namespace]'s registry, otherwise the
@exnraise[exn:fail:contract]. See also @secref["mz:modprotect"].}


@defproc[(namespace-module-registry [namespace namespace?])
         any]{

Returns the registry of the given namespace. This value is useful only
for identification via @scheme[eq?].}


@defproc[(module->namespace [modname module-path?]) namespace?]{

Returns a namespace that corresponds to the body of an instantiated
module in the current namespace's registry. The returned namespace has
the same module registry as the current namespace. Modifying a binding
in the namespace changes the binding seen in modules that require the
namespace's module.

Module paths in a top-level @scheme[require] expression are resolved
with respect to the namespace's module. New @scheme[provide]
declarations are not allowed.

If the current code inspector does not control the invocation of the
module in the current namespace's registry, the
@exnraise[exn:fail:contract]; see also
@secref["mz:modprotect"].

Bindings in the namespace cannot be modified if the
@scheme[compile-enforce-module-constants] parameter was true when the
module was declared, unless the module declaration itself included
assignments to the binding via @scheme[set!].}


@defproc[(namespace-syntax-introduce [stx syntax-object?]) syntax-object?]{

Returns a syntax object like @scheme[stx], except that the current
namespace's bindings are included in the @tech{syntax object}'s
@tech{lexical information} (see @secref["mz:stxobj-model"]). The
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
@secref["mz:modpathidx"] for more information on module path
indices.

Typically, the arguments to @scheme[module-provide-protected?]
correspond to the first two elements of a list produced by
@scheme[identifier-binding].}
