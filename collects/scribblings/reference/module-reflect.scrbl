#lang scribble/doc
@(require "mz.rkt"
          (for-label compiler/embed))

@title{Module Names and Loading}

@;------------------------------------------------------------------------
@section[#:tag "modnameresolver"]{Resolving Module Names}

The name of a declared module is represented by a @deftech{resolved
module path}, which encapsulates either a symbol or a complete
filesystem path (see @secref["pathutils"]). A symbol normally refers
to a predefined module or module declared through reflective
evaluation (e.g., @racket[eval]). A filesystem path normally refers to
a module declaration that was loaded on demand via @racket[require] or
other forms.

A @deftech{module path} is a datum that matches the grammar for
@racket[_module-path] for @racket[require]. A module path is relative
to another module.

@defproc[(resolved-module-path? [v any/c]) boolean?]{

Returns @racket[#f] if @racket[v] is a @tech{resolved module path},
@racket[#f] otherwise.}

@defproc[(make-resolved-module-path [path (or/c symbol? (and/c path? complete-path?))])
         resolved-module-path?]{

Returns a @tech{resolved module path} that encapsulates @racket[path].
If @racket[path] is not a symbol, it normally should be
@tech{cleanse}d (see @racket[cleanse-path]) and simplified (see
@racket[simplify-path]).

A @tech{resolved module path} is interned. That is, if two
@tech{resolved module path} values encapsulate paths that are
@racket[equal?], then the @tech{resolved module path} values are
@racket[eq?].}

@defproc[(resolved-module-path-name [module-path resolved-module-path?])
         (or/c path? symbol?)]{

Returns the path or symbol encapsulated by a @tech{resolved module path}.}


@defproc[(module-path? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] corresponds to a datum that matches
the grammar for @racket[_module-path] for @racket[require],
@racket[#f] otherwise.}


@defparam[current-module-name-resolver proc
           (case->
            (resolved-module-path? . -> . any)
            ((or/c module-path? path?)
             (or/c #f resolved-module-path?)
             (or/c #f syntax?)
             boolean?
             . -> .
             resolved-module-path?))]{

A parameter that determines the current @deftech{module name
resolver}, which manages the conversion from other kinds of module
references to a @tech{resolved module path}. For example,
when the expander encounters @racket[(require _module-path)] where
@racket[_module-path] is not an identifier, then the expander passes
@racket['@#,racket[_module-path]] to the module name resolver to obtain a symbol
or resolved module path. When such a @racket[require] appears within a
module, the @deftech{module path resolver} is also given the name of
the enclosing module, so that a relative reference can be converted to
an absolute symbol or @tech{resolved module path}.

A @tech{module name resolver} takes one and four arguments:
@itemize[

  @item{When given one argument, it is a name for a module declaration
  that is already loaded. Such a call to the module name resolver is a
  notification that the corresponding module does not need to be
  loaded (for the current namespace, or any other namespace that
  shares the same module registry). The module name resolver's result
  is ignored.}
 
  @item{When given four arguments, the first is a module path, either
  equivalent to a quoted @racket[_module-path] for @racket[require] or
  a file system path.  The second is name for the source module, if
  any, to which the path is relative; if the second argument is
  @racket[#f], the module path is relative to @racket[(or
  (current-load-relative-directory) (current-directory))].  The third
  argument is a @tech{syntax object} that can be used for error
  reporting, if it is not @racket[#f]. If the last argument is
  @racket[#t], then the module declaration should be loaded (if it is
  not already), otherwise the module path should be simply resolved to
  a name. The result is the resolved name.}

]

For the second case, the standard module name resolver keeps a
per-registry table of loaded module name. If a resolved module path is
not in the table, and @racket[#f] is not provided as the third
argument to the @tech{module name resolver}, then the name is put into
the table and the corresponding file is loaded with a variant of
@racket[load/use-compiled] that passes the expected module name to the
@tech{compiled-load handler}.

While loading a file, the default @tech{module name resolver} sets the
@racket[current-module-declare-name] parameter to the resolved module
name (while the @tech{compiled-load handler} sets
@racket[current-module-declare-source]). Also, the default
@tech{module name resolver} records in a private @tech{continuation
mark} the module being loaded, and it checks whether such a mark
already exists; if such a continuation mark does exist in the current
continuation, then the @exnraise[exn:fail] with a message about a
dependency cycle.

Module loading is suppressed (i.e., @racket[#f] is supplied as a third
argument to the module name resolver) when resolving module paths in
@tech{syntax objects} (see @secref["stxobj-model"]). When a
@tech{syntax object} is manipulated, the current namespace might not
match the original namespace for the syntax object, and the module
should not necessarily be loaded in the current namespace.

The current module name resolver is called with a single argument by
@racket[namespace-attach-module] to notify the resolver that a module
was attached to the current namespace (and should not be loaded in the
future for the namespace's registry). No other Racket operation
invokes the module name resolver with a single argument, but other
tools (such as DrRacket) might call this resolver in this mode to
avoid redundant module loads.}


@defparam[current-module-declare-name name (or/c resolved-module-path? #f)]{

A parameter that determines a module name that is used when evaluating
a @racket[module] declaration (when the parameter value is not
@racket[#f]). In that case, the @racket[_id] from the @racket[module]
declaration is ignored, and the parameter's value is used as the name
of the declared module.}

@defparam[current-module-declare-source src (or/c symbol? (and/c path? complete-path?) #f)]{

A parameter that determines source information to be associated with a
module when evaluating a @racket[module] declaration. Source
information is used in error messages and reflected by
@racket[variable-reference->module-source]. When the parameter value
is @racket[#f], the module's name (as determined by
@racket[current-module-declare-name]) is used as the source name
instead of the parameter value.}

@;------------------------------------------------------------------------
@section[#:tag "modpathidx"]{Compiled Modules and References}

While expanding a @racket[module] declaration, the expander resolves
module paths for imports to load module declarations as necessary and
to determine imported bindings, but the compiled form of a
@racket[module] declaration preserves the original module path.
Consequently, a compiled module can be moved to another filesystem,
where the module name resolver can resolve inter-module references
among compiled code.

When a module reference is extracted from compiled form (see
@racket[module-compiled-imports]) or from syntax objects in macro
expansion (see @secref["stxops"]), the module reference is reported in
the form of a @deftech{module path index}. A @tech{module path index}
is a semi-interned (multiple references to the same relative module
tend to use the same @tech{module path index} value, but not always)
opaque value that encodes a module path (see @racket[module-path?])
and either a @tech{resolved module path} or another @tech{module path
index} to which it is relative.

A @tech{module path index} that uses both @racket[#f] for its path and
base @tech{module path index} represents ``self''---i.e., the module
declaration that was the source of the @tech{module path index}---and
such a @tech{module path index} can be used as the root for a chain of
@tech{module path index}es at compile time. For example, when
extracting information about an identifier's binding within a module,
if the identifier is bound by a definition within the same module, the
identifier's source module is reported using the ``self'' @tech{module
path index}. If the identifier is instead defined in a module that is
imported via a module path (as opposed to a literal module name), then
the identifier's source module will be reported using a @tech{module
path index} that contains the @racket[require]d module path and the
``self'' @tech{module path index}.


A @tech{module path index} has state. When it is @deftech{resolved} to
a @tech{resolved module path}, then the @tech{resolved module path} is
stored with the @tech{module path index}. In particular, when a module
is loaded, its root @tech{module path index} is resolved to match the
module's declaration-time name. This resolved path is forgotten,
however, in identifiers that the module contributes to the compiled
and marshaled form of other modules. The transient nature of resolved
names allows the module code to be loaded with a different resolved
name than the name when it was compiled.

@defproc[(module-path-index? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{module path index},
@racket[#f] otherwise.}


@defproc[(module-path-index-resolve [mpi module-path-index?])
         resolved-module-path?]{

Returns a @tech{resolved module path} for the resolved module name,
computing the resolved name (and storing it in @racket[mpi]) if it has
not been computed before.

Resolving a @tech{module path index} uses the current @tech{module
name resolver} (see @racket[current-module-name-resolver]). Depending
on the kind of module paths encapsulated by @racket[mpi], the computed
resolved name can depend on the value of
@racket[current-load-relative-directory] or
@racket[current-directory].}


@defproc[(module-path-index-split [mpi module-path-index?])
         (values (or/c module-path? #f)
                 (or/c module-path-index? resolved-module-path? #f))]{

Returns two values: a module path, and a base @tech{module path index}
or @racket[#f] to which the module path is relative.

A @racket[#f] second result means that the path is relative to an
unspecified directory (i.e., its resolution depends on the value of
@racket[current-load-relative-directory] and/or
@racket[current-directory]).

A @racket[#f] for the first result implies a @racket[#f] for the
second result, and means that @racket[mpi] represents ``self'' (see
above).}

@defproc[(module-path-index-join [path (or/c module-path? #f)]
                                 [mpi (or/c module-path-index? resolved-module-path? #f)])
         module-path-index?]{

Combines @racket[path] and @racket[mpi] to create a new @tech{module
path index}. The @racket[path] argument can @racket[#f] only if
@racket[mpi] is also @racket[#f].}

@defproc[(compiled-module-expression? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a compiled @racket[module]
declaration, @racket[#f] otherwise. See also
@racket[current-compile].}


@defproc[(module-compiled-name [compiled-module-code compiled-module-expression?])
         symbol?]{

Takes a module declaration in compiled form and returns a symbol for
the module's declared name.}


@defproc[(module-compiled-imports [compiled-module-code compiled-module-expression?])
         (listof (cons/c (or/c exact-integer? #f) 
                         (listof module-path-index?)))]{

Takes a module declaration in compiled form and returns an association
list mapping @tech{phase level} shifts (where @racket[#f] corresponds
to a shift into the @tech{label phase level}) to module references for
the module's explicit imports.}


@defproc[(module-compiled-exports [compiled-module-code compiled-module-expression?])
         (values (listof (cons/c (or/c exact-integer? #f) list?))
                 (listof (cons/c (or/c exact-integer? #f) list?)))]

Returns two association lists mapping @tech{phase level} values (where
@racket[#f] corresponds to the @tech{label phase level}) to exports at
the corresponding phase. The first association list is for exported
variables, and the second is for exported syntax. Beware however, that
value bindings re-exported though a @tech{rename transformer} are in
the syntax list instead of the value list.

Each associated list, which is represented by @racket[list?] in the
result contracts above, more precisely matches the contract

@racketblock[
(listof (list/c symbol?
                (listof 
                 (or/c module-path-index?
                       (list/c module-path-index?
                               (or/c exact-integer? #f)
                               symbol?
                               (or/c exact-integer? #f))))))
]

For each element of the list, the leading symbol is the name of the
export.

The second part---the list of @tech{module path index} values,
etc.---describes the origin of the exported identifier. If the origin
list is @racket[null], then the exported identifier is defined in the
module. If the exported identifier is re-exported, instead, then the
origin list provides information on the import that was re-exported.
The origin list has more than one element if the binding was imported
multiple times from (possibly) different sources.

For each origin, a @tech{module path index} by itself means that the
binding was imported with a @tech{phase level} shift of @racket[0]
(i.e., a plain @racket[require] without @racket[for-meta],
@racket[for-syntax], etc.), and imported identifier has the same name
as the re-exported name. An origin represented with a list indicates
explicitly the import, the import @tech{phase level} shift (where
@racket[#f] corresponds to a @racket[for-label] import), the import
name of the re-exported binding, and the @tech{phase level} of the
import.}

@defproc[(module-compiled-language-info [compiled-module-code compiled-module-expression?])
         (or/c #f (vector/c module-path? symbol? any/c))]{

@guidealso["module-runtime-config"]

Returns information intended to reflect the ``language'' of the
module's implementation as originally attached to the syntax of the
module's declaration though the @indexed-racket['module-language]
@tech{syntax property}. See also @racket[module].

If no information is available for the module, the result is
@racket[#f]. Otherwise, the result is @racket[(vector _mp _name _val)]
such that @racket[((dynamic-require _mp _name) _val)] should return
function that takes two arguments. The function's arguments are a key
for reflected information and a default value.  Acceptable keys and
the interpretation of results is up to external tools, such as
DrRacket.  If no information is available for a given key, the result
should be the given default value.

See also @racket[module->language-info] and
@racketmodname[racket/language-info].}


@;------------------------------------------------------------------------
@section[#:tag "dynreq"]{Dynamic Module Access}

@defproc[(dynamic-require [mod (or/c module-path?
                                     resolved-module-path?
                                     module-path-index?)]
                          [provided (or/c symbol? #f 0 void?)]
                          [fail-thunk (-> any) (lambda () ....)])
         any]{

Dynamically @tech{instantiates} the module specified by @racket[mod]
in the current namespace's registry at the namespace's @tech{base
phase}, if it is not yet @tech{instantiate}d. The current @tech{module
name resolver} may load a module declaration to resolve @racket[mod]
(see @racket[current-module-name-resolver]); the path is resolved
relative to @racket[current-load-relative-directory] and/or
@racket[current-directory].

If @racket[provided] is @racket[#f], then the result is @|void-const|,
and the module is not @tech{visit}ed (see @secref["mod-parse"]) or
even made @tech{available} (for on-demand @tech{visits}) in phases
above the @tech{base phase}.

When @racket[provided] is a symbol, the value of the module's export
with the given name is returned, and still the module is not
@tech{visit}ed or made @tech{available} in higher phases. If the
module exports @racket[provided] as syntax, then a use of the binding
is expanded and evaluated in a fresh namespace to which the module is
attached, which means that the module is @tech{visit}ed in the fresh
namespace. If the module has no such exported variable or syntax, then
@racket[fail-thunk] is called; the default @racket[fail-thunk] raises
@racket[exn:fail:contract]. If the variable named by @racket[provided]
is exported protected (see @secref["modprotect"]), then the
@exnraise[exn:fail:contract].

If @racket[provided] is @racket[0], then the module is
@tech{instantiate}d but not @tech{visit}ed, the same as when
@racket[provided] is @racket[#f]. With @racket[0], however, the module
is made @tech{available} in higher phases.

If @racket[provided] is @|void-const|, then the module is
@tech{visit}ed but not @tech{instantiate}d (see @secref["mod-parse"]),
and the result is @|void-const|.}


@defproc[(dynamic-require-for-syntax [mod module-path?]
                                     [provided (or/c symbol? #f)]
                                     [fail-thunk (-> any) (lambda () ....)])
         any]{

Like @racket[dynamic-require], but in a @tech{phase} that is @math{1}
more than the namespace's @tech{base phase}.}


@defproc[(module->language-info
          [mod (or/c module-path? path? resolved-module-path?)]
          [load? any/c #f])
         (or/c #f (vector/c module-path? symbol? any/c))]{

Returns information intended to reflect the ``language'' of the
implementation of @racket[mod]. If @racket[load?] is @racket[#f], the
module named by @racket[mod] must be declared (but not necessarily
@tech{instantiate}d or @tech{visit}ed) in the current namespace;
otherwise, @racket[mod] may be loaded (as for @racket[dynamic-require]
and other functions). The information returned by
@racket[module->language-info] is the same as would have been returned
by @racket[module-compiled-language-info] applied to the module's
implementation as compiled code.}


@defproc[(module->imports
          [mod (or/c module-path? path? resolved-module-path?)])
         (listof (cons/c (or/c exact-integer? #f) 
                         (listof module-path-index?)))]{

Like @racket[module-compiled-imports], but produces the imports of
@racket[mod], which must be declared (but not necessarily
@tech{instantiate}d or @tech{visit}ed) in the current namespace.}


@defproc[(module->exports
          [mod (or/c module-path? path? resolved-module-path?)])
         (values (listof (cons/c (or/c exact-integer? #f) list?))
                 (listof (cons/c (or/c exact-integer? #f) list?)))]{

Like @racket[module-compiled-exports], but produces the exports of
@racket[mod], which must be declared (but not necessarily
@tech{instantiate}d or @tech{visit}ed) in the current namespace.}

@defproc[(module-predefined?
          [mod (or/c module-path? path? resolved-module-path?)])
         boolean?]{

Reports whether @racket[mod] refers to a module that is predefined for
the running Racket instance. Predefined modules always have a symbolic
resolved module path, and they may be predefined always or
specifically within a particular executable (such as one created by
@exec{raco exe} or @racket[create-embedding-executable]).}
