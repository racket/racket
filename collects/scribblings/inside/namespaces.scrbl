#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "im:env"]{Namespaces and Modules}

A Racket namespace (a top-level environment) is represented by a value
of type @cppi{Scheme_Env*} --- which is also a Racket value, castable
to @cpp{Scheme_Object*}. Calling @cppi{scheme_basic_env} returns a
namespace that includes all of Racket's standard global procedures
and syntax.

The @cpp{scheme_basic_env} function must be called once by an
embedding program, before any other Racket function is called
(except @cpp{scheme_make_param}), but @cpp{scheme_main_setup}
automatically calls @cpp{scheme_basic_env}. The returned namespace is
the initial current namespace for the main Racket thread. Racket
extensions cannot call @cpp{scheme_basic_env}.

The current thread's current namespace is available from
@cppi{scheme_get_env}, given the current parameterization (see
@secref["config"]): @cpp{scheme_get_env(scheme_config)}.

New values can be added as @as-index{globals} in a namespace using
@cppi{scheme_add_global}. The @cppi{scheme_lookup_global} function
takes a Racket symbol and returns the global value for that name, or
@cpp{NULL} if the symbol is undefined.

A @as-index{module}'s set of top-level bindings is implemented using
the same machinery as a namespace. Use @cppi{scheme_primitive_module}
to create a new @cpp{Scheme_Env*} that represents a primitive
module. The name provided to @cppi{scheme_primitive_module} is subject
to change through the @racket[current-module-declare-name] parameter
(which is normally set by the module name resolver when auto-loading
module files). After installing variables into the module with
@cppi{scheme_add_global}, etc., call
@cppi{scheme_finish_primitive_module} on the @cpp{Scheme_Env*} value
to make the module declaration available. All defined variables are
exported from the primitive module.

The Racket @indexed-racket[#%variable-reference] form produces a value
that is opaque to Racket code. Use @cpp{SCHEME_PTR_VAL} on the result
of @racket[#%variable-reference] to obtain the same kind of value as
returned by @cpp{scheme_global_bucket} (i.e., a bucket containing the
variable's value, or @cpp{NULL} if the variable is not yet defined).

@; ----------------------------------------------------------------------

@function[(void scheme_add_global
           [char* name]
           [Scheme_Object* val]
           [Scheme_Env* env])]{

Adds a value to the table of globals for the namespace @var{env},
 where @var{name} is a null-terminated string. (The string's case will
 be normalized in the same way as for interning a symbol.)}

@function[(void scheme_add_global_symbol
           [Scheme_Object* name]
           [Scheme_Object* val]
           [Scheme_Env* env])]{

Adds a value to the table of globals by symbol name instead of string
 name.}

@function[(Scheme_Object* scheme_lookup_global
           [Scheme_Object* symbol]
           [Scheme_Env* env])]{

Given a global variable name (as a symbol) in @var{sym}, returns the current
value.}

@function[(Scheme_Bucket* scheme_global_bucket
           [Scheme_Object* symbol]
           [Scheme_Env* env])]{

Given a global variable name (as a symbol) in @var{sym}, returns the bucket
where the value is stored. When the value in this bucket is @cpp{NULL}, then
the global variable is undefined.

The @cppi{Scheme_Bucket} structure is defined as:

@verbatim[#:indent 2]{
  typedef struct Scheme_Bucket {
    Scheme_Object so; /* so.type = scheme_variable_type */
    void *key;
    void *val;
  } Scheme_Bucket;
}}

@function[(Scheme_Bucket* scheme_module_bucket
           [Scheme_Object* mod]
           [Scheme_Object* symbol]
           [int pos]
           [Scheme_Env* env])]{

Like @cpp{scheme_global_bucket}, but finds a variable in a
 module. The @var{mod} and @var{symbol} arguments are as for
 @racket[dynamic-require] in Racket. The @var{pos} argument should be
 @cpp{-1} always. The @var{env} argument represents the namespace in
 which the module is declared.}

@function[(void scheme_set_global_bucket
           [char* procname]
           [Scheme_Bucket* var]
           [Scheme_Object* val]
           [int set_undef])]{

Changes the value of a global variable. The @var{procname} argument is
used to report errors (in case the global variable is constant, not
yet bound, or bound as syntax). If @var{set_undef} is not 1, then the
global variable must already have a binding. (For example,
@racket[set!] cannot set unbound variables, while @racket[define]
can.)}

@function[(Scheme_Object* scheme_builtin_value
           [const-char* name])]{

Gets the binding of a name as it would be defined in the initial
namespace.}

@function[(Scheme_Env* scheme_get_env
           [Scheme_Config* config])]{

Returns the current namespace for the given parameterization (see
@secref["config"]). The current thread's current parameterization is
available as @cppi{scheme_config}.}

@function[(Scheme_Env* scheme_primitive_module
           [Scheme_Object* name]
           [Scheme_Env* for_env])]{

Prepares a new primitive module whose name is the symbol @var{name} (or an
 alternative that is active via @racket[current-module-declare-name]). The
 module will be declared within the namespace @var{for_env}. The
 result is a @cpp{Scheme_Env *} value that can be used with
 @cpp{scheme_add_global}, etc., but it represents a module instead
 of a namespace. The module is not fully declared until
 @cpp{scheme_finish_primitive_module} is called, at which point all
 variables defined in the module become exported.}

@function[(void scheme_finish_primitive_module
           [Scheme_Env* env])]{

Finalizes a primitive module and makes it available for use within the
 module's namespace.}
