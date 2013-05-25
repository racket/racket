#lang scribble/doc
@(require "utils.rkt")

@title{Evaluation}

A Racket S-expression is evaluated by calling @cppi{scheme_eval}.
This function takes an S-expression (as a @cpp{Scheme_Object*}) and a
namespace and returns the value of the expression in that namespace.

The function @cppi{scheme_apply} takes a @cpp{Scheme_Object*} that is
a procedure, the number of arguments to pass to the procedure, and an
array of @cpp{Scheme_Object *} arguments. The return value is the
result of the application. There is also a function
@cppi{scheme_apply_to_list}, which takes a procedure and a list
(constructed with @cppi{scheme_make_pair}) and performs the Racket
@racket[apply] operation.

The @cppi{scheme_eval} function actually calls @cppi{scheme_compile}
followed by @cppi{scheme_eval_compiled}.

@; ----------------------------------------------------------------------

@section[#:tag "topleveleval"]{Top-level Evaluation Functions}

The functions @cpp{scheme_eval}, @cpp{scheme_apply}, etc., are
@defterm{top-level evaluation functions}. Continuation invocations are
confined to jumps within a top-level evaluation (i.e., a continuation
barrier is installed by these functions).

The functions @cppi{_scheme_eval_compiled}, @cppi{_scheme_apply},
etc. (with a leading underscore) provide the same functionality
without starting a new top-level evaluation; these functions should
only be used within new primitive procedures. Since these functions
allow full continuation hops, calls to non-top-level evaluation
functions can return zero or multiple times.

Currently, escape continuations and primitive error escapes can jump
out of all evaluation and application functions. For more information,
see @secref["exceptions"].

@; ----------------------------------------------------------------------

@section{Tail Evaluation}

@section-index{tail recursion}

All of Racket's built-in functions and syntax support proper
tail-recursion. When a new primitive procedure or syntax is added to
Racket, special care must be taken to ensure that tail recursion is
handled properly. Specifically, when the final return value of a
function is the result of an application, then
@cppi{scheme_tail_apply} should be used instead of
@cppi{scheme_apply}.  When @cppi{scheme_tail_apply} is called, it
postpones the procedure application until control returns to the
Racket evaluation loop.

For example, consider the following implementation of a
@racket[thunk-or] primitive, which takes any number of thunks and
performs @racket[or] on the results of the thunks, evaluating only as
many thunks as necessary.

@verbatim{
static Scheme_Object *
thunk_or (int argc, Scheme_Object **argv)
{
  int i;
  Scheme_Object *v;

  if (!argc)
    return scheme_false;

  for (i = 0; i < argc - 1; i++)
    if (SCHEME_FALSEP((v = _scheme_apply(argv[i], 0, NULL))))
      return v;

  return scheme_tail_apply(argv[argc - 1], 0, NULL);
}
}

This @racket[thunk-or] properly implements tail-recursion: if the
final thunk is applied, then the result of @racket[thunk-or] is the
result of that application, so @cppi{scheme_tail_apply} is used for
the final application.

@; ----------------------------------------------------------------------

@section[#:tag "multiple"]{Multiple Values}

A primitive procedure can return multiple values by returning the
result of calling @cppi{scheme_values}. The functions
@cppi{scheme_eval_compiled_multi}, @cppi{scheme_apply_multi},
@cppi{_scheme_eval_compiled_multi}, and @cppi{_scheme_apply_multi}
potentially return multiple values; all other evaluation and
applications procedures return a single value or raise an exception.

Multiple return values are represented by the
@cppi{scheme_multiple_values} ``value.'' This quasi-value has the type
@cpp{Scheme_Object*}, but it is not a pointer or a fixnum. When the
result of an evaluation or application is
@cppi{scheme_multiple_values}, the number of actual values can be
obtained as @cppi{scheme_multiple_count}, and the array of
@cpp{Scheme_Object*} values as @cppi{scheme_multiple_array}. (Both of
those identifiers are actually macros.) 

A garbage collection must not occur between the return of a
@cppi{scheme_multiple_values} ``value'' and the receipt of the values
through @cppi{scheme_multiple_count} @cppi{scheme_multiple_array}.
Furthermore, if @cpp{scheme_multiple_array} is to be used across a
potential garbage collection, then it must be specifically received by
calling @cpp{scheme_detach_multiple_array}; otherwise, a garbage
collection or further evaluation may change the content of the array.
Otherwise, if any application or evaluation procedure is called, the
@cpp{scheme_multiple_count} and @cpp{scheme_multiple_array} variables
may be modified (but the array previously referenced by
@cpp{scheme_multiple_array} is never re-used if
@cpp{scheme_detach_multiple_array} is called).

The @cpp{scheme_multiple_count} and
@cpp{scheme_multiple_array} variables only contain meaningful values
when @cpp{scheme_multiple_values} is returned.

@; ----------------------------------------------------------------------

@section{Evaluation Functions}

@function[(Scheme_Object* scheme_eval
           [Scheme_Object* expr]
           [Scheme_Env* env])]{

Evaluates the (uncompiled) S-expression @var{expr} in the namespace
@var{env}.}

@function[(Scheme_Object* scheme_eval_compiled
           [Scheme_Object* obj]
           [Scheme_Env* env])]{

Evaluates the compiled expression @var{obj}, which was previously
returned from @cpp{scheme_compile}, first linking to the namespace @var{env}.}

@function[(Scheme_Object* scheme_eval_compiled_multi
           [Scheme_Object* obj]
           [Scheme_Env* env])]{

Evaluates the compiled expression @var{obj}, possibly
returning multiple values (see @secref["multiple"]).}

@function[(Scheme_Object* _scheme_eval_compiled
           [Scheme_Object* obj]
           [Scheme_Env* env])]{

Non-top-level version of @cpp{scheme_eval_compiled}. (See @secref["topleveleval"].)}

@function[(Scheme_Object* _scheme_eval_compiled_multi
           [Scheme_Object* obj]
           [Scheme_Env* env])]{

Non-top-level version of @cpp{scheme_eval_compiled_multi}. (See @secref["topleveleval"].)}


@function[(Scheme_Env* scheme_basic_env)]{

Creates the main namespace for an embedded Racket. This procedure
must be called before other Racket library function (except
@cpp{scheme_make_param}). Extensions to Racket cannot call this
function.

If it is called more than once, this function resets all threads
(replacing the main thread), parameters, ports, namespaces, and
finalizations.}

@function[(Scheme_Object* scheme_make_namespace
           [int argc]
           [Scheme_Object** argv])]{

Creates and returns a new namespace. This values can be cast to
@cpp{Scheme_Env *}. It can also be installed in
a parameterization using @cppi{scheme_set_param} with
@cppi{MZCONFIG_ENV}.

When Racket is embedded in an application, create the initial
namespace with @cppi{scheme_basic_env} before calling this procedure
to create new namespaces.}


@function[(Scheme_Object* scheme_apply
           [Scheme_Object* f]
           [int c]
           [Scheme_Object** args])]{

Applies the procedure @var{f} to the given arguments.

Beware that the procedure can mutate @var{args} if it is the same as
the result of @cpp{scheme_current_argument_stack}.}

@function[(Scheme_Object* scheme_apply_multi
           [Scheme_Object* f]
           [int c]
           [Scheme_Object** args])]{

Applies the procedure @var{f} to the given arguments, possibly
returning multiple values (see @secref["multiple"]).}


@function[(Scheme_Object* _scheme_apply
           [Scheme_Object* f]
           [int c]
           [Scheme_Object** args])]{

Non-top-level version of @cpp{scheme_apply}. (See @secref["topleveleval"].)}

@function[(Scheme_Object* _scheme_apply_multi
           [Scheme_Object* f]
           [int c]
           [Scheme_Object** args])]{

Non-top-level version of @cpp{scheme_apply_multi}. (See @secref["topleveleval"].)}


@function[(Scheme_Object* scheme_apply_to_list
           [Scheme_Object* f]
           [Scheme_Object* args])]{

Applies the procedure @var{f} to the list of arguments in @var{args}.}

@function[(Scheme_Object* scheme_eval_string
           [char* str]
           [Scheme_Env* env])]{

Reads a single S-expression from @var{str} and evaluates it in the given
namespace; the expression must return a single value, otherwise an
exception is raised. The @var{str} argument is parsed as a
UTF-8-encoded string of Unicode characters (so plain ASCII is fine).}

@function[(Scheme_Object* scheme_eval_string_multi
           [char* str]
           [Scheme_Env* env])]{

Like @cpp{scheme_eval_string}, but returns
 @cpp{scheme_multiple_values} when the expression returns multiple
 values.}

@function[(Scheme_Object* scheme_eval_string_all
           [char* str]
           [Scheme_Env* env]
           [int all])]{

Like @cpp{scheme_eval_string}, but if @var{all} is not @cpp{0}, then
 expressions are read and evaluated from @var{str} until the end of
 the string is reached.}

@function[(Scheme_Object* scheme_tail_apply
           [Scheme_Object* f]
           [int n]
           [Scheme_Object** args])]{

Applies the procedure as a tail-call. Actually, this function just
registers the given application to be invoked when control returns to
the evaluation loop. (Hence, this function is only useful within a
primitive procedure that is returning to its caller.)}

@function[(Scheme_Object* scheme_tail_apply_no_copy
           [Scheme_Object* f]
           [int n]
           [Scheme_Object** args])]{

Like @cpp{scheme_tail_apply}, but the array @var{args} is not
copied. Use this only when @var{args} has infinite extent and will not
be used again, or when @var{args} will certainly not be used again
until the called procedure has returned.}

@function[(Scheme_Object* scheme_tail_apply_to_list
           [Scheme_Object* f]
           [Scheme_Object* l])]{

Applies the procedure as a tail-call.} 

@function[(Scheme_Object* scheme_compile
           [Scheme_Object* form]
           [Scheme_Env* env]
           [int writable])]{

Compiles the S-expression @var{form} in the given namespace. The
returned value can be used with @cpp{scheme_eval_compiled} et al.
Provide a non-zero value for @var{writable} if the resulting compiled
object will be marshalled via @racket[write] instead of evaluated.}

@function[(Scheme_Object* scheme_expand
           [Scheme_Object* form]
           [Scheme_Env* env])]{

Expands all macros in the S-expression @var{form} using the given
namespace.}

@function[(Scheme_Object* scheme_values
           [int n]
           [Scheme_Object** args])]{

Returns the given values together as multiple return values. Unless
@var{n} is @cpp{1}, the result will always be
@cpp{scheme_multiple_values}.}

@function[(void scheme_detach_multiple_array
           [Scheme_Object** args])]{

Called to receive multiple-value results; see @secref["multiple"].}
