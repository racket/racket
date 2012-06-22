#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "exceptions"]{Exceptions and Escape Continuations}

When Racket encounters an error, it raises an exception. The default
exception handler invokes the error display handler and then the error
escape handler. The default error escape handler escapes via a
@defterm{primitive error escape}, which is implemented by calling
@cpp{scheme_longjmp(*scheme_current_thread->error_buf)}.

An embedding program should install a fresh buffer into
@cpp{scheme_current_thread->error_buf} and call
@cpp{scheme_setjmp(*scheme_current_thread->error_buf)} before any
top-level entry into Racket evaluation to catch primitive error
escapes. When the new buffer goes out of scope, restore the original
in @cpp{scheme_current_thread->error_buf}. The macro
@cppi{scheme_error_buf} is a shorthand for
@cpp{*scheme_current_thread->error_buf}.

@verbatim[#:indent 2]{
  mz_jmp_buf * volatile save, fresh;
  ...
  save = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &fresh;
  if (scheme_setjmp(scheme_error_buf)) {
    /* There was an error */
    ...
  } else {
    v = scheme_eval_string(s, env);
  }
  scheme_current_thread->error_buf = save;
  ...
}

3m: when @cpp{scheme_setjmp} is used, the enclosing context must
provide a local-variable registration record via @cpp{MZ_GC_DECL_REG}.
Use @cpp{MZ_GC_DECL_REG(0)} if the context has no local variables to
register. Unfortunately, when using @DFlag{xform} with @|mzc| instead
of @cpp{MZ_GC_DECL_REG}, etc., you may need to declare a dummy pointer
and use it after @cpp{scheme_setjmp} to ensure that a local-variable
registration is generated.

New primitive procedures can raise a generic exception by calling
@cppi{scheme_signal_error}. The arguments for
@cpp{scheme_signal_error} are roughly the same as for the standard C
function @cpp{printf}. A specific primitive exception can be raised by
calling @cppi{scheme_raise_exn}.

Full @as-index{continuations} are implemented in Racket by copying
the C stack and using @cppi{scheme_setjmp} and @cppi{scheme_longjmp}.
As long a C/C++ application invokes Racket evaluation through the
top-level evaluation functions (@cpp{scheme_eval}, @cpp{scheme_apply},
etc., as opposed to @cpp{_scheme_apply}, @cpp{_scheme_eval_compiled},
etc.), the code is protected against any unusual behavior from Racket
evaluations (such as returning twice from a function) because
continuation invocations are confined to jumps within a single
top-level evaluation. However, escape continuation jumps are still
allowed; as explained in the following sub-section, special care must
be taken in extension that is sensitive to escapes.

@; ----------------------------------------------------------------------

@section[#:tag "imz:tempcatch"]{Temporarily Catching Error Escapes}

When implementing new primitive procedure, it is sometimes useful to
catch and handle errors that occur in evaluating subexpressions. One
way to do this is the following: save
@cppi{scheme_current_thread->error_buf} to a temporary variable, set
@cppi{scheme_current_thread->error_buf} to the address of a
stack-allocated @cpp{mz_jmp_buf}, invoke
@cpp{scheme_setjmp(scheme_error_buf)}, perform the function's work,
and then restore @cpp{scheme_current_thread->error_buf} before
returning a value. (3m: A stack-allocated @cpp{mz_jmp_buf} instance
need not be registered with the garbage collector, and a
heap-allocated @cpp{mz_jmp_buf} should be allocated as atomic.)

However, beware that a prompt abort or the invocation of an escaping
continuation looks like a primitive error escape. In that case, the
special indicator flag @cppi{scheme_jumping_to_continuation} is
non-zero (instead of its normal zero value); this situation is only
visible when implementing a new primitive procedure. When
@cppi{scheme_jumping_to_continuation} is non-zero, honor the escape
request by chaining to the previously saved error buffer; otherwise,
call @cppi{scheme_clear_escape}.

@verbatim[#:indent 2]{
  mz_jmp_buf * volatile save, fresh;
  save = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &fresh;
  if (scheme_setjmp(scheme_error_buf)) {
    /* There was an error or continuation invocation */
    if (scheme_jumping_to_continuation) {
      /* It was a continuation jump */
      scheme_longjmp(*save, 1);
      /* To block the jump, instead: scheme_clear_escape(); */
    } else {
      /* It was a primitive error escape */
    }
  } else {
    scheme_eval_string("x", scheme_env);
  }
  scheme_current_thread->error_buf = save;
}

This solution works fine as long as the procedure implementation only
calls top-level evaluation functions (@cpp{scheme_eval},
@cpp{scheme_apply}, etc., as opposed to @cpp{_scheme_apply},
@cpp{_scheme_eval_compiled}, etc.).  Otherwise, use
@cppi{scheme_dynamic_wind} to protect your code against full
continuation jumps in the same way that @racket[dynamic-wind] is used
in Racket.

The above solution simply traps the escape; it doesn't report the
reason that the escape occurred. To catch exceptions and obtain
information about the exception, the simplest route is to mix Racket
code with C-implemented thunks. The code below can be used to catch
exceptions in a variety of situations. It implements the function
@cpp{_apply_catch_exceptions}, which catches exceptions during the
application of a thunk. (This code is in
@filepath{collects/mzscheme/examples/catch.c} in the distribution.)

@verbatim[#:indent 2]{
  static Scheme_Object *exn_catching_apply, *exn_p, *exn_message;

  static void init_exn_catching_apply()
  {
    if (!exn_catching_apply) {
      char *e = 
        "(lambda (thunk) "
          "(with-handlers ([void (lambda (exn) (cons #f exn))]) "
            "(cons #t (thunk))))";
      /* make sure we have a namespace with the standard bindings: */
      Scheme_Env *env = (Scheme_Env *)scheme_make_namespace(0, NULL);

      scheme_register_extension_global(&exn_catching_apply, 
                                       sizeof(Scheme_Object *));
      scheme_register_extension_global(&exn_p, 
                                       sizeof(Scheme_Object *));
      scheme_register_extension_global(&exn_message, 
                                       sizeof(Scheme_Object *));

      exn_catching_apply = scheme_eval_string(e, env);
      exn_p = scheme_lookup_global(scheme_intern_symbol("exn?"), env);
      exn_message 
        = scheme_lookup_global(scheme_intern_symbol("exn-message"), 
                               env);
    }
  }

  /* This function applies a thunk, returning the Racket value if 
     there's no exception, otherwise returning NULL and setting *exn 
     to the raised value (usually an exn structure). */
  Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, 
                                               Scheme_Object **exn)
  {
    Scheme_Object *v;

    init_exn_catching_apply();

    v = _scheme_apply(exn_catching_apply, 1, &f);
    /* v is a pair: (cons #t value) or (cons #f exn) */

    if (SCHEME_TRUEP(SCHEME_CAR(v)))
      return SCHEME_CDR(v);
    else {
      *exn = SCHEME_CDR(v);
      return NULL;
    }
  }

  Scheme_Object *extract_exn_message(Scheme_Object *v)
  {
    init_exn_catching_apply();

    if (SCHEME_TRUEP(_scheme_apply(exn_p, 1, &v)))
      return _scheme_apply(exn_message, 1, &v);
    else
      return NULL; /* Not an exn structure */
  }
}

In the following example, the above code is used to catch exceptions
that occur during while evaluating source code from a string.

@verbatim[#:indent 2]{
  static Scheme_Object *do_eval(void *s, int noargc, 
                                Scheme_Object **noargv)
  {
    return scheme_eval_string((char *)s, 
                              scheme_get_env(scheme_config));
  }

  static Scheme_Object *eval_string_or_get_exn_message(char *s)
  {
    Scheme_Object *v, *exn;

    v = scheme_make_closed_prim(do_eval, s);
    v = _apply_thunk_catch_exceptions(v, &exn);
    /* Got a value? */
    if (v)
      return v;

    v = extract_exn_message(exn);
    /* Got an exn? */
    if (v)
      return v;

    /* `raise' was called on some arbitrary value */
    return exn;
  }
}

@; ----------------------------------------------------------------------

@section{Enabling and Disabling Breaks}

When embedding Racket, asynchronous break exceptions are disabled by
default. Call @cpp{scheme_set_can_break} (which is the same as calling
the Racket function @racket[break-enabled]) to enable or disable
breaks. To enable or disable breaks during the dynamic extent of
another evaluation (where you would use
@racket[with-break-parameterization] in Racket), use
@cppi{scheme_push_break_enable} before and
@cppi{scheme_pop_break_enable} after, instead.

@section{Exception Functions}

@function[(void scheme_signal_error
           [char* msg]
           [... ...])]{

Raises a generic primitive exception. The parameters are roughly as
for @cpp{printf}, but with the following format directives:

@itemize[

 @item{@FormatD{c} : a Unicode character (of type @cpp{mzchar})}

 @item{@FormatD{d} : an @cpp{int}}

 @item{@FormatD{o} : an @cpp{int} formatted in octal}

 @item{@FormatD{gd} : a @cpp{long} integer}

 @item{@FormatD{gx} : a @cpp{long} integer formatted in hexadecimal}

 @item{@FormatD{ld} : an @cpp{intptr_t} integer}

 @item{@FormatD{lx} : an @cpp{intptr_t} integer formatted in hexadecimal}

 @item{@FormatD{f} : a floating-point @cpp{double}}

 @item{@FormatD{s} : a nul-terminated @cpp{char} string}

 @item{@FormatD{5} : a nul-terminated @cpp{mzchar} string}

 @item{@FormatD{S} : a Racket symbol (a @cpp{Scheme_Object*})}

 @item{@FormatD{t} : a @cpp{char} string with a @cpp{intptr_t} size (two
 arguments), possibly containing a non-terminating nul byte, and
 possibly without a nul-terminator}

 @item{@FormatD{u} : a @cpp{mzchar} string with a @cpp{intptr_t} size (two
 arguments), possibly containing a non-terminating nul character, and
 possibly without a nul-terminator}

 @item{@FormatD{T} : a Racket string (a @cpp{Scheme_Object*})}

 @item{@FormatD{q} : a string, truncated to 253 characters, with ellipses
 printed if the string is truncated}

 @item{@FormatD{Q} : a Racket string (a @cpp{Scheme_Object*}),
 truncated to 253 characters, with ellipses printed if the string is
 truncated}

 @item{@FormatD{V} : a Racket value  (a @cpp{Scheme_Object*}),
 truncated according to the current error print width.}

 @item{@FormatD{D} : a Racket value  (a @cpp{Scheme_Object*}),
 to @racket[display].}

 @item{@FormatD["@"] : a Racket value (a @cpp{Scheme_Object*}),
 that is a list whose printed elements are spliced into the result.}

 @item{@FormatD{e} : an @cpp{errno} value, to be printed as a text
 message.}

 @item{@FormatD{E} : a platform-specific error value, to be printed as a
 text message.}

 @item{@FormatD{Z} : a potential platform-specific error value and a
 @cpp{char} string; if the string is non-@cpp{NULL}, then the error
 value is ignored, otherwise the error value is used as for @FormatD{E}.}

 @item{@FormatD{%} : a percent sign}

 @item{@FormatD{_} : a pointer to ignore}

 @item{@FormatD{-} : an @cpp{int} to ignore}

]

The arguments following the format string must include no more than 25
strings and Racket values, 25 integers, and 25 floating-point
numbers. (This restriction simplifies the implementation with precise
garbage collection.)}

@function[(void scheme_raise_exn
           [int exnid]
           [... ...])]{

Raises a specific primitive exception. The @var{exnid} argument
specifies the exception to be raised. If an instance of that exception
has @math{n} fields, then the next @math{n-2} arguments are values for
those fields (skipping the @racket[message] and @racket[debug-info]
fields). The remaining arguments start with an error string and
proceed roughly as for @cpp{printf}; see @cpp{scheme_signal_error}
above for more details.

Exception ids are @cpp{#define}d using the same names as in Racket,
but prefixed with ``MZ'', all letters are capitalized, and all ``:'s',
``-''s, and ``/''s are replaced with underscores. For example,
@cpp{MZEXN_FAIL_FILESYSTEM} is the exception id for a filesystem
exception.}


@function[(void scheme_wrong_count
           [char* name]
           [int minc]
           [int maxc]
           [int argc]
           [Scheme_Object** argv])]{

This function is automatically invoked when the wrong number of
arguments are given to a primitive procedure.  It signals that the
wrong number of parameters was received and escapes (like
@cpp{scheme_signal_error}).  The @var{name} argument is the name of
the procedure that was given the wrong number of arguments; @var{minc}
is the minimum number of expected arguments; @var{maxc} is the maximum
number of expected arguments, or -1 if there is no maximum; @var{argc}
and @var{argv} contain all of the received arguments.}


@function[(void scheme_wrong_contract
           [char* name]
           [char* contract]
           [int which]
           [int argc]
           [Scheme_Object** argv])]{

Signals that an argument was received that does not satisfy a
contract and escapes (like @cpp{scheme_signal_error}).  The
@var{name} argument is the name of the procedure that was given the
wrong argument; @var{expected} is the contract; @var{which} is the
offending argument in the @var{argv} array; @var{argc} and @var{argv}
contain all of the received arguments. If the original @var{argc} and
@var{argv} are not available, provide -1 for @var{which} and a pointer
to the bad value in @var{argv}, in which case the magnitude (but not
sign) of @var{argc} is ignored. Negate @var{argc} if the exception
corresponds to a result contract instead of an argument contract.}


@function[(void scheme_wrong_type
           [char* name]
           [char* expected]
           [int which]
           [int argc]
           [Scheme_Object** argv])]{

Signals that an argument of the wrong type was received and
escapes. Use @cpp{scheme_wrong_contract}, instead.

The arguments are the same as for @cpp{scheme_wrong_contract},
except that @var{expected} is the name of the expected type.}


@function[(void scheme_wrong_return_arity
           [char* name]
           [int expected]
           [int got]
           [Scheme_Object** argv]
           [const-char* detail])]{

Signals that the wrong number of values were returned to a
multiple-values context. The @var{expected} argument indicates how
many values were expected, @var{got} indicates the number received,
and @var{argv} are the received values. The @var{detail} string can be
@cpp{NULL} or it can contain a @cpp{printf}-style string (with
additional arguments) to describe the context of the error; see
@cpp{scheme_signal_error} above for more details about the
@cpp{printf}-style string.}


@function[(void scheme_unbound_global
           [char* name])]{

Signals an unbound-variable error, where @var{name} is the name of the
variable.}


@function[(void scheme_contract_error
           [const-char* name]
           [const-char* msg]
           [... ...])]{

Raises a contract-violation exception. The @var{msg} string is static,
instead of a format string. After @var{msg}, any number of triples can
be provided to add fields (each on its own line) to the error message;
each triple is a string for the field name, a @cpp{0} or @cpp{1} to
indicate whether the field value is a literal string or a Racket
value, and either a literal string or a Racket value. The sequence of
field triples must be terminated with @cpp{NULL}.}


@function[(char* scheme_make_provided_string
           [Scheme_Object* o]
           [int count]
           [int* len])]{

Converts a Racket value into a string for the purposes of reporting an
error message. The @var{count} argument specifies how many Racket
values total will appear in the error message (so the string for this
value can be scaled appropriately). If @var{len} is not @cpp{NULL}, it
is filled with the length of the returned string.}


@function[(char* scheme_make_arg_lines_string
           [char* s]
           [int which]
           [int argc]
           [Scheme_Object** argv]
           [intptr_t* len])]{

Converts an array of Racket values into a byte string, skipping the
array element indicated by @var{which} if @var{which} is not -1. This
function is used to format the ``other'' arguments to a function when
one argument is bad (thus giving the user more information about the
state of the program when the error occurred).  If @var{len} is not
@cpp{NULL}, it is filled with the length of the returned string.

If the arguments are shown on multiple lines, then the result string
starts with a newline character and each line is indented by three
spaces. Otherwise, the result string starts with a space. If the
result would contain no arguments, it contains @litchar{[none]},
instead.}


@function[(char* scheme_make_args_string
           [char* s]
           [int which]
           [int argc]
           [Scheme_Object** argv]
           [intptr_t* len])]{

Like @cpp{scheme_make_arg_lines_string}, but for old-style messages
where the arguments are always shown within a single line. The result
does not include a leading space.}


@function[(void scheme_check_proc_arity
           [char* where]
           [int a]
           [int which]
           [int argc]
           [Scheme_Object** argv])]{

Checks the @var{which}th argument in @var{argv} to make sure it is a
procedure that can take @var{a} arguments. If there is an error, the
@var{where}, @var{which}, @var{argc}, and @var{argv} arguments are
passed on to @cpp{scheme_wrong_type}. As in @cpp{scheme_wrong_type},
@var{which} can be -1, in which case @cpp{*}@var{argv} is checked.}


@function[(Scheme_Object* scheme_dynamic_wind
           [Pre_Post_Proc pre]
           [Action_Proc action]
           [Pre_Post_Proc post]
           [Action_Proc jmp_handler]
           [void* data])]{

Evaluates calls the function @var{action} to get a value for the
 @cpp{scheme_dynamic_wind} call. The @cpp{Pre_Post_Proc} and
 @cpp{Action_Proc} types are not actually defined; instead the types
 are inlined as if they were defined as follows:

@verbatim[#:indent 2]{
typedef void (*Pre_Post_Proc)(void *data);
typedef Scheme_Object* (*Action_Proc)(void *data);
}

The functions @var{pre} and @var{post} are invoked when jumping into
 and out of @var{action}, respectively.

The function @var{jmp_handler} is called when an error is signaled (or
 an escaping continuation is invoked) during the call to @var{action};
 if @var{jmp_handler} returns @cpp{NULL}, then the error is passed on
 to the next error handler, otherwise the return value is used as the
 return value for the @cpp{scheme_dynamic_wind} call.

The pointer @var{data} can be anything; it is passed along in calls to
 @var{action}, @var{pre}, @var{post}, and @var{jmp_handler}.}


@function[(void scheme_clear_escape)]{

Clears the ``jumping to escape continuation'' flag associated with a
thread. Call this function when blocking escape continuation hops (see
the first example in @secref["imz:tempcatch"]).}


@function[(void scheme_set_can_break
           [int on])]{

Enables or disables breaks in the same way as
calling @racket[break-enabled].}


@function[(void scheme_push_break_enable
           [Scheme_Cont_Frame_Data* cframe]
           [int on]
           [int pre_check])]{

Use this function with @cpp{scheme_pop_break_enable} to enable or
disable breaks in the same way as
@racket[with-break-parameterization]; this function writes to
@var{cframe} to initialize it, and @cpp{scheme_pop_break_enable} reads
from @var{cframe}. If @var{pre_check} is non-zero and breaks are
currently enabled, any pending break exception is raised.}


@function[(void scheme_pop_break_enable
           [Scheme_Cont_Frame_Data* cframe]
           [int post_check])]{

Use this function with @cpp{scheme_push_break_enable}.  If
@var{post_check} is non-zero and breaks are enabled after restoring
the previous state, then any pending break exception is raised.}


@function[(Scheme_Object* scheme_current_continuation_marks
           [Scheme_Object* prompt_tag])]{

Like @racket[current-continuation-marks]. Passing @cpp{NULL} as
@var{prompt_tag} is the same as providing the default continuation
prompt tag.}


@function[(void scheme_warning
           [char* msg]
           [... ...])]{

Writes a warning message. The parameters are roughly as for
@cpp{printf}; see @cpp{scheme_signal_error} above for more details.

Normally, Racket's logging facilities should be used instead of this
function.}
