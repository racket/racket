#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "config"]{Parameterizations}

A @defterm{parameterization} is a set of parameter values. Each thread
has its own initial parameterization, which is extended functionally
and superseded by parameterizations that are attached to a particular
continuation mark.

Parameterization information is stored in a @cppi{Scheme_Config}
record. For the currently executing thread,
@cppi{scheme_current_config} returns the current parameterization.

To obtain parameter values, a @cpp{Scheme_Config} is combined with the
current threads @cpp{Scheme_Thread_Cell_Table}, as stored in the
thread record's @cpp{cell_values} field.

Parameter values for built-in parameters are obtained and modified
(for the current thread) using @cppi{scheme_get_param} and
@cppi{scheme_set_param}. Each parameter is stored as a
@cpp{Scheme_Object *} value, and the built-in parameters are accessed
through the following indices:

@itemize[
@item{@cppdef{MZCONFIG_ENV} --- @racket[current-namespace] (use @cpp{scheme_get_env})}
@item{@cppdef{MZCONFIG_INPUT_PORT} --- @racket[current-input-port]}
@item{@cppdef{MZCONFIG_OUTPUT_PORT} ---  @racket[current-output-port]}
@item{@cppdef{MZCONFIG_ERROR_PORT} ---  @racket[current-error-port]}

@item{@cppdef{MZCONFIG_ERROR_DISPLAY_HANDLER} --- @racket[error-display-handler]}
@item{@cppdef{MZCONFIG_ERROR_PRINT_VALUE_HANDLER} --- @racket[error-value->string-handler]}

@item{@cppdef{MZCONFIG_EXIT_HANDLER} --- @racket[exit-handler]}

@item{@cppdef{MZCONFIG_INIT_EXN_HANDLER} ---  @racket[uncaught-exception-handler]}

@item{@cppdef{MZCONFIG_EVAL_HANDLER} --- @racket[current-eval]}
@item{@cppdef{MZCONFIG_LOAD_HANDLER} --- @racket[current-load]}

@item{@cppdef{MZCONFIG_PRINT_HANDLER} --- @racket[current-print]}
@item{@cppdef{MZCONFIG_PROMPT_READ_HANDLER} --- @racket[current-prompt-read]}

@item{@cppdef{MZCONFIG_CAN_READ_GRAPH} --- @racket[read-accept-graph]}
@item{@cppdef{MZCONFIG_CAN_READ_COMPILED} --- @racket[read-accept-compiled]}
@item{@cppdef{MZCONFIG_CAN_READ_BOX} --- @racket[read-accept-box]}
@item{@cppdef{MZCONFIG_CAN_READ_PIPE_QUOTE} --- @racket[read-accept-bar-quote]}

@item{@cppdef{MZCONFIG_PRINT_GRAPH} --- @racket[print-graph]}
@item{@cppdef{MZCONFIG_PRINT_STRUCT} --- @racket[print-struct]}
@item{@cppdef{MZCONFIG_PRINT_BOX} --- @racket[print-box]}

@item{@cppdef{MZCONFIG_CASE_SENS} --- @racket[read-case-sensitive]}
@item{@cppdef{MZCONFIG_SQUARE_BRACKETS_ARE_PARENS} --- @racket[read-square-brackets-as-parens]}
@item{@cppdef{MZCONFIG_CURLY_BRACES_ARE_PARENS} --- @racket[read-curly-braces-as-parens]}

@item{@cppdef{MZCONFIG_ERROR_PRINT_WIDTH} --- @racket[error-print-width]}

@item{@cppdef{MZCONFIG_ALLOW_SET_UNDEFINED} --- @racket[allow-compile-set!-undefined]}

@item{@cppdef{MZCONFIG_CUSTODIAN} --- @racket[current-custodian]}

@item{@cppdef{MZCONFIG_USE_COMPILED_KIND} --- @racket[use-compiled-file-paths]}

@item{@cppdef{MZCONFIG_LOAD_DIRECTORY} --- @racket[current-load-relative-directory]}

@item{@cppdef{MZCONFIG_COLLECTION_PATHS} --- @racket[current-library-collection-paths]}

@item{@cppdef{MZCONFIG_PORT_PRINT_HANDLER} --- @racket[global-port-print-handler]}

@item{@cppdef{MZCONFIG_LOAD_EXTENSION_HANDLER} --- @racket[current-load-extension]}

]

To get or set a parameter value for a thread other than the current
one, use @cppi{scheme_get_thread_param} and
@cppi{scheme_set_thread_param}, each of which takes a
@cpp{Scheme_Thread_Cell_Table} to use in resolving or setting a
parameter value.

When installing a new parameter with @cpp{scheme_set_param}, no check
is performed on the supplied value to ensure that it is a legal value
for the parameter; this is the responsibility of the caller of
@cpp{scheme_set_param}. Note that Boolean parameters should only be
set to the values @racket[#t] and @racket[#f].

New primitive parameter indices are created with
@cppi{scheme_new_param} and implemented with
@cppi{scheme_make_parameter} and @cppi{scheme_param_config}.

@; ----------------------------------------------------------------------

@function[(Scheme_Object* scheme_get_param
           [Scheme_Config* config]
           [int param_id])]{

Gets the current value (for the current thread) of the parameter
 specified by @var{param_id}.}

@function[(Scheme_Object* scheme_set_param
           [Scheme_Config* config]
           [int param_id]
           [Scheme_Object* v])]{

Sets the current value (for the current thread) of the parameter
 specified by @var{param_id}.}

@function[(Scheme_Object* scheme_get_thread_param
           [Scheme_Config* config]
           [Scheme_Thread_Cell_Table* cells]
           [int param_id])]{

Like @cpp{scheme_get_param}, but using an arbitrary thread's
cell-value table.}

@function[(Scheme_Object* scheme_set_thread_param
           [Scheme_Config* config]
           [Scheme_Thread_Cell_Table* cells]
           [int param_id]
           [Scheme_Object* v])]{

Like @cpp{scheme_set_param}, but using an arbitrary thread's
 cell-value table.}

@function[(Scheme_Object* scheme_extend_config
           [Scheme_Config* base]
           [int param_id]
           [Scheme_Object* v])]{

Creates and returns a parameterization that extends @var{base} with a
 new value @var{v} (in all threads) for the parameter
 @var{param_id}. Use @cpp{scheme_install_config} to make this
 configuration active in the current thread.}

@function[(void scheme_install_config
           [Scheme_Config* config])]{

Adjusts the current thread's continuation marks to make @var{config}
 the current parameterization. Typically, this function is called
 after @cpp{scheme_push_continuation_frame} to establish a new
 continuation frame, and then @cpp{scheme_pop_continuation_frame}
 is called later to remove the frame (and thus the parameterization).}

@function[(Scheme_Thread_Cell_Table* scheme_inherit_cells
           [Scheme_Thread_Cell_Table* cells])]{

Creates a new thread-cell-value table, copying values for preserved
 thread cells from @var{cells}.}

@function[(int scheme_new_param)]{

Allocates a new primitive parameter index. This function must be
 called @italic{before} @cppi{scheme_basic_env}, so it is only
 available to embedding applications (i.e., not extensions).}

@function[(Scheme_Object* scheme_register_parameter
           [Scheme_Prim* function]
           [char* name]
           [int exnid])]{

Use this function instead of the other primitive-constructing
 functions, like @cpp{scheme_make_prim}, to create a primitive
 parameter procedure. See also @cpp{scheme_param_config}, below.
 This function is only available to embedding applications (i.e., not
 extensions).}

@function[(Scheme_Object* scheme_param_config
           [char* name]
           [Scheme_Object* param]
           [int argc]
           [Scheme_Object** argv]
           [int arity]
           [Scheme_Prim* check]
           [char* expected]
           [int isbool])]{

Call this procedure in a primitive parameter procedure to implement
 the work of getting or setting the parameter. The @var{name} argument
 should be the parameter procedure name; it is used to report
 errors. The @var{param} argument is a fixnum corresponding to the
 primitive parameter index returned by @cpp{scheme_new_param}.  The
 @var{argc} and @var{argv} arguments should be the un-touched and
 un-tested arguments that were passed to the primitive parameter.
 Argument-checking is performed within @cpp{scheme_param_config}
 using @var{arity}, @var{check}, @var{expected}, and @var{isbool}:

@itemize[

 @item{If @var{arity} is non-negative, potential parameter values must
 be able to accept the specified number of arguments. The @var{check}
 and @var{expected} arguments should be @cpp{NULL}.}

 @item{If @var{check} is not @cpp{NULL}, it is called to check a
 potential parameter value. The arguments passed to @var{check} are
 always @cpp{1} and an array that contains the potential parameter
 value. If @var{isbool} is @cpp{0} and @var{check} returns
 @cpp{scheme_false}, then a type error is reported using @var{name}
 and @var{expected}. If @var{isbool} is @cpp{1}, then a type error is
 reported only when @var{check} returns @cpp{NULL} and any
 non-@cpp{NULL} return value is used as the actual value to be stored
 for the parameter.}

 @item{Otherwise, @var{isbool} should be 1. A potential procedure
 argument is then treated as a Boolean value.}

]

 This function is only available to embedding applications (i.e., not
 extensions).}
