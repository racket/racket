#lang scribble/doc
@(require "utils.rkt")

@title{Flags and Hooks}

The following flags and hooks are available when Racket is
embedded:

@itemize[

 @item{@cppdef{scheme_exit} --- This pointer can be set to a function
 that takes an integer argument and returns @cpp{void}; the function
 will be used as the default exit handler. The default is @cpp{NULL}.}

 @item{@cppdef{scheme_make_stdin}, @cppdef{scheme_make_stdout},
 @cppdef{scheme_make_stderr}, --- These pointers can be set to a
 function that takes no arguments and returns a Racket port
 @cpp{Scheme_Object *} to be used as the starting standard input,
 output, and/or error port. The defaults are @cpp{NULL}. Setting the
 initial error port is particularly important for seeing unexpected
 error messages if @cpp{stderr} output goes nowhere.}

 @item{@cppdef{scheme_console_output} --- This pointer can be set to a
 function that takes a string and a @cpp{intptr_t} string length; the
 function will be called to display internal Racket warnings and
 messages that possibly contain non-terminating nuls. The default is
 @var{NULL}.}

 @item{@cppdef{scheme_check_for_break} --- This points to a function
 of no arguments that returns an integer. It is used as the default
 user-break polling procedure in the main thread. A non-zero return
 value indicates a user break, and each time the function returns a
 non-zero value, it counts as a new break signal (though the break
 signal may be ignored if a previous signal is still pending).  The
 default is @cpp{NULL}.}

 @item{@cppdef{scheme_case_sensitive} --- If this flag is set to a
 non-zero value before @cppi{scheme_basic_env} is called, then
 Racket will not ignore capitalization for symbols and global
 variable names.  The value of this flag should not change once it is
 set. The default is zero.}

 @item{@cppdef{scheme_allow_set_undefined} --- This flag determines
 the initial value of @racket[compile-allow-set!-undefined]. The default
 is zero.}

 @item{@cppdef{scheme_console_printf} --- This function pointer was
 left for backward compatibility.  The default builds a string and
 calls @cppi{scheme_console_output}.}

]

@function[(void scheme_set_collects_path
           [Scheme_Object* path])]{

Sets the path to be returned by @racket[(find-system-path
'collects-dir)].}


@function[(void scheme_set_addon_path
           [Scheme_Object* path])]{

Sets the path to be returned by @racket[(find-system-path
'addon-dir)].}


@function[(void scheme_set_exec_cmd
                [const-char* path])]{

Sets the path to be returned by @racket[(find-system-path
'exec-file)].}


@function[(void scheme_init_collection_paths_post
           [Scheme_Env* env]
           [Scheme_Object* pre_extra_paths]
           [Scheme_Object* post_extra_paths])]{

Initializes the @racket[current-library-collection-paths] parameter
using @racket[find-library-collection-paths]. The
@var{pre_extra_paths} and @var{post_extra-paths} arguments are
propagated to @racket[find-library-collection-paths].

The function calls @cpp{scheme_seal_parameters} automatically.}

@function[(void scheme_init_collection_paths
           [Scheme_Env* env]
           [Scheme_Object* pre_extra_paths])]{

Like @cpp{scheme_init_collection_paths_post}, but with @racket[null]
as the last argument.}


@function[(void scheme_seal_parameters)]{

Takes a snapshot of the current values of built-in parameters. These
values are used for privileged actions, such as installing a @|PLaneT|
package.}
