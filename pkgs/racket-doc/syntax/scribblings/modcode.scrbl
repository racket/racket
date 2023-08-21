#lang scribble/doc
@(require "common.rkt" (for-label syntax/modcode))

@title[#:tag "modcode"]{Getting Module Compiled Code}

@defmodule[syntax/modcode]

@defproc[(get-module-code [path path-string?]
                          [#:submodule-path submodule-path (listof symbol?) '()]
                          [#:sub-path compiled-subdir0
                           (or/c (and/c path-string? relative-path?)
                                 (listof (and/c path-string? relative-path?)))
                           (use-compiled-file-paths)]
                          [compiled-subdir
                           (or/c (and/c path-string? relative-path?)
                                 (listof (and/c path-string? relative-path?)))
                           compiled-subdir0]
                          [#:roots roots (listof (or/c path-string? 'same)) (current-compiled-file-roots)]
                          [#:compile compile-proc0 (any/c . -> . any) compile] 
                          [compile-proc (any/c . -> . any) compile-proc0] 
                          [#:extension-handler ext-proc0 (or/c false/c (path? boolean? . -> . any)) #f]
                          [ext-proc (or/c false/c (path? boolean? . -> . any)) ext-proc0]
                          [#:notify notify-proc (any/c . -> . any) void]
                          [#:source-reader read-syntax-proc 
                                        (any/c input-port? . -> . (or/c syntax? eof-object?)) 
                                        read-syntax]
                          [#:rkt-try-ss? rkt-try-ss? boolean? #t]
                          [#:choose choose-proc
                           (or/c (-> path? path? path?
                                     (or/c 'src 'zo 'so #f))
                                 #f)
                           #f])
         any]{

Returns a compiled expression for the declaration of the module
specified by @racket[path] and @racket[submodule-path], where
@racket[submodule-path] is empty for a root module or a list for a
submodule.

The @racket[roots], @racket[compiled-subdir], @racket[choose-proc], and
@racket[rkt-try-ss?] and @racket[submodule-path] arguments determine which file is consulted
to find the compiled code. If the default values are provided,
then this function uses the same logic as the default
value of @racket[current-load/use-compiled]. In more detail:
 @itemlist[
 @item{If @racket[submodule-path] is not the empty list, then the compiled code
   will never be located in a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dynamic extension};
   instead the original source or a @filepath{zo} file will be used.}
 @item{The @racket[rkt-try-ss?] argument defaults to @racket[#t].  If it is not
   @racket[#f], then if @racket[path] ends in @filepath{.rkt}, then the
   corresponding file ending in @filepath{.ss} will be tried as well.}
 @item{The @racket[choose-proc] argument is called with the
   original source file (which might have had its ending
   changed, c.f. the @racket[rkt-try-ss?] argument) and two
   other paths that end with @filepath{zo} and @filepath{so}
   (but they are not necessarily the paths that
   @racket[get-module-code] uses). If the @racket[choose-proc]
   returns @racket['src], then compiled files are not used. If
   it returns any other result, the result is ignored. In
   previous versions of this function, the @racket[choose-proc] offered more control over
   which file was used but it no longer does;
   the current interface is kept for backwards compatibility.}
 @item{
   The @racket[compiled-subdir] argument defaults to @racket[(use-compiled-file-paths)];
   it specifies the sub-directories to search for a compiled version of the
   module. If @racket[compiled-subdir] is a list, then the first directory
   that contains a file with an appropriate name is used as the compiled file.}
 @item{The @racket[roots] list specifies a compiled-file search path
   in the same way as the @racket[current-compiled-file-roots] parameter;
   it defaults to the current value of @racket[current-compiled-file-roots].}]

The @racket[compile-proc] argument defaults to @racket[compile]. This
procedure is used to compile module source if an already-compiled
version is not available. If @racket[submodule-path] is not @racket['()],
then @racket[compile-proc] must return a compiled module form.

The @racket[ext-proc] argument defaults to @racket[#f]. If it is not
@racket[#f], it must be a procedure of two arguments that is called
when a native-code version of @racket[path] should be used. In that
case, the arguments to @racket[ext-proc] are the path for the
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dynamic extension},
and a boolean indicating whether the extension is a @tt{_loader}
file (@racket[#t]) or not (@racket[#f]).

If a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dynamic extension}
is preferred or is the only file that exists, it is
supplied to @racket[ext-proc] when @racket[ext-proc] is @racket[#f],
or an exception is raised (to report that an extension file cannot be
used) when @racket[ext-proc] is @racket[#f].

If @racket[notify-proc] is supplied, it is called for the file
(source, @filepath{.zo} or
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dynamic extension})
that is chosen.

If @racket[read-syntax-proc] is provided, it is used to read the
module from a source file (but not from a bytecode file).

@history[#:changed "6.90.0.7" @elem{Use @racket[(default-compiled-sub-path)]
                                    for the default value of @racket[compiled-subdir].}
         #:changed "8.6.0.12" @list{Generalize the @racket[#:sub-path] argument,
           change @racket[#:sub-path]'s default to @racket[(use-compiled-file-paths)],
           and pay less attention to the @racket[#:choose] argument.}]}

@defproc[(get-module-path [path path-string?]
                          [#:submodule? submodule? boolean?]
                          [#:sub-path compiled-subdir0
                           (or/c (and/c path-string? relative-path?)
                                 (listof (and/c path-string? relative-path?)))
                           (use-compiled-file-paths)]
                          [compiled-subdir
                           (or/c (and/c path-string? relative-path?)
                                 (listof (and/c path-string? relative-path?)))
                           compiled-subdir0]
                          [#:roots roots (listof (or/c path-string? 'same)) (current-compiled-file-roots)]
                          [#:rkt-try-ss? rkt-try-ss? boolean? #t]
                          [#:choose choose-proc any/c #f])
         (values path? (or/c 'src 'zo 'so))]{

Produces two values.  The first is the path of the latest source or compiled
file for the module specified by @racket[path]; this result is the path of the
file that @racket[get-module-code] would read to produce a compiled module
expression.  The second value is @racket['src], @racket['zo], or @racket['so],
depending on whether the first value represents a Racket source file, a
compiled bytecode file, or a native library file.

The @racket[compiled-subdir], @racket[roots], @racket[choose-proc], and
@racket[rkt-try-ss?] arguments are interpreted the
same as by @racket[get-module-code].

The @racket[submodule?] argument represents whether the desired module is a
submodule of the one specified by @racket[path].  When @racket[submodule?] is
true, the result path never refers to a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dynamic extension}
and the result symbol is never @racket['so], as native libraries cannot
provide submodules.

@history[#:changed "6.90.0.7" @elem{Use @racket[(default-compiled-sub-path)]
                                    for the default value of @racket[compiled-subdir].}
         #:changed "8.6.0.12" @list{Generalize the @racket[#:sub-path] argument,
           change @racket[#:sub-path]'s default to @racket[(use-compiled-file-paths)],
           and pay less attention to the @racket[#:choose] argument.}]}


@defproc[(default-compiled-sub-path) path-string?]{

If @racket[(use-compiled-file-paths)] is not @racket['()], returns the
first element of the list. Otherwise, returns @racket["compiled"].

This function used to provide the default for the @racket[#:sub-path]
argument to @racket[get-module-code] and @racket[get-module-path], but
it is no longer used by this library.

@history[#:added "6.90.0.7"]}


@defproc[(get-metadata-path [path path-string?]
                            [#:roots roots (listof (or/c path-string? 'same))
                                           (current-compiled-file-roots)]
                            [sub-path (or/c path-string? 'same)]
                            ...+)
         path?]{

Constructs the path used to store compilation metadata for a source file stored
in the directory @racket[path].  The argument @racket[roots] specifies the
possible root directories to consider and to search for an existing file.  The
@racket[sub-path] arguments specify the subdirectories and filename of the
result relative to the chosen root.  For example, the compiled @filepath{.zo}
file for @filepath{/path/to/source.rkt} might be stored in
@racket[(get-metadata-path (build-path "/path/to") "compiled" "source_rkt.zo")].

}

@defparam[moddep-current-open-input-file proc (path-string? . -> . input-port?)]{

A parameter whose value is used like @racket[open-input-file] to read
a module source or @filepath{.zo} file.}

@defstruct[(exn:get-module-code exn:fail) ([path path?])]{

An exception structure type for exceptions raised by
@racket[get-module-code].}
