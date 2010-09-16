#lang scribble/doc
@(require "common.ss"
          (for-label syntax/modcode))

@title[#:tag "modcode"]{Getting Module Compiled Code}

@defmodule[syntax/modcode]

@defproc[(get-module-code [module-path-v module-path?]
                          [#:sub-path compiled-subdir0 (and/c path-string? relative-path?) "compiled"]
                          [compiled-subdir (and/c path-string? relative-path?) compiled-subdir0]
                          [#:compile compile-proc0 (any/c . -> . any) compile] 
                          [compile-proc (any/c . -> . any) compile-proc0] 
                          [#:extension-handler ext-proc0 (or/c false/c (path? boolean? . -> . any)) #f]
                          [ext-proc (or/c false/c (path? boolean? . -> . any)) ext-proc0]
                          [#:choose choose-proc 
                           (path? path? path? 
                            . -> . 
                            (or/c (symbols 'src 'zo 'so) false/c))
                           (lambda (src zo so) #f)]
                          [#:notify notify-proc (any/c . -> . any) void]
                          [#:source-reader read-syntax-proc 
                                        (any/c input-port? . -> . syntax?) 
                                        read-syntax])
         any]{

Returns a compiled expression for the declaration of the module
specified by @scheme[module-path-v].

The @scheme[compiled-subdir] argument defaults to @scheme["compiled"];
it specifies the sub-directory to search for a compiled version of the
module.

The @scheme[compile-proc] argument defaults to @scheme[compile]. This
procedure is used to compile module source if an already-compiled
version is not available.

The @scheme[ext-proc] argument defaults to @scheme[#f]. If it is not
@scheme[#f], it must be a procedure of two arguments that is called
when a native-code version of @scheme[path] is should be used. In that
case, the arguments to @scheme[ext-proc] are the path for the
extension, and a boolean indicating whether the extension is a @tt{_loader}
file (@scheme[#t]) or not (@scheme[#f]).

The @scheme[choose-proc] argument is a procedure that takes three
paths: a source path, a @filepath{.zo} file path, and an extension path
(for a non-@tt{_loader} extension). Some of the paths may not
exist. The result should be either @scheme['src], @scheme['zo],
@scheme['so], or @scheme[#f], indicating which variant should be used
or (in the case of @scheme[#f]) that the default choice should be
used.

The default choice is computed as follows: if a @filepath{.zo} version
of @scheme[path] is available and newer than @scheme[path] itself (in
one of the directories specified by @scheme[compiled-subdir]), then it
is used instead of the source. Native-code versions of @scheme[path]
are ignored, unless only a native-code non-@tt{_loader} version exists
(i.e., @scheme[path] itself does not exist). A @tt{_loader} extension
is selected a last resort.

If an extension is prefered or is the only file that exists, it is
supplied to @scheme[ext-proc] when @scheme[ext-proc] is @scheme[#f],
or an exception is raised (to report that an extension file cannot be
used) when @scheme[ext-proc] is @scheme[#f].

If @scheme[notify-proc] is supplied, it is called for the file
(source, @filepath{.zo} or extension) that is chosen.

If @scheme[read-syntax-proc] is provided, it is used to read the
module from a source file (but not from a bytecode file).}

@defparam[moddep-current-open-input-file proc (path-string? . -> . input-port?)]{

A parameter whose value is used like @scheme[open-input-file] to read
a module source or @filepath{.zo} file.}


@defstruct[(exn:get-module-code exn) ([path path?])]{

An exception structure type for exceptions raised by
@scheme[get-module-code].}
