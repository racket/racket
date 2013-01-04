#lang scribble/doc
@(require "common.rkt" (for-label syntax/modcode))

@title[#:tag "modcode"]{Getting Module Compiled Code}

@defmodule[syntax/modcode]

@defproc[(get-module-code [path path?]
                          [#:submodule-path submodule-path (listof symbol?) '()]
                          [#:sub-path compiled-subdir0 (and/c path-string? relative-path?) "compiled"]
                          [compiled-subdir (and/c path-string? relative-path?) compiled-subdir0]
                          [#:roots roots (listof (or/c path-string? 'same)) (current-compiled-file-roots)]
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
                                        (any/c input-port? . -> . (or/c syntax? eof-object?)) 
                                        read-syntax])
         any]{

Returns a compiled expression for the declaration of the module
specified by @racket[path] and @racket[submodule-path], where
@racket[submodule-path] is empty for a root module or a list for a
submodule.

The @racket[compiled-subdir] argument defaults to @racket["compiled"];
it specifies the sub-directory to search for a compiled version of the
module. The @racket[roots] list specifies a compiled-file search path
in the same way as the @racket[current-compiled-file-roots] parameter.

The @racket[compile-proc] argument defaults to @racket[compile]. This
procedure is used to compile module source if an already-compiled
version is not available. If @racket[submodule-path] is not @racket['()],
then @racket[compile-proc] must return a compiled module form.

The @racket[ext-proc] argument defaults to @racket[#f]. If it is not
@racket[#f], it must be a procedure of two arguments that is called
when a native-code version of @racket[path] should be used. In that
case, the arguments to @racket[ext-proc] are the path for the
extension, and a boolean indicating whether the extension is a @tt{_loader}
file (@racket[#t]) or not (@racket[#f]).

The @racket[choose-proc] argument is a procedure that takes three
paths: a source path, a @filepath{.zo} file path, and an extension path
(for a non-@tt{_loader} extension). Some of the paths may not
exist. The result should be either @racket['src], @racket['zo],
@racket['so], or @racket[#f], indicating which variant should be used
or (in the case of @racket[#f]) that the default choice should be
used.

The default choice is computed as follows: if a @filepath{.zo} version
of @racket[path] is available and newer than @racket[path] itself (in
one of the directories specified by @racket[compiled-subdir]), then it
is used instead of the source. Native-code versions of @racket[path]
are ignored, unless only a native-code non-@tt{_loader} version exists
(i.e., @racket[path] itself does not exist). A @tt{_loader} extension
is selected a last resort.

If an extension is preferred or is the only file that exists, it is
supplied to @racket[ext-proc] when @racket[ext-proc] is @racket[#f],
or an exception is raised (to report that an extension file cannot be
used) when @racket[ext-proc] is @racket[#f].

If @racket[notify-proc] is supplied, it is called for the file
(source, @filepath{.zo} or extension) that is chosen.

If @racket[read-syntax-proc] is provided, it is used to read the
module from a source file (but not from a bytecode file).}

@defparam[moddep-current-open-input-file proc (path-string? . -> . input-port?)]{

A parameter whose value is used like @racket[open-input-file] to read
a module source or @filepath{.zo} file.}


@defstruct[(exn:get-module-code exn:fail) ([path path?])]{

An exception structure type for exceptions raised by
@racket[get-module-code].}
