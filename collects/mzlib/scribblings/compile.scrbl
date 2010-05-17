#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/compile
                     compiler/compiler
                     compiler/cm))

@mzlib[#:mode title compile]

@defproc[(compile-file [src path-string?]
                       [dest path-string? (let-values ([(base name dir?) (split-path src)])
                                            (build-path base "compiled"
                                                        (path-add-suffix name #".zo")))]
                       [filter (any/c . -> . any/c) values])
         path?]{

Compiles the Scheme file @scheme[src] and saves the compiled code to
@scheme[dest].  If @scheme[dest] is not provided and the
@filepath{compiled} subdirectory does not already exist, the
subdirectory is created. The result of @scheme[compile-file] is the
destination file's path.

If the @scheme[filter] procedure is provided, it is applied to each
source expression, and the result is compiled. 

The @scheme[compile-file] procedure is designed for compiling modules
files, in that each expression in @scheme[src] is compiled
independently. If @scheme[src] does not contain a single
@scheme[module] expression, then earlier expressions can affect the
compilation of later expressions when @scheme[src] is loaded
directly. An appropriate @scheme[filter] can make compilation behave
like evaluation, but the problem is also solved (as much as possible)
by the @scheme[compile-zos] procedure.

See also @scheme[managed-compile-zo].}
