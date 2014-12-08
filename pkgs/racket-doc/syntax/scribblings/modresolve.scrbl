#lang scribble/doc
@(require "common.rkt" (for-label syntax/modresolve))

@title{Resolving Module Paths to File Paths}

@defmodule[syntax/modresolve]

@defproc[(resolve-module-path [module-path-v module-path?] 
                              [rel-to-path-v (or/c path-string? (-> any) false/c)])
         (or/c path? symbol?
               (cons/c 'submod (cons/c (or/c path? symbol?) (listof symbol?))))]{

Resolves a module path to filename path. The module path is resolved
relative to @racket[rel-to-path-v] if it is a path string (assumed to
be for a file), to the directory result of calling the thunk if it is
a thunk, or to the current directory otherwise.}

@defproc[(resolve-module-path-index [module-path-index module-path-index?] 
                                    [rel-to-path-v (or/c path-string? (-> any) false/c)])
         (or/c path? symbol?
               (cons/c 'submod (cons/c (or/c path? symbol?) (listof symbol?))))]{

Like @racket[resolve-module-path] but the input is a @techlink[#:doc
refman]{module path index}; in this case, the @racket[rel-to-path-v]
base is used where the module path index contains the ``self'' index.
If @racket[module-path-index] depends on the ``self'' module path
index, then an exception is raised unless @racket[rel-to-path-v] is a
path string.}
