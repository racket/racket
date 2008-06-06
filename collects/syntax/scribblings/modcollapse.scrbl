#lang scribble/doc
@(require "common.ss"
          (for-label syntax/modcollapse))

@title[#:tag "modcollapse"]{Simplifying Module Paths}

@defmodule[syntax/modcollapse]

@defproc[(collapse-module-path [module-path-v module-path?]
                               [rel-to-module-path-v any/c]) 
         (or/c path? module-path?)]{

Returns a ``simplified'' module path by combining
@scheme[module-path-v] with @scheme[rel-to-module-path-v], where the
latter must have the form @scheme['(lib ....)] or a symbol,
@scheme['(file <string>)], @scheme['(path ....)], a @techlink[#:doc
refman]{path}, or a thunk to generate one of those.

The result can be a path if @scheme[module-path-v] contains a path
element that is needed for the result, or if
@scheme[rel-to-module-path-v] is a non-string path that is needed for
the result; otherwise, the result is a module path in the sense of
@scheme[module-path?].

When the result is a @scheme['lib] or @scheme['planet] module path, it
is normalized so that equivalent module paths are represented by
@scheme[equal?] results.}

@defproc[(collapse-module-path-index [module-path-index module-path-index?]
                                     [rel-to-module-path-v any/c])
         (or/c path? module-path?)]{

Like @scheme[collapse-module-path], but the input is a @techlink[#:doc
refman]{module path index}; in this case, the
@scheme[rel-to-module-path-v] base is used where the module path index
contains the ``self'' index.}
