#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/include))

@mzlib[#:mode title include]

@deprecated[@racketmodname[racket/include]]{}

Similar to @racketmodname[scheme/include], but with a different syntax
for paths.

@defform/subs[#:literals (build-path lib up same)
              (include path-spec)
              ([path-spec string
                          (build-path elem ...+)
                          (lib file-string collection-string ...)]
               [elem string
                     up
                     same])]{

Inlines the syntax in the designated file in place of the
@racket[include] expression. The @racket[path-spec] can be any of the
following:

@itemize[

 @item{A literal string that specifies a path to include, parsed
       according to the platform's conventions (which means that it is
       not portable).}

 @item{A path construction of the form @racket[(build-path elem
       ...+)], where @racket[build-path] is
       @racket[module-identifier=?] either to the @racket[build-path]
       export from @racket[mzscheme] or to the top-level
       @racket[build-path], and where each @racket[elem] is a path
       string, @racket[up] (unquoted), or @racket[same] (unquoted).
       The @racket[elem]s are combined in the same way as for the
       @racket[build-path] function to obtain the path to include.}

 @item{A path construction of the form @racket[(lib file-string
       collection-string ...)], where @racket[lib] is free or refers
       to a top-level @racket[lib] variable.  The
       @racket[collection-string]s are passed to
       @racket[collection-path] to obtain a directory; if no
       @racket[collection-strings]s are supplied, @racket["mzlib"] is
       used. The @racket[file-string] is then appended to the
       directory using @racket[build-path] to obtain the path to
       include.}

]

If @racket[path-spec] specifies a relative path to include, the path
is resolved relative to the source for the @racket[include]
expression, if that source is a complete path string. If the source is
not a complete path string, then @racket[path-spec] is resolved
relative to the current load relative directory if one is available,
or to the current directory otherwise.

The included syntax is given the lexical context of the
@racket[include] expression.}

@deftogether[(
@defform[(include-at/relative-to context source path-spec)]
@defform[(include-at/relative-to/reader context source path-spec reader-expr)]
@defform[(include/reader path-spec reader-expr)]
)]{

Variants of @racket[include] analogous to the variants of
@racketmodname[scheme/include].}

