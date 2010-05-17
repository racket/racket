#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/include))

@mzlib[#:mode title include]

Similar to @schememodname[scheme/include], but with a different syntax
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
@scheme[include] expression. The @scheme[path-spec] can be any of the
following:

@itemize[

 @item{A literal string that specifies a path to include, parsed
       according to the platform's conventions (which means that it is
       not portable).}

 @item{A path construction of the form @scheme[(build-path elem
       ...+)], where @scheme[build-path] is
       @scheme[module-identifier=?] either to the @scheme[build-path]
       export from @scheme[mzscheme] or to the top-level
       @scheme[build-path], and where each @scheme[elem] is a path
       string, @scheme[up] (unquoted), or @scheme[same] (unquoted).
       The @scheme[elem]s are combined in the same way as for the
       @scheme[build-path] function to obtain the path to include.}

 @item{A path construction of the form @scheme[(lib file-string
       collection-string ...)], where @scheme[lib] is free or refers
       to a top-level @scheme[lib] variable.  The
       @scheme[collection-string]s are passed to
       @scheme[collection-path] to obtain a directory; if no
       @scheme[collection-strings]s are supplied, @scheme["mzlib"] is
       used. The @scheme[file-string] is then appended to the
       directory using @scheme[build-path] to obtain the path to
       include.}

]

If @scheme[path-spec] specifies a relative path to include, the path
is resolved relative to the source for the @scheme[include]
expression, if that source is a complete path string. If the source is
not a complete path string, then @scheme[path-spec] is resolved
relative to the current load relative directory if one is available,
or to the current directory otherwise.

The included syntax is given the lexical context of the
@scheme[include] expression.}

@deftogether[(
@defform[(include-at/relative-to context source path-spec)]
@defform[(include-at/relative-to/reader context source path-spec reader-expr)]
@defform[(include/reader path-spec reader-expr)]
)]{

Variants of @scheme[include] analogous to the variants of
@schememodname[scheme/include].}

