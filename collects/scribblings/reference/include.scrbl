#lang scribble/doc
@(require "mz.ss")

@title[#:tag "include"]{File Inclusion}

@note-lib[racket/include]

@defform/subs[#:literals (file lib)
              (include path-spec)
              ([path-spec string
                          (file string)
                          (lib string ...+)])]{

Inlines the syntax in the file designated by @scheme[path-spec] in
place of the @scheme[include] expression.

A @scheme[path-spec] resembles a subset of the @scheme[_mod-path]
forms for @scheme[require], but it specifies a file whose content need
not be a module. That is, @scheme[string] refers to a file using a
platform-independent relative path, @scheme[(file string)] refers to a
file using platform-specific notation, and @scheme[(lib string ...)]
refers to a file within a collection.

If @scheme[path-spec] specifies a relative path, the path is resolved
relative to the source for the @scheme[include] expression, if that
source is a complete path string. If the source is not a complete path
string, then @scheme[path-spec] is resolved relative to
@scheme[(current-load-relative-directory)] if it is not @scheme[#f],
or relative to @scheme[(current-directory)] otherwise.

The included syntax is given the lexical context of the
@scheme[include] expression, while the included syntax's source
location refers to its actual source.}


@defform[(include-at/relative-to context source path-spec)]{

Like @scheme[include], except that the lexical context of
@scheme[context] is used for the included syntax, and a relative
@scheme[path-spec] is resolved with respect to the source of
@scheme[source]. The @scheme[context] and @scheme[source] elements are
otherwise discarded by expansion.}


@defform[(include/reader path-spec reader-expr)]{

Like @scheme[include], except that the procedure produced by the
expression @scheme[reader-expr] is used to read the included file,
instead of @scheme[read-syntax].

The @scheme[reader-expr] is evaluated at expansion time in the
@tech{transformer environment}. Since it serves as a replacement for
@scheme[read-syntax], the expression's value should be a procedure
that consumes two inputs---a string representing the source and an
input port---and produces a syntax object or @scheme[eof]. The
procedure will be called repeatedly until it produces @scheme[eof].

The syntax objects returned by the procedure should have source
location information, but usually no lexical context; any lexical
context in the syntax objects will be ignored.}


@defform[(include-at/relative-to/reader context source path-spec reader-expr)]{

Combines @scheme[include-at/relative-to] and @scheme[include/reader].}
