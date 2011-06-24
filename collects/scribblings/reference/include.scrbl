#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "include"]{File Inclusion}

@note-lib[racket/include]

@defform/subs[#:literals (file lib)
              (include path-spec)
              ([path-spec string
                          (file string)
                          (lib string ...+)])]{

Inlines the syntax in the file designated by @racket[path-spec] in
place of the @racket[include] expression.

A @racket[path-spec] resembles a subset of the @racket[_mod-path]
forms for @racket[require], but it specifies a file whose content need
not be a module. That is, @racket[string] refers to a file using a
platform-independent relative path, @racket[(file string)] refers to a
file using platform-specific notation, and @racket[(lib string ...)]
refers to a file within a collection.

If @racket[path-spec] specifies a relative path, the path is resolved
relative to the source for the @racket[include] expression, if that
source is a complete path string. If the source is not a complete path
string, then @racket[path-spec] is resolved relative to
@racket[(current-load-relative-directory)] if it is not @racket[#f],
or relative to @racket[(current-directory)] otherwise.

The included syntax is given the lexical context of the
@racket[include] expression, while the included syntax's source
location refers to its actual source.}


@defform[(include-at/relative-to context source path-spec)]{

Like @racket[include], except that the lexical context of
@racket[context] is used for the included syntax, and a relative
@racket[path-spec] is resolved with respect to the source of
@racket[source]. The @racket[context] and @racket[source] elements are
otherwise discarded by expansion.}


@defform[(include/reader path-spec reader-expr)]{

Like @racket[include], except that the procedure produced by the
expression @racket[reader-expr] is used to read the included file,
instead of @racket[read-syntax].

The @racket[reader-expr] is evaluated at expansion time in the
@tech{transformer environment}. Since it serves as a replacement for
@racket[read-syntax], the expression's value should be a procedure
that consumes two inputs---a string representing the source and an
input port---and produces a syntax object or @racket[eof]. The
procedure will be called repeatedly until it produces @racket[eof].

The syntax objects returned by the procedure should have source
location information, but usually no lexical context; any lexical
context in the syntax objects will be ignored.}


@defform[(include-at/relative-to/reader context source path-spec reader-expr)]{

Combines @racket[include-at/relative-to] and @racket[include/reader].}
