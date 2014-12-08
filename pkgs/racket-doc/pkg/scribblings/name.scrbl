#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     pkg/name))
                     

@title[#:tag "name"]{Package Source Parsing}

@defmodule[pkg/name]{The @racketmodname[pkg/name] library provides
functions for parsing and normalizing a package source, especially for
extracting a package name.}


@defproc[(package-source-format? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is @racket['name] , @racket['file],
@racket['dir], @racket['git], @racket['github], @racket['clone], @racket['file-url],
@racket['dir-url], @racket['link], or @racket['static-link], and
returns @racket[#f] otherwise.

The @racket['link] and @racket['static-link] formats are the same as
@racket['dir] in terms of parsing, but they are treated differently
for tasks such as package installation. The @racket['clone] format
is similarly the same as @racket['github] or @racket['git] in terms of
parsing.

@history[#:changed "6.1.1.1" @elem{Added @racket['git].}
         #:changed "6.1.1.5" @elem{Added @racket['clone].}]}


@defproc[(package-source->name [source string?]
                               [type (or/c package-source-format? #f)
                                #f])
          (or/c #f string?)]{

Extracts the @tech{package name} from a @tech{package source}, where
the package source type is inferred if @racket[type] is @racket[#f].
If a valid name cannot be inferred, the result is @racket[#f].}


@defproc[(package-source->name+type [source string?]
                                    [type (or/c package-source-format? #f) #f]
                                    [#:complain complain-proc (string? string? . -> . any) void]
                                    [#:must-infer-name? must-infer-name? boolean? #f]
                                    [#:link-dirs? link-dir? boolean?])
          (values (or/c #f string?)
                  (or/c package-source-format? #f))]{

Like @racket[package-source->name], but also returns the type of the
source (which is useful when the type is inferred). If the source is
not well-formed, the second result can be @racket[#f].

The @racket[complain-proc] function is applied when @racket[source] is
ill-formed. The arguments to @racket[complain-proc] are
@racket[source] and an error message.

If @racket[must-infer-name?] is true, then @racket[complain-proc]
is called if a valid name cannot be inferred from @racket[source].

If @racket[link-dirs?] is true, then a directory path is reported as
type @racket['link] instead of @racket['dir].}


@defproc[(package-source->path [source string?]
                               [type (or/c #f 'file 'dir 'link 'static-link) #f])
         path?]{

Converts a file or directory package source to a filesystem path.

The @racket[package-source->path] function is different from
@racket[string->path] in the case that @racket[source] starts with
@litchar{file://}. Also, if @racket[type] is @racket['dir],
@racket['link], or @racket['static-link], then
@racket[path->directory-path] is used to ensure that the result path
refers to a directory.

@history[#:added "10.0.1.11"]}

