#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     config))

@title{Config: Installation and Search Paths}

@section{Configuring Directories and Search Paths}

@defmodule[config]{

The @racketmodname[config] library specifies the location of
directories (such as the main documentation directory) and also
directory search paths (such as a list of directories to search for
documentation).}


@bold{Note:} Instead of @racket[require]ing
@racketmodname[config] directly, use the
@racketmodname[setup/dirs] library, which combines information from
@racketmodname[config] and other sources.

The @racketmodname[config] module must export the following
values. In all cases where a @racket[delay]ed value is expected for an
exported identifier, the value can be a @racket[delay]ed @racket[#f]
to indicate the default.

@defthing[doc-dir (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, or byte string for the main
documentation directory. It defaults to a @filepath{doc} sibling
directory of the main collection directory.}

@defthing[lib-dir (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, or byte string for the main directory
containing C libraries and build information; it defaults to a
@filepath{lib} sibling directory of the main collection directory.}

@defthing[dll-dir (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, or byte string for a directory
containing Unix shared libraries for the main executable; it defaults
to the main C-library directory}

@defthing[include-dir (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, or byte string for the main directory
containing C header files; it defaults to an @filepath{include}
sibling directory of the main collection directory.}

@defthing[bin-dir (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, or byte string for the main directory
containing executables; it defaults to a @filepath{bin} sibling
directory of the main collection directory.}

@defthing[doc-search-dirs (promise/c (or/c path? string? bytes? false/c))]{

A @racket[delay]ed path, string, byte string, or @racket[#f]
representing the search path for documentation; each @racket[#f] in
the list, if any, is replaced with the default search path, which is
the user- and version-specific @filepath{doc} directory followed by
the main documentation directory.}

@defthing[lib-search-dirs (promise/c (or/c path? string? bytes? false/c))]{

Like @racket[doc-search-dirs], but for directories containing C
libraries and other build information}

@defthing[include-search-dirs(promise/c (or/c path? string? bytes? false/c))]{

Like @racket[doc-search-dirs], but for directories containing C header
files}

@defthing[absolute-installation? boolean?]{

A (simple, non-@racket[delay]ed) boolean that is @racket[#t] if the
installation uses absolute path names, @racket[#f] otherwise.}

@defthing[cgc-suffix (promise/c (or/c string? false/c))]{

A @racket[delay]ed string used as the suffix (before the actual
suffix, such as @filepath{.exe}) for a @filepath{CGC} executable. Use
Windows-style casing, and the string will be downcased as appropriate
(e.g., for a Unix binary name). A @racket[#f] value means that if the
@exec{mzscheme} binary identifies itself as CGC, then the suffix is
@racket[""], otherwise it is @racket["CGC"].}

@defthing[3m-suffix (promise/c (or/c string? false/c))]{

Analogous to @racket[cgc-suffix], but for 3m. A @racket[#f] value
means that if the @filepath{mzscheme} binary identifies itself as CGC,
then the suffix is @racket["3m"], otherwise it is @racket[""].}

@; ----------------------------------------------------------------------

@section{Overriding the Installation's Configuration}

A user can override an installation's configuration through a
@filepath{config} collection in the user's collection directory (which
normally takes precedence over the main collection directory).

@; ----------------------------------------------------------------------

@section{Configuration Language}

@defmodule[setup/configtab]{

The @racketmodname[setup/configtab] library defines a language module
that can be used to implement @racketmodname[config].}

When @racketmodname[setup/configtab] is used as a language module, the
module body must consist of a sequence of

@racketblock[(define _id _val)]

declarations, where each @racket[_id] is one of the names that the
@racketmodname[config] library must export, and @racket[_val] is
an expression for the value (which will be automatically wrapped with
@racket[delay] when needed). If a required export has no corresponding
@racket[define], a definition with @racket[#f] is inserted
automatically.
