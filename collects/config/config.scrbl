#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     config))

@title{@bold{Config}: Installation and Search Paths}

@section{Configuring Directories and Search Paths}

@defmodule[config]{

The @schememodname[config] library specifies the location of
directories (such as the main documentation directory) and also
directory search paths (such as a list of directories to search for
documentation).}


@bold{Note:} Instead of @scheme[require]ing
@schememodname[config] directly, use the
@schememodname[setup/dirs] library, which combines information from
@schememodname[config] and other sources.

The @schememodname[config] module must export the following
values. In all cases where a @scheme[delay]ed value is expected for an
exported identifier, the value can be a @scheme[delay]ed @scheme[#f]
to indicate the default.

@defthing[doc-dir (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, or byte string for the main
documentation directory. It defaults to a @filepath{doc} sibling
directory of the main collection directory.}

@defthing[lib-dir (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, or byte string for the main directory
containing C libraries and build information; it defaults to a
@filepath{lib} sibling directory of the main collection directory.}

@defthing[dll-dir (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, or byte string for a directory
containing Unix shared libraries for the main executable; it defaults
to the main C-library directory}

@defthing[include-dir (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, or byte string for the main directory
containing C header files; it defaults to an @filepath{include}
sibling directory of the main collection directory.}

@defthing[bin-dir (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, or byte string for the main directory
containing executables; it defaults to a @filepath{bin} sibling
directory of the main collection directory.}

@defthing[doc-search-dirs (promise/c (or/c path? string? bytes? false/c))]{

A @scheme[delay]ed path, string, byte string, or @scheme[#f]
representing the search path for documentation; each @scheme[#f] in
the list, if any, is replaced with the default search path, which is
the user- and version-specific @filepath{doc} directory followed by
the main documentation directory.}

@defthing[lib-search-dirs (promise/c (or/c path? string? bytes? false/c))]{

Like @scheme[doc-search-dirs], but for directories containing C
libraries and other build information}

@defthing[include-search-dirs(promise/c (or/c path? string? bytes? false/c))]{

Like @scheme[doc-search-dirs], but for directories containing C header
files}

@defthing[absolute-installation? boolean?]{

A (simple, non-@scheme[delay]ed) boolean that is @scheme[#t] if the
installation uses absolute path names, @scheme[#f] otherwise.}

@defthing[cgc-suffix (promise/c (or/c string? false/c))]{

A @scheme[delay]ed string used as the suffix (before the actual
suffix, such as @filepath{.exe}) for a @filepath{CGC} executable. Use
Windows-style casing, and the string will be downcased as appropriate
(e.g., for a Unix binary name). A @scheme[#f] value means that if the
@exec{mzscheme} binary identifies itself as CGC, then the suffix is
@scheme[""], otherwise it is @scheme["CGC"].}

@defthing[3m-suffix (promise/c (or/c string? false/c))]{

Analogous to @scheme[cgc-suffix], but for 3m. A @scheme[#f] value
means that if the @filepath{mzscheme} binary identifies itself as CGC,
then the suffix is @scheme["3m"], otherwise it is @scheme[""].}

@; ----------------------------------------------------------------------

@section{Overriding the Installation's Configuration}

A user can override an installation's configuration through a
@filepath{config} collection in the user's collection directory (which
normally takes precedence over the main collection directory).

@; ----------------------------------------------------------------------

@section{Configuration Language}

@defmodule[setup/configtab]{

The @schememodname[setup/configtab] library defines a language module
that can be used to implement @schememodname[config].}

When @schememodname[setup/configtab] is used as a language module, the
module body must consist of a sequence of

@schemeblock[(define _id _val)]

declarations, where each @scheme[_id] is one of the names that the
@schememodname[config] library must export, and @scheme[_val] is
an expression for the value (which will be automatically wrapped with
@scheme[delay] when needed). If a required export has no corresponding
@scheme[define], a definition with @scheme[#f] is inserted
automatically.
