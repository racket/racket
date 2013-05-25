#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     racket/contract))

@title[#:tag "config-file"]{Installation Configuration and Search Paths}

A configuration directory path is built into the Racket executable as
selected at install time. Use @racket[find-system-path 'config-dir] to
locate the configuration directory.

Other directories and attributes of an installation can be configured
through files in the configuration directory. Instead of trying to
read configuraion files directly, however, use the
@racketmodname[setup/dirs] library, which combines information from
the configuration files and other sources.

A @filepath{config.rktd} file in the configuration directory should
contain a @racket[read]able hash table with any of the following
symbolic keys:

@itemlist[

 @item{@racket['doc-dir] --- a path, string, or byte string for the
       main documentation directory. The value defaults to a
       @filepath{doc} sibling directory of the main collection
       directory's parent.}

 @item{@racket['lib-dir] --- a path, string, or byte string for the
       main library directory; it defaults to the parent of the main
       collection directory.}

 @item{@racket['dll-dir] --- a path, string, or byte string for a
       directory containing Unix shared libraries for the main
       executable; it defaults to the main library directory.}

 @item{@racket['include-dir] --- a path, string, or byte string for
       the main directory containing C header files; it defaults to an
       @filepath{include} sibling directory of the main library
       directory.}

 @item{@racket['bin-dir] --- a path, string, or byte string for the
       main directory containing executables; it defaults to a
       @filepath{bin} sibling directory of the main library
       directory.}

 @item{@racket['doc-search-dirs] --- a path, string, byte string, or
       @racket[#f] representing the search path for documentation;
       each @racket[#f] in the list, if any, is replaced with the
       default search path, which is the user- and version-specific
       @filepath{doc} directory followed by the main documentation
       directory.}

 @item{@racket['lib-search-dirs] --- like @racket[doc-search-dirs],
       but for directories containing foreign libraries.}

 @item{@racket['include-search-dirs] --- like
       @racket[doc-search-dirs], but for directories containing C
       header files}

 @item{@racket['absolute-installation?] --- a boolean that is
       @racket[#t] if the installation uses absolute path names,
       @racket[#f] otherwise.}

 @item{@racket['cgc-suffix] --- a string used as the suffix (before
       the actual suffix, such as @filepath{.exe}) for a
       @filepath{CGC} executable. Use Windows-style casing, and the
       string will be downcased as appropriate (e.g., for a Unix
       binary name). A @racket[#f] value means that if the
       @exec{racket} binary identifies itself as CGC, then the suffix
       is @racket[""], otherwise it is @racket["CGC"].}

 @item{@racket['3m-suffix] --- analogous to @racket['cgc-suffix], but
       for 3m. A @racket[#f] value means that if the @filepath{racket}
       binary identifies itself as CGC, then the suffix is
       @racket["3m"], otherwise it is @racket[""].}

]
