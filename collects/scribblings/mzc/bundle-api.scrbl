#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label scheme/gui
                     compiler/bundle-dist))

@title{Scheme API for Bundling Distributions}

@defmodule[compiler/bundle-dist]{

The @schememodname[compiler/bundle-dist] library provides a function
to pack a directory (usually assembled by
@scheme[assemble-distribution]) into a distribution file.  Under
Windows, the result is a @filepath{.zip} archive; under Mac OS X, it's
a @filepath{.dmg} disk image; under Unix, it's a @filepath{.tgz}
archive.}


@defproc[(bundle-directory [dist-file file-path?] 
                           [dir file-path?]
                           [for-exe? any/c #f])
         void?]{

Packages @scheme[dir] into @scheme[dist-file]. If @scheme[dist-file]
has no extension, a file extension is added automatcially (using the
first result of @scheme[bundle-put-file-extension+style+filters]).

The created archive contains a directory with the same name as
@scheme[dir]---except under Mac OS X when @scheme[for-exe?] is true
and @scheme[dir] contains a single a single file or directory, in
which case the created disk image contains just the file or
directory. The default for @scheme[for-exe?] is @scheme[#f].

Archive creation fails if @scheme[dist-file] exists.}


@defproc[(bundle-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Returns three values suitable for use as the @scheme[extension],
@scheme[style], and @scheme[filters] arguments to @scheme[put-file],
respectively to select a distribution-file name.}
