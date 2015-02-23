#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          (for-label racket/gui
                     compiler/bundle-dist))

@title{API for Bundling Distributions}

@defmodule[compiler/bundle-dist]{

The @racketmodname[compiler/bundle-dist] library provides a function
to pack a directory (usually assembled by
@racket[assemble-distribution]) into a distribution file.  On
Windows, the result is a @filepath{.zip} archive; on Mac OS X, it's
a @filepath{.dmg} disk image; on Unix, it's a @filepath{.tgz}
archive.}


@defproc[(bundle-directory [dist-file file-path?] 
                           [dir file-path?]
                           [for-exe? any/c #f])
         void?]{

Packages @racket[dir] into @racket[dist-file]. If @racket[dist-file]
has no extension, a file extension is added automatcially (using the
first result of @racket[bundle-put-file-extension+style+filters]).

The created archive contains a directory with the same name as
@racket[dir]---except on Mac OS X when @racket[for-exe?] is true
and @racket[dir] contains a single a single file or directory, in
which case the created disk image contains just the file or
directory. The default for @racket[for-exe?] is @racket[#f].

Archive creation fails if @racket[dist-file] exists.}


@defproc[(bundle-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Returns three values suitable for use as the @racket[extension],
@racket[style], and @racket[filters] arguments to @racket[put-file],
respectively to select a distribution-file name.}
