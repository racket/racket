#lang scribble/doc
@(require scribble/manual 
          "common.rkt" 
          (for-label racket/base
                     setup/unpack
                     setup/dirs))

@title[#:tag "unpack"]{@exec{raco unpack}: Unpacking Library Collections}

The @exec{raco unpack} command unpacks a @filepath{.plt} archive (see
@secref["plt"]) to the current directory without attempting to install
any collections. Use @exec{raco pkg} (see @other-manual['(lib
"pkg/scribblings/pkg.scrbl")]) to install a @filepath{.plt} archive as
a package, or use @exec{raco setup -A} (see @secref["setup"]) to
unpack and install collections from a @filepath{.plt} archive.

Command-line flags:

@itemlist[

 @item{@Flag{l} or @DFlag{list} --- lists the content of the archive
       without unpacking it.}

 @item{@Flag{c} or @DFlag{config} --- shows the archive configuration
       before unpacking or listing the archive content.}

 @item{@Flag{f} or @DFlag{force} --- replace files that exist already;
       fails that the archive says should be replaced will be replaced
       without this flag.}

]

@; ------------------------------------------------------------------------

@section[#:tag "unpacking-.plt-archives"]{Unpacking API}

@defmodule[setup/unpack]{The @racketmodname[setup/unpack]
library provides raw support for unpacking a @filepath{.plt} file.}

@defproc[(unpack [archive path-string?]
                 [main-collects-parent-dir path-string? (current-directory)]
                 [print-status (string? . -> . any) (lambda (x) (printf "~a\n" x))]
                 [get-target-directory (-> path-string?) (lambda () (current-directory))]
                 [force? any/c #f]
                 [get-target-plt-directory
                  (path-string? 
                   path-string? 
                   (listof path-string?) 
                   . -> . path-string?)
                  (lambda (_preferred-dir _main-dir _options)
                    _preferred-dir)])
          void?]{

Unpacks @racket[archive]. 

The @racket[main-collects-parent-dir] argument is passed along to
@racket[get-target-plt-directory].

The @racket[print-status] argument is used to report unpacking
progress.

The @racket[get-target-directory] argument is used to get the
destination directory for unpacking an archive whose content is
relative to an arbitrary directory.

If @racket[force?] is true, then version and required-collection
mismatches (comparing information in the archive to the current
installation) are ignored.

The @racket[get-target-plt-directory] function is called to select a
target for installation for an archive whose is relative to the
installation. The function should normally return one if its first two
arguments; the third argument merely contains the first two, but has
only one element if the first two are the same. If the archive does
not request installation for all uses, then the first two arguments
will be different, and the former will be a user-specific location,
while the second will refer to the main installation.}

@defproc[(fold-plt-archive [archive path-string?]
                           [on-config-fn (any/c any/c . -> . any/c)]
                           [on-setup-unit (any/c input-port? any/c . -> . any/c)]
                           [on-directory ((or/c path-string?
                                                (list/c (or/c 'collects 'doc 'lib 'include)
                                                        path-string?))
                                          any/c 
                                          . -> . any/c)]
                           [on-file (or/c ((or/c path-string?
                                                 (list/c (or/c 'collects 'doc 'lib 'include)
                                                         path-string?))   
                                           input-port? 
                                           any/c 
                                           . -> . any/c)
                                          ((or/c path-string?
                                                 (list/c (or/c 'collects 'doc 'lib 'include)
                                                         path-string?))
                                           input-port? 
                                           (one-of/c 'file 'file-replace) 
                                           any/c 
                                           . -> . any/c))]
                           [initial-value any/c])
          any/c]{

Traverses the content of @racket[archive], which must be a
@filepath{.plt} archive that is created with the default unpacking
unit and configuration expression. The configuration expression is not
evaluated, the unpacking unit is not invoked, and not files are
unpacked to the filesystem. Instead, the information in the archive is
reported back through @racket[on-config], @racket[on-setup-unit],
@racket[on-directory], and @racket[on-file], each of which can build on
an accumulated value that starts with @racket[initial-value] and whose
final value is returned.

The @racket[on-config-fn] function is called once with an S-expression
that represents a function to implement configuration information.
The second argument to @racket[on-config] is @racket[initial-value],
and the function's result is passes on as the last argument to @racket[on-setup-unit].

The @racket[on-setup-unit] function is called with the S-expression
representation of the installation unit, an input port that points to
the rest of the file, and the accumulated value. This input port is
the same port that will be used in the rest of processing, so if
@racket[on-setup-unit] consumes any data from the port, then that data
will not be consumed by the remaining functions. (This means that
on-setup-unit can leave processing in an inconsistent state, which is
not checked by anything, and therefore could cause an error.)
The result of @racket[on-setup-unit] becomes the new accumulated value.

For each directory that would be created by the archive when unpacking
normally, @racket[on-directory] is called with the directory
path (described more below) and the accumulated value up to that
point, and its result is the new accumulated value.

For each file that would be created by the archive when unpacking
normally, @racket[on-file] is called with the file path (described
more below), an input port containing the contents of the file, an
optional mode symbol indicating whether the file should be replaced,
and the accumulated value up to that point; its result is the new
accumulated value. The input port can be used or ignored, and parsing
of the rest of the file continues the same either way. After
@racket[on-file] returns control, however, the input port is drained
of its content.

A directory or file path can be a plain path, or it can be a list
containing @racket['collects], @racket['doc], @racket['lib], or
@racket['include] and a relative path. The latter case corresponds to
a directory or file relative to a target installation's collection
directory (in the sense of @racket[find-collects-dir]), documentation
directory (in the sense of @racket[find-doc-dir]), library
directory (in the sense of @racket[find-lib-dir]), or ``include''
directory (in the sense of @racket[find-include-dir]).}
