#lang scribble/doc
@(require "common.ss"
          (for-label file/zip
                     file/gunzip
                     scheme/file))

@title[#:tag "zip"]{@exec{zip} File Creation}

@defmodule[file/zip]{The @schememodname[file/zip] library provides
utilities to create @exec{zip} archive files, which are compatible
with both Windows and Unix (including Mac OS X) unpacking. The actual
compression is implemented by @scheme[deflate].}

@defproc[(zip [zip-file path-string?][path path-string?] ...) 
         void?]{

Creates @scheme[zip-file], which holds the complete content of all
@scheme[path]s.  The given @scheme[path]s are all expected to be
relative path names of existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@scheme[path], its ancestor directories are also added to the
resulting zip file, up to the current directory (using
@scheme[pathlist-closure]).  Files are packaged as usual for
@exec{zip} files, including permission bits for both Windows and Unix
(including Mac OS X).  The permission bits are determined by
@scheme[file-or-directory-permissions], which does not preserve the
distinction between owner/group/other permissions. Also, symbolic
links are always followed.}


@defproc[(zip->output [paths (listof path-string?)]
                      [out output-port? (current-output-port)])
         void?]{

Zips each of the given @scheme[paths], and packages it as a zip
``file'' that is written directly to @scheme[out].  Unlike
@scheme[zip], the specified @scheme[paths] are included as-is; if a
directory is specified, its content is not automatically added, and
nested directories are added without parent directories.}


@defboolparam[zip-verbose on?]{

A parameter that controls output during a @scheme[zip]
operation. Setting this parameter to a true value causes @scheme[zip]
to display to @scheme[(current-error-port)] the filename that is
currently being compressed.}
