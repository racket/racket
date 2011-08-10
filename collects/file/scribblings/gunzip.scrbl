#lang scribble/doc
@(require "common.rkt" (for-label file/gunzip file/gzip))

@title[#:tag "gunzip"]{@exec{gzip} Decompression}

@defmodule[file/gunzip]{The @racketmodname[file/gunzip] library provides
utilities to decompress archive files in @exec{gzip} format, or simply
to decompress data using the @exec{pkzip} ``inflate'' method.}


@defproc[(gunzip [file path-string?]
                 [output-name-filter (string? boolean? . -> . path-string?)
                                     (lambda (file archive-supplied?) file)])
         void?]{ 

Extracts data that was compressed using the @exec{gzip} utility (or
@racket[gzip] function), writing the uncompressed data directly to a
file. The @racket[file] argument is the name of the file containing
compressed data. The default output file name is the original name of
the compressed file as stored in @racket[file]. If a file by this name
exists, it will be overwritten. If no original name is stored in the
source file, @racket["unzipped"] is used as the default output file
name.

The @racket[output-name-filter] procedure is applied to two
arguments---the default destination file name and a boolean that is
@racket[#t] if this name was read from @racket[file]---before the
destination file is created. The return value of the file is used as
the actual destination file name (to be opened with the
@racket['truncate] flag of @racket[open-output-file]).

If the compressed data turns out to be corrupted, the
@racket[exn:fail] exception is raised.}


@defproc[(gunzip-through-ports [in input-port?]
                               [out output-port?])
         void?]{

Reads the port @racket[in] for compressed data that was created using
the @exec{gzip} utility, writing the uncompressed data to the port
@racket[out].

If the compressed data turns out to be corrupted, the
@racket[exn:fail] exception is raised. The unzipping process may peek
further into @racket[in] than needed to decompress the data, but it
will not consume the unneeded bytes.}


@defproc[(inflate [in input-port?]
                  [out output-port?])
         void?]{

Reads @exec{pkzip}-format ``deflated'' data from the port @racket[in]
and writes the uncompressed (``inflated'') data to the port
@racket[out].  The data in a file created by @exec{gzip} uses this
format (preceded with some header information).

If the compressed data turns out to be corrupted, the
@racket[exn:fail] exception is raised. The inflate process may peek
further into @racket[in] than needed to decompress the data, but it
will not consume the unneeded bytes.}
