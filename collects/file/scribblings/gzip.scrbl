#lang scribble/doc
@(require "common.ss"
          (for-label file/gzip))

@title[#:tag "gzip"]{@exec{gzip} Compression and File Creation}

@defmodule[file/gzip]{The @schememodname[file/gzip] library provides
utilities to create archive files in @exec{gzip} format, or simply to
compress data using the @exec{pkzip} ``deflate'' method.}

@defproc[(gzip [in-file path-string?]
               [out-file path-string? (string-append in-file ".gz")])
         void?]{

Compresses data to the same format as the @exec{gzip} utility, writing
the compressed data directly to a file. The @scheme[in-file] argument
is the name of the file to compress. If the file named by
@scheme[out-file] exists, it will be overwritten.}


@defproc[(gzip-through-ports [in input-port?]
                             [out output-port?]
                             [orig-filename (or/c string? false/c)]
                             [timestamp exact-integer?])
         void?]{

Reads the port @scheme[in] for data and compresses it to @scheme[out],
outputting the same format as the @exec{gzip} utility. The
@scheme[orig-filename] string is embedded in this output;
@scheme[orig-filename] can be @scheme[#f] to omit the filename from
the compressed stream. The @scheme[timestamp] number is also embedded
in the output stream, as the modification date of the original file
(in Unix seconds, as @scheme[file-or-directory-modify-seconds] would
report under Unix).}


@defproc[(deflate [in input-port?]
                  [out output-port?])
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{

Writes @exec{pkzip}-format ``deflated'' data to the port @scheme[out],
compressing data from the port @scheme[in].  The data in a file
created by @exec{gzip} uses this format (preceded with header
information).

The result is three values: the number of bytes read from @scheme[in],
the number of bytes written to @scheme[out], and a cyclic redundancy
check (CRC) value for the input.}
