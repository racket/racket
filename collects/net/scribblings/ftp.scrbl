#lang scribble/doc
@(require "common.ss"
          (for-label net/ftp
                     net/ftp-unit
                     net/ftp-sig))

@title[#:tag "ftp"]{FTP: Client Downloading}

@defmodule[net/ftp]{The @schememodname[net/ftp] library provides
utilities for FTP client operations.

The library was written by Micah Flatt.}

@section[#:tag "ftp-procs"]{Functions}

@defproc[(ftp-connection? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] represents an FTP connection as
returned by @scheme[ftp-establish-connection], @scheme[#f] otherwise.}


@defproc[(ftp-establish-connection [server string?]
                                   [port-no (integer-in 0 65535)]
                                   [user string?]
                                   [passwd string?])
          ftp-connection?]{

Establishes an FTP connection with the given server using the
supplied username and password.

The username and password strings are encoded to bytes using the
current locale's encoding.}


@defproc[(ftp-close-connection [ftp-conn ftp-connection?]) void?]{

Closes an FTP connection.}


@defproc[(ftp-cd [ftp-conn ftp-connection?][new-dir string?]) void?]{

Changes the current directory on the FTP server to @scheme[new-dir].
The @scheme[new-dir] argument is not interpreted at all, but simply
passed on to the server (encoded using the current locale's
encoding); it must not contain a newline.}

@defproc[(ftp-directory-list [ftp-conn ftp-connection?])
         (listof (list/c (one-of/c "-" "d" "l")
                         string?
                         string?))]{

Returns a list of files and directories in the current directory of
the server, assuming that the server provides directory information in
the quasi-standard Unix format.

Each file or directory is represented by a list of three strings. The
first string is either @scheme["-"], @scheme["d"], or @scheme["l"],
depending on whether the items is a file, directory, or link,
respectively.  The second item is the file's date; to convert this
value to seconds consistent with @scheme[file-seconds], pass the date
string to @scheme[ftp-make-file-seconds], below.  The third string is
the name of the file or directory.

All strings are decoded from bytes using the current locale's
encoding.}


@defproc[(ftp-make-file-seconds [ftp-date string?]) exact-integer?]{

Takes a date string produced by @scheme[ftp-directory-list] and
converts it to seconds (which can be used with
@scheme[seconds->date]).}


@defproc[(ftp-download-file [ftp-conn ftp-connection?]
                            [local-dir path-string?]
                            [file string?]) void?]{

Downloads @scheme[file] from the server's current directory and puts
it in @scheme[local-dir] using the same name. If the file already
exists in the local directory, it is replaced, but only after the
transfer succeeds (i.e., the file is first downloaded to a temporary
file, then moved into place on success).}

@; ----------------------------------------

@section{FTP Unit}

@defmodule[net/ftp-unit]

@defthing[ftp@ unit?]{

Imports nothing, exports @scheme[ftp^].}

@; ----------------------------------------

@section{FTP Signature}

@defmodule[net/ftp-sig]

@defsignature[ftp^ ()]{}

Includes everything exported by the @schememodname[net/ftp] module.
