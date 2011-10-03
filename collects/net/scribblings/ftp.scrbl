#lang scribble/doc
@(require "common.rkt" (for-label net/ftp net/ftp-unit net/ftp-sig))

@title[#:tag "ftp"]{FTP: Client Downloading}

@defmodule[net/ftp]{The @racketmodname[net/ftp] library provides
utilities for FTP client operations.

The library was written by Micah Flatt.}

@section[#:tag "ftp-procs"]{Functions}

@defproc[(ftp-connection? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents an FTP connection as
returned by @racket[ftp-establish-connection], @racket[#f] otherwise.}


@defproc[(ftp-establish-connection [server string?]
                                   [port-no (integer-in 0 65535)]
                                   [user string?]
                                   [passwd string?])
          ftp-connection?]{

Establishes an FTP connection with the given server using the supplied
username and password. The @racket[port-np] argument usually should be
@racket[21].}


@defproc[(ftp-close-connection [ftp-conn ftp-connection?]) void?]{

Closes an FTP connection.}


@defproc[(ftp-cd [ftp-conn ftp-connection?] [new-dir string?]) void?]{

Changes the current directory on the FTP server to @racket[new-dir].
The @racket[new-dir] argument is not interpreted at all, but simply
passed on to the server; it must not contain a newline.}

@defproc[(ftp-directory-list [ftp-conn ftp-connection?]
                             [path (or/c false/c string?) #f])
         (listof (list/c (one-of/c "-" "d" "l")
                         string?
                         string?))]{

Returns a list of files and directories in the current directory of the
server, assuming that the server provides directory information in the
quasi-standard Unix format.  If a @racket[path] argument is given, use
it instead of the current directory.

Each file or directory is represented by a list of three or four
strings.  The first string is either @racket["-"], @racket["d"], or
@racket["l"], depending on whether the items is a file, directory, or
link, respectively.  The second item is the file's date; to convert this
value to seconds consistent with @racket[file-seconds], pass the date
string to @racket[ftp-make-file-seconds].  The third string is the name
of the file or directory.  If the item is a file (the first string is
@racket["-"]), and if the line that the server replied with has a size
in the expected place, then a fourth string containing this size is
included.

Warning: the FTP protocol has no specification for the reply format, so
this information can be unreliable.}


@defproc[(ftp-make-file-seconds [ftp-date string?]) exact-integer?]{

Takes a date string produced by @racket[ftp-directory-list] and
converts it to seconds (which can be used with
@racket[seconds->date]).

Warning: the FTP protocol has no specification for the reply format, so
this information can be unreliable.}


@defproc[(ftp-download-file [ftp-conn ftp-connection?]
                            [local-dir path-string?]
                            [file string?]) void?]{

Downloads @racket[file] from the server's current directory and puts
it in @racket[local-dir] using the same name. If the file already
exists in the local directory, it is replaced, but only after the
transfer succeeds (i.e., the file is first downloaded to a temporary
file, then moved into place on success).}

@; ----------------------------------------

@section{FTP Unit}

@margin-note{@racket[ftp@] and @racket[ftp^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/ftp] module.}

@defmodule[net/ftp-unit]

@defthing[ftp@ unit?]{

Imports nothing, exports @racket[ftp^].}

@; ----------------------------------------

@section{FTP Signature}

@defmodule[net/ftp-sig]

@defsignature[ftp^ ()]{}

Includes everything exported by the @racketmodname[net/ftp] module.
