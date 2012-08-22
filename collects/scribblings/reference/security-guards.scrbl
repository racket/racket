#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "securityguards"]{Security Guards}

@defproc[(security-guard? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a security guard value as created
by @racket[make-security-guard], @racket[#f] otherwise.}


A @deftech{security guard} provides a set of access-checking
procedures to be called when a thread initiates access of a file,
directory, or network connection through a primitive procedure. For
example, when a thread calls @racket[open-input-file], the thread's
current security guard is consulted to check whether the thread is
allowed read access to the file. If access is granted, the thread
receives a port that it may use indefinitely, regardless of changes to
the security guard (although the port's custodian could shut down the
port; see @secref["custodians"]).

A thread's current security guard is determined by the
@racket[current-security-guard] parameter. Every security guard has a
parent, and a parent's access procedures are called whenever a child's
access procedures are called. Thus, a thread cannot increase its own
access arbitrarily by installing a new guard. The initial security
guard enforces no access restrictions other than those enforced by the
host platform.

@defproc[(make-security-guard [parent security-guard?]
                              [file-guard (symbol? 
                                           (or/c path? #f)
                                           (listof symbol?) 
                                           . -> . any)]
                              [network-guard (symbol?
                                              (or/c (and/c string? immutable?) #f)
                                              (or/c (integer-in 1 65535) #f)
                                              (or/c 'server 'client)
                                              . -> . any)]
                              [link-guard (or/c (symbol? path? path? . -> . any) #f)
                                          #f])
          security-guard?]{

Creates a new security guard as child of @racket[parent].

The @racket[file-guard] procedure must accept three arguments:

@itemize[

  @item{a symbol for the primitive procedure that triggered the access
  check, which is useful for raising an exception to deny access.}

  @item{a path (see @secref["pathutils"]) or @racket[#f] for
  pathless queries, such as @racket[(current-directory)],
  @racket[(filesystem-root-list)], and @racket[(find-system-path
  _symbol)]. A path provided to @racket[file-guard] is not expanded or
  otherwise normalized before checking access; it may be a relative
  path, for example.}

  @item{a list containing one or more of the following
  symbols:

    @itemize[

    @item{@indexed-racket['read] --- read a file or directory}

    @item{@indexed-racket['write] --- modify or create a file or
    directory}

    @item{@indexed-racket['execute] --- execute a file}

    @item{@indexed-racket['delete] --- delete a file or directory}

    @item{@indexed-racket['exists] --- determine whether a file or
    directory exists, or that a path string is well-formed}

    ]

 The @racket['exists] symbol is never combined with other symbols in
 the last argument to @racket[file-guard], but any other combination is
 possible. When the second argument to @racket[file-guard] is @racket[#f],
 the last argument always contains only @racket['exists].}

]

The @racket[network-guard] procedure must accept four arguments:

@itemize[

 @item{a symbol for the primitive operation that triggered the access
 check, which is useful for raising an exception to deny access.}

 @item{an immutable string representing the target hostname for a
 client connection or the accepting hostname for a listening server;
 @racket[#f] for a listening server or UDP socket that accepts
 connections at all of the host's address; or @racket[#f] an unbound
 UDP socket.}

 @item{an exact integer between @racket[1] and @racket[65535]
 (inclusive) representing the port number, or @racket[#f] for an
 unbound UDP socket. In the case of a client connection, the port
 number is the target port on the server. For a listening server, the
 port number is the local port number.}

 @item{a symbol, either @indexed-racket['client] or
 @indexed-racket['server], indicating whether the check is for the
 creation of a client connection or a listening server. The opening of
 an unbound UDP socket is identified as a @racket['client] connection;
 explicitly binding the socket is identified as a @racket['server]
 action.}

]

The @racket[link-guard] argument can be @racket[#f] or a procedure of
three arguments:

@itemize[

  @item{a symbol for the primitive procedure that triggered the access
  check, which is useful for raising an exception to deny access.}

  @item{a complete path (see @secref["pathutils"]) representing the
  file to create as link.}

  @item{a path representing the content of the link, which may be
  relative the second-argument path; this path is not expanded or
  otherwise normalized before checking access.}

]

If @racket[link-guard] is @racket[#f], then a default
procedure is used that always raises @racket[exn:fail].

The return value of @racket[file-guard], @racket[network-guard], or
@racket[link-guard] is ignored. To deny access, the procedure must
raise an exception or otherwise escape from the context of the
primitive call. If the procedure returns, the parent's corresponding
procedure is called on the same inputs, and so on up the chain of
security guards.

The @racket[file-guard], @racket[network-guard], and
@racket[link-guard] procedures are invoked in the thread that called
the access-checked primitive. Breaks may or may not be enabled (see
@secref["breakhandler"]). Full continuation jumps are blocked going
into or out of the @racket[file-guard] or @racket[network-guard] call
(see @secref["prompt-model"]).}


@defparam[current-security-guard guard security-guard?]{

A @tech{parameter} that determines the current security guard that controls
access to the filesystem and network.}


