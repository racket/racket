#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/port
                     racket/tcp))

@title{Ports}

@defmodule[ffi/unsafe/port]{The
@racketmodname[ffi/unsafe/port] library provides functions for working
with ports, file descriptors, and sockets. The library's operations
are unsafe, because no checking is performed on file descriptors and
sockets, and misuse of file descriptors and sockets can break other
objects.}

@history[#:added "6.11.0.4"]

@deftogether[(
@defproc[(unsafe-file-descriptor->port [fd exact-integer?]
                                       [name any/c]
                                       [mode (listof (or/c 'read 'write 'text 'regular-file))])
         (or/c port? (values input-port? output-port?))]
@defproc[(unsafe-socket->port [socket exact-integer?]
                              [name bytes?]
                              [mode (listof (or/c 'no-close))])
         (values input-port? output-port?)]
)]{         

Returns an input port and/or output port for the given file descriptor
or socket. On Windows, a ``file descriptor'' corresponds to a file
@tt{HANDLE}, while a socket corresponds to a @tt{SOCKET}. On Unix, a
socket is a file descriptor, but using the socket-specific
@racket[unsafe-socket->port] may enable socket-specific functionality,
such as address reporting via @racket[tcp-addresses].

The @racket[name] argument determines the port's name as reported by
@racket[object-name]. The @racket[name] must be a UTF-8 encoding that
is converted to a symbol for the socket name.

For a file descriptor, the @racket[mode] list must include at least
one of @racket['read] or @racket['write], and two ports are returned
if @racket[mode] includes both @racket['read] and @racket['write]. The
@racket['text] mode affects only Windows ports. The
@racket['regular-file] mode indicates that the file descriptor
corresponds to a regular file (which has the property, for example,
that reading never blocks). Closing all returned file-descriptor ports
closes the file descriptor.

For a socket, the @racket[mode] list can include @racket['no-close],
in which case closing both of the returned ports does not close the
socket.

For any kind of result port, closing the resulting ports readies and
unregisters any semaphores for the file descriptor or socket that were
previously created with @racket[unsafe-file-descriptor->semaphore] or
@racket[unsafe-socket->semaphore].}


@deftogether[(
@defproc[(unsafe-port->file-descriptor [p port?])
         (or/c exact-integer? #f)]
@defproc[(unsafe-port->socket [p port?])
         (or/c exact-integer? #f)]
)]{

Returns a file descriptor (which is a @tt{HANDLE} value on Windows) of
a socket for @racket[port] if it has one, @racket[#f] otherwise.}


@deftogether[(
@defproc[(unsafe-file-descriptor->semaphore [fd exact-integer?]
                                            [mode (or/c 'read 'write 'check-read 'check-write 'remove)])
         (or/c semaphore? #f)]
@defproc[(unsafe-socket->semaphore [socket exact-integer?]
                                   [mode (or/c 'read 'write 'check-read 'check-write 'remove)])
         (or/c semaphore? #f)]
)]{         

Returns a semaphore that becomes ready when @racket[fd] or @racket[socket]
is ready for reading or writing, as selected by @racket[mode]. Specifically,
these functions provide a one-shot, @emph{edge-triggered} indicator; the
semaphore is posted the @emph{first time} any of the following cases holds:

@itemlist[

@item{@racket[fd] or @racket[socket] is ready for reading or writing
(depending on @racket[mode]),}

@item{ports were created from @racket[fd] or @racket[socket] using
@racket[unsafe-file-descriptor->port] or @racket[unsafe-socket->port],
and those ports were closed, or}

@item{a subsequent call occurred with the same @racket[fd] or
@racket[socket] and with @racket['remove] for @racket[mode].}

]

The result is @racket[#f] if a conversion to a semaphore is not
supported for the current platform or for the given file descriptor or
socket.

The @racket['check-read] and @racket['check-write] modes are like
@racket['read] and @racket['write], but the result if @racket[#f] if a
semaphore is not already generated for the specified file descriptor
or socket in the specified mode.

The @racket['remove] mode readies and unregisters any semaphores
previously created for the given file descriptor or socket. Semaphores
must be unregistered before the file descriptor or socket is closed.
Beware that closing a port from @racket[unsafe-file-descriptor->port]
or @racket[unsafe-socket->port] will also ready and unregister
semaphores.}


@defproc[(unsafe-fd->evt [fd exact-integer?]
                         [mode (or/c 'read 'write 'check-read 'check-write 'remove)]
                         [socket? any/c #t])
         (or/c evt? #f)]{

Returns an event that is ready when @racket[fd] is ready for reading
or writing, as selected by @racket[mode]. Specifically, it returns a
multi-use, @emph{level-triggered} indicator; the event is ready
@emph{whenever} any of the following cases holds:

@itemlist[

@item{@racket[fd] is ready for reading or writing (depending on
@racket[mode]),}

@item{a subsequent call occurred with the same @racket[fd] and with
@racket['remove] for @racket[mode] (once removed, the event is
perpetually ready).}

]

The synchronization result of the event is the event itself.

The @racket['check-read] and @racket['check-write] modes are like
@racket['read] and @racket['write], but the result is @racket[#f] if
an event is not already generated for the specified file descriptor or
socketin the specified mode.

The @racket['remove] mode readies and unregisters any events
previously created for the given file descriptor or socket. Events
must be unregistered before the file descriptor or socket is
closed. Unlike @racket[unsafe-file-descriptor->semaphore] and
@racket[unsafe-socket->semaphore], closing a port from
@racket[unsafe-file-descriptor->port] or @racket[unsafe-socket->port]
does not unregister events.

@history[#:added "7.2.0.6"]}
