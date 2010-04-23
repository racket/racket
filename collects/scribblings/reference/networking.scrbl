#lang scribble/doc
@(require "mz.ss")

@title[#:tag "networking" #:style 'toc]{Networking}

@local-table-of-contents[]

@;------------------------------------------------------------------------
@section[#:tag "tcp"]{TCP}

@note-lib[racket/tcp]

For information about TCP in general, see @italic{TCP/IP Illustrated,
 Volume 1} by W. Richard Stevens.

@defproc[(tcp-listen [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 0 65535))]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? #f) #f]) 
         tcp-listener?]{

Creates a ``listening'' server on the local machine at the port number
specified by @scheme[port-no]. If @scheme[port-no] is 0 the socket binds
to an ephemeral port, which can be determined by calling 
@scheme[tcp-addresses].  The @scheme[max-allow-wait] argument
determines the maximum number of client connections that can be
waiting for acceptance. (When @scheme[max-allow-wait] clients are
waiting acceptance, no new client connections can be made.)

If the @scheme[reuse?] argument is true, then @scheme[tcp-listen] will
create a listener even if the port is involved in a @tt{TIME_WAIT}
state. Such a use of @scheme[reuse?] defeats certain guarantees of the
TCP protocol; see Stevens's book for details. Furthermore, on many
modern platforms, a true value for @scheme[reuse?] overrides
@tt{TIME_WAIT} only if the listener was previously created with a true
value for @scheme[reuse?].

If @scheme[hostname] is @scheme[#f] (the default), then the listener
accepts connections to all of the listening machine's addresses.
Otherwise, the listener accepts connections only at the interface(s)
associated with the given hostname. For example, providing
@scheme["127.0.0.1"] as @scheme[hostname] creates a listener that
accepts only connections to @scheme["127.0.0.1"] (the loopback
interface) from the local machine.

(Scheme implements a listener with multiple sockets, if necessary, to
accomodate multiple addresses with different protocol families. Under
Linux, if @scheme[hostname] maps to both IPv4 and IPv6 addresses, then
the behavior depends on whether IPv6 is supported and IPv6 sockets can
be configured to listen to only IPv6 connections: if IPv6 is not
supported or IPv6 sockets are not configurable, then the IPv6
addresses are ignored; otherwise, each IPv6 listener accepts only IPv6
connections.)

The return value of @scheme[tcp-listen] is a @deftech{TCP
listener}. This value can be used in future calls to
@scheme[tcp-accept], @scheme[tcp-accept-ready?], and
@scheme[tcp-close].  Each new TCP listener value is placed into the
management of the current custodian (see @secref["custodians"]).

If the server cannot be started by @scheme[tcp-listen], the
@exnraise[exn:fail:network].}


@defproc[(tcp-connect [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? #f) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           #f)
                                     #f])
          (values input-port? output-port?)]{

Attempts to connect as a client to a listening server.  The
@scheme[hostname] argument is the server host's Internet address name,
and @scheme[port-no] is the port number where the server is listening.

(If @scheme[hostname] is associated with multiple addresses, they are
tried one at a time until a connection succeeds. The name
@scheme["localhost"] generally specifies the local machine.)

The optional @scheme[local-hostname] and @scheme[local-port-no]
specify the client's address and port. If both are @scheme[#f] (the
default), the client's address and port are selected automatically. If
@scheme[local-hostname] is not @scheme[#f], then
@scheme[local-port-no] must be non-@scheme[#f]. If
@scheme[local-port-no] is non-@scheme[#f] and @scheme[local-hostname]
is @scheme[#f], then the given port is used but the address is
selected automatically.

Two values are returned by @scheme[tcp-connect]: an input port and an
output port. Data can be received from the server through the input
port and sent to the server through the output port.  If the server is
a @exec{mzscheme} process, it can obtain ports to communicate to the
client with @scheme[tcp-accept].  These ports are placed into the
management of the current custodian (see @secref["custodians"]).

Initially, the returned input port is block-buffered, and the returned
output port is block-buffered. Change the buffer mode using
@scheme[file-stream-buffer-mode].

Both of the returned ports must be closed to terminate the TCP
connection. When both ports are still open, closing the output port
with @scheme[close-output-port] sends a TCP close to the server (which
is seen as an end-of-file if the server reads the connection through a
port). In contrast, @scheme[tcp-abandon-port] (see below) closes the
output port, but does not send a TCP close until the input port is
also closed.

Note that the TCP protocol does not support a state where one end is
willing to send but not read, nor does it include an automatic message
when one end of a connection is fully closed. Instead, the other end
of a connection discovers that one end is fully closed only as a
response to sending data; in particular, some number of writes on the
still-open end may appear to succeed, though writes will eventually
produce an error.

If a connection cannot be established by @scheme[tcp-connect], the
@exnraise[exn:fail:network].}

@defproc[(tcp-connect/enable-break [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? #f) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           #f)])
          (values input-port? output-port?)]{

Like @scheme[tcp-connect], but breaking is enabled (see
@secref["breakhandler"]) while trying to connect. If breaking is
disabled when @scheme[tcp-connect/enable-break] is called, then either
ports are returned or the @scheme[exn:break] exception is raised, but
not both.}

@defproc[(tcp-accept [listener tcp-listener?])
         (values input-port? output-port?)]{

Accepts a client connection for the server associated with
@scheme[listener]. If no client connection is waiting on the
listening port, the call to @scheme[tcp-accept] will block. (See also
@scheme[tcp-accept-ready?].)

Two values are returned by @scheme[tcp-accept]: an input port and an
output port. Data can be received from the client through the input
port and sent to the client through the output port.  These ports are
placed into the management of the current custodian (see
@secref["custodians"]).

In terms of buffering and connection states, the ports act the same as
ports from @scheme[tcp-connect].

If a connection cannot be accepted by @scheme[tcp-accept], or if the
listener has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-accept/enable-break [listener tcp-listener?])
         (values input-port? output-port?)]{

Like @scheme[tcp-accept], but breaking is enabled (see
@secref["breakhandler"]) while trying to accept a connection. If
breaking is disabled when @scheme[tcp-accept/enable-break] is called,
then either ports are returned or the @scheme[exn:break] exception is
raised, but not both.}


@defproc[(tcp-accept-ready? [listener tcp-listener?]) boolean?]{

Tests whether an unaccepted client has connected to the server
associated with @scheme[listener]. If a client is
waiting, the return value is @scheme[#t], otherwise it is
@scheme[#f]. A client is accepted with the @scheme[tcp-accept]
procedure, which returns ports for communicating with the client and
removes the client from the list of unaccepted clients.

If the listener has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-close [listener tcp-listener?]) void?]{

Shuts down the server associated with @scheme[listener]. All
unaccepted clients receive an end-of-file from the server; connections
to accepted clients are unaffected.

If the listener has already been closed, the @exnraise[exn:fail:network].

The listener's port number may not become immediately available for
new listeners (with the default @scheme[reuse?] argument of
@scheme[tcp-listen]). For further information, see Stevens's
explanation of the @tt{TIME_WAIT} TCP state.}


@defproc[(tcp-listener? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{TCP listener} created by
@scheme[tcp-listen], @scheme[#f] otherwise.}


@defproc[(tcp-accept-evt [listener tcp-listener?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is in
a blocking state when @scheme[tcp-accept] on @scheme[listener] would
block. If the event is chosen in a synchronization, the result is a
list of two items, which correspond to the two results of
@scheme[tcp-accept]. (If the event is not chosen, no connections are
accepted.) The ports are placed into the management of the custodian
that is the current custodian (see @secref["custodians"]) at the time that
@scheme[tcp-accept-evt] is called.}


@defproc[(tcp-abandon-port [tcp-port tcp-port?]) void?]{

Like @scheme[close-output-port] or @scheme[close-input-port]
(depending on whether @scheme[tcp-port] is an input or output port),
but if @scheme[tcp-port] is an output port and its associated input
port is not yet closed, then the other end of the TCP connection does
not receive a TCP close message until the input port is also
closed.

The TCP protocol does not include a ``no longer reading'' state on
connections, so @scheme[tcp-abandon-port] is equivalent to
@scheme[close-input-port] on input @tech{TCP ports}.}


@defproc[(tcp-addresses [tcp-port (or/c tcp-port? tcp-listener?)]
                        [port-numbers? any/c #f]) 
         (or/c (values string? string?)
               (values string? (integer-in 1 65535) 
                       string? (integer-in 1 65535)))]{

Returns two strings when @scheme[port-numbers?] is @scheme[#f] (the
default). The first string is the Internet address for the local
machine a viewed by the given @tech{TCP port}'s connection. (For most
machines, the answer corresponds to the current machine's only
Internet address, but when a machine serves multiple addresses, the
result is connection-specific.) The second string is the Internet
address for the other end of the connection.

If @scheme[port-numbers?] is true, then four results are returned: a
string for the local machine's address, an exact integer between
@scheme[1] and @scheme[65535] for the local machine's port number, a
string for the remote machine's address, and an exact integer between
@scheme[1] and @scheme[65535] for the remote machine's port number.

If the given port has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-port? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @deftech{TCP port}---which is a
port returned by @scheme[tcp-accept], @scheme[tcp-connect],
@scheme[tcp-accept/enable-break], or
@scheme[tcp-connect/enable-break]---@scheme[#f] otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "udp"]{UDP}

@note-lib[racket/udp]

For information about UDP in general, see @italic{TCP/IP Illustrated,
Volume 1} by W. Richard Stevens.

@defproc[(udp-open-socket [family-hostname (or/c string? #f) #f]
                          [family-port-no (or/c string? #f) #f])
         udp?]{

Creates and returns a @deftech{UDP socket} to send and receive
datagrams (broadcasting is allowed). Initially, the socket is not
bound or connected to any address or port.

If @scheme[family-hostname] or @scheme[family-port-no] is not
@scheme[#f], then the socket's protocol family is determined from
these arguments. The socket is @italic{not} bound to the hostname
or port number. For example, the arguments might be the hostname
and port to which messages will be sent through the socket, which
ensures that the socket's protocol family is consistent with the
destination. Alternately, the arguments might be the same as for
a future call to @scheme[udp-bind!], which ensures that the
socket's protocol family is consistent with the binding. If
neither @scheme[family-hostname] nor @scheme[family-port-no] is
non-@scheme[#f], then the socket's protocol family is IPv4.}

@defproc[(udp-bind! [udp-socket udp?]
                    [hostname-string (or/c string? #f)]
                    [port-no (and/c exact-nonnegative-integer?
                                    (integer-in 0 65535))])
         void?]{

Binds an unbound @scheme[udp-socket] to the local port number
@scheme[port-no].  If @scheme[port-no] is 0 the @scheme[udp-socket] is
bound to an ephemeral port, which can be determined by calling
@scheme[udp-addresses].

If @scheme[hostname-string] is @scheme[#f], then the socket
accepts connections to all of the listening machine's IP
addresses at @scheme[port-no]. Otherwise, the socket accepts
connections only at the IP address associated with the given
name. For example, providing @scheme["127.0.0.1"] as
@scheme[hostname-string] typically creates a listener that
accepts only connections to @scheme["127.0.0.1"] from the local
machine.

A socket cannot receive datagrams until it is bound to a local address
and port. If a socket is not bound before it is used with a sending
procedure @scheme[udp-send], @scheme[udp-send-to], etc., the sending
procedure binds the socket to a random local port. Similarly, if an
event from @scheme[udp-send-evt] or @scheme[udp-send-to-evt] is chosen
for a synchronization (see @secref["sync"]), the socket is bound; if
the event is not chosen, the socket may or may not become bound. 

The binding of a bound socket cannot be changed, with one exception:
on some systems, if the socket is bound automatically when sending, if
the socket is disconnected via @scheme[udp-connect!], and if the
socket is later used again in a send, then the later send may change
the socket's automatic binding.

If @scheme[udp-socket] is already bound or closed, the
@exnraise[exn:fail:network].}


@defproc[(udp-connect! [udp-socket udp?]
                       [hostname-string (or/c string? #f)]
                       [port-no (or/c (and/c exact-nonnegative-integer?
                                             (integer-in 1 65535))
                                      #f)])
         void?]{

Connects the socket to the indicated remote address and port if
@scheme[hostname-string] is a string and @scheme[port-no] is an exact
integer.

If @scheme[hostname-string] is @scheme[#f], then @scheme[port-no] also
must be @scheme[#f], and the port is disconnected (if connected). If
one of @scheme[hostname-string] or @scheme[port-no] is @scheme[#f] and
the other is not, the @exnraise[exn:fail:contract].

A connected socket can be used with @scheme[udp-send] (not
@scheme[udp-send-to]), and it accepts datagrams only from the
connected address and port. A socket need not be connected to receive
datagrams.  A socket can be connected, re-connected, and disconnected
any number of times.

If @scheme[udp-socket] is closed, the @exnraise[exn:fail:network].}


@defproc[(udp-send-to [udp-socket udp?]
                      [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                      (integer-in 1 65535))]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Sends @scheme[(subbytes bytes start-pos end-pos)] as a datagram from
the unconnected @scheme[udp-socket] to the socket at the remote
machine @scheme[hostname-address] on the port @scheme[port-no]. The
@scheme[udp-socket] need not be bound or connected; if it is not
bound, @scheme[udp-send-to] binds it to a random local port. If the
socket's outgoing datagram queue is too full to support the send,
@scheme[udp-send-to] blocks until the datagram can be queued.

If @scheme[start-pos] is greater than the length of @scheme[bstr], or
if @scheme[end-pos] is less than @scheme[start-pos] or greater than
the length of @scheme[bstr], the @exnraise[exn:fail:contract].

If @scheme[udp-socket] is closed or connected, the
@exnraise[exn:fail:network].}

@defproc[(udp-send [udp-socket udp?]
                   [bstr bytes?]
                   [start-pos exact-nonnegative-integer? 0]
                   [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @scheme[udp-send-to], except that @scheme[udp-socket] must be
connected, and the datagram goes to the connection target.  If
@scheme[udp-socket] is closed or unconnected, the
@exnraise[exn:fail:network].}

@defproc[(udp-send-to* [udp-socket udp?]
                       [hostname string?]
                       [port-no (and/c exact-nonnegative-integer?
                                       (integer-in 1 65535))]
                       [bstr bytes?]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         boolean?]{

Like @scheme[udp-send-to], but never blocks; if the socket's outgoing
queue is too full to support the send, @scheme[#f] is returned,
otherwise the datagram is queued and the result is @scheme[#t].}

@defproc[(udp-send* [udp-socket udp?]
                    [bstr bytes?]
                    [start-pos exact-nonnegative-integer? 0]
                    [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         boolean?]{

Like @scheme[udp-send], except that (like @scheme[udp-send-to]) it
never blocks and returns @scheme[#f] or @scheme[#t].}

@defproc[(udp-send-to/enable-break [udp-socket udp?]
                      [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                      (integer-in 1 65535))]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @scheme[udp-send-to], but breaking is enabled (see
@secref["breakhandler"]) while trying to send the datagram. If
breaking is disabled when @scheme[udp-send-to/enable-break] is called,
then either the datagram is sent or the @scheme[exn:break] exception
is raised, but not both.}


@defproc[(udp-send/enable-break [udp-socket udp?]
                   [bstr bytes?]
                   [start-pos exact-nonnegative-integer? 0]
                   [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @scheme[udp-send], except that breaks are enabled like
@scheme[udp-send-to/enable-break].}


@defproc[(udp-receive! [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values exact-nonnegative-integer?
                 string?
                 (integer-in 1 65535))]{

Accepts up to @math{@scheme[end-pos]-@scheme[start-pos]} bytes of
@scheme[udp-socket]'s next incoming datagram into @scheme[bstr],
writing the datagram bytes starting at position @scheme[start-pos]
within @scheme[bstr]. The @scheme[udp-socket] must be bound to a local
address and port (but need not be connected). If no incoming datagram
is immediately available, @scheme[udp-receive!] blocks until one is
available.

Three values are returned: the number of received bytes (between
@scheme[0] and @math{@scheme[end-pos]-@scheme[start-pos]}, a hostname
string indicating the source address of the datagram, and an integer
indicating the source port of the datagram. If the received datagram
is longer than @math{@scheme[end-pos]-@scheme[start-pos]} bytes, the
remainder is discarded.

If @scheme[start-pos] is greater than the length of @scheme[bstr], or
if @scheme[end-pos] is less than @scheme[start-pos] or greater than
the length of @scheme[bstr], the @exnraise[exn:fail:contract].}

@defproc[(udp-receive!* [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values (or/c exact-nonnegative-integer? #f)
                 (or/c string? #f)
                 (or/c (integer-in 1 65535) #f))]{

Like @scheme[udp-receive!], except that it never blocks. If no
datagram is available, the three result values are all @scheme[#f].}

@defproc[(udp-receive!/enable-break [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values exact-nonnegative-integer?
                 string?
                 (integer-in 1 65535))]{

Like @scheme[udp-receive!], but breaking is enabled (see
@secref["breakhandler"]) while trying to receive the datagram. If
breaking is disabled when @scheme[udp-receive!/enable-break] is
called, then either a datagram is received or the @scheme[exn:break]
exception is raised, but not both.}


@defproc[(udp-close [udp-socket udp?]) void?]{

Closes @scheme[udp-socket], discarding unreceived datagrams.  If the
socket is already closed, the @exnraise[exn:fail:network].}


@defproc[(udp? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a socket created by
@scheme[udp-open-socket], @scheme[#f] otherwise.}


@defproc[(udp-bound? [udp-socket udp?]) boolean?]{

Returns @scheme[#t] if @scheme[udp-socket] is bound to a local address
and port, @scheme[#f] otherwise.}


@defproc[(udp-connected? [udp-socket udp?]) boolean?]{

Returns @scheme[#t] if @scheme[udp-socket] is connected to a remote
address and port, @scheme[#f] otherwise.}


@defproc[(udp-send-ready-evt [udp-socket udp?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
in a blocking state when @scheme[udp-send-to] on @scheme[udp-socket]
would block.}


@defproc[(udp-receive-ready-evt [udp-socket udp?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
in a blocking state when @scheme[udp-receive!] on @scheme[udp-socket]
would block.}

@defproc[(udp-send-to-evt [udp-socket udp?]
                      [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                      (integer-in 1 65535))]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         evt?]{

Returns a @tech{synchronizable event}. The event is in a blocking
state when @scheme[udp-send-to] on @scheme[udp-socket] would
block. Otherwise, if the event is chosen in a synchronization, data is
sent as for @scheme[(udp-send-to udp-socket hostname-address port-no
bstr start-pos end-pos)], and the synchronization result is
@|void-const|. (No bytes are sent if the event is not chosen.)}


@defproc[(udp-send-evt [udp-socket udp?]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         evt?]{

Returns a @tech{synchronizable event}. The event is in a blocking
state when @scheme[udp-send] on @scheme[udp-socket] would
block. Otherwise, if the event is chosen in a synchronization, data is
sent as for @scheme[(udp-send-to udp-socket bstr start-pos end-pos)],
and the synchronization result is @|void-const|. (No bytes are sent if
the event is not chosen.) If @scheme[udp-socket] is closed or
unconnected, the @exnraise[exn:fail:network] during a synchronization
attempt.}

@defproc[(udp-receive!-evt [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         evt?]{

Returns a @tech{synchronizable event}. The event is in a blocking
state when @scheme[udp-receive] on @scheme[udp-socket] would
block. Otherwise, if the event is chosen in a synchronization, data is
received into @scheme[bstr] as for @scheme[(udp-receive! udp-socket
bytes start-pos end-pos)], and the synchronization result is a list of
three values, corresponding to the three results from
@scheme[udp-receive!]. (No bytes are received and the @scheme[bstr]
content is not modified if the event is not chosen.)}

@defproc[(udp-addresses [udp-port udp?]
                        [port-numbers? any/c #f]) 
         (or/c (values string? string?)
               (values string? (integer-in 1 65535) 
                       string? (integer-in 1 65535)))]{

Returns two strings when @scheme[port-numbers?] is @scheme[#f] (the
default). The first string is the Internet address for the local
machine a viewed by the given @tech{UDP socket}'s connection. (For most
machines, the answer corresponds to the current machine's only
Internet address, but when a machine serves multiple addresses, the
result is connection-specific.) The second string is the Internet
address for the other end of the connection.

If @scheme[port-numbers?] is true, then four results are returned: a
string for the local machine's address, an exact integer between
@scheme[1] and @scheme[65535] for the local machine's port number, a
string for the remote machine's address, and an exact integer between
@scheme[1] and @scheme[65535] for the remote machine's port number.

If the given port has been closed, the @exnraise[exn:fail:network].}
