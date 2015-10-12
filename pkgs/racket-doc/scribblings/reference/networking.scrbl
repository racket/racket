#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "networking" #:style 'toc]{Networking}

@local-table-of-contents[]

@;------------------------------------------------------------------------
@section[#:tag "tcp"]{TCP}

@note-lib[racket/tcp]

For information about TCP in general, see @italic{TCP/IP Illustrated,
 Volume 1} by W. Richard Stevens.

@defproc[(tcp-listen [port-no listen-port-number?]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? #f) #f]) 
         tcp-listener?]{

Creates a ``listening'' server on the local machine at the port number
specified by @racket[port-no]. If @racket[port-no] is 0 the socket binds
to an ephemeral port, which can be determined by calling 
@racket[tcp-addresses].  The @racket[max-allow-wait] argument
determines the maximum number of client connections that can be
waiting for acceptance. (When @racket[max-allow-wait] clients are
waiting acceptance, no new client connections can be made.)

If the @racket[reuse?] argument is true, then @racket[tcp-listen] will
create a listener even if the port is involved in a @tt{TIME_WAIT}
state. Such a use of @racket[reuse?] defeats certain guarantees of the
TCP protocol; see Stevens's book for details. Furthermore, on many
modern platforms, a true value for @racket[reuse?] overrides
@tt{TIME_WAIT} only if the listener was previously created with a true
value for @racket[reuse?].

If @racket[hostname] is @racket[#f] (the default), then the listener
accepts connections to all of the listening machine's addresses.
Otherwise, the listener accepts connections only at the interface(s)
associated with the given hostname. For example, providing
@racket["127.0.0.1"] as @racket[hostname] creates a listener that
accepts only connections to @racket["127.0.0.1"] (the loopback
interface) from the local machine.

(Racket implements a listener with multiple sockets, if necessary, to
accommodate multiple addresses with different protocol families. On
Linux, if @racket[hostname] maps to both IPv4 and IPv6 addresses, then
the behavior depends on whether IPv6 is supported and IPv6 sockets can
be configured to listen to only IPv6 connections: if IPv6 is not
supported or IPv6 sockets are not configurable, then the IPv6
addresses are ignored; otherwise, each IPv6 listener accepts only IPv6
connections.)

The return value of @racket[tcp-listen] is a @deftech{TCP
listener}. This value can be used in future calls to
@racket[tcp-accept], @racket[tcp-accept-ready?], and
@racket[tcp-close].  Each new TCP listener value is placed into the
management of the current custodian (see @secref["custodians"]).

If the server cannot be started by @racket[tcp-listen], the
@exnraise[exn:fail:network].

A TCP listener can be used as a @tech{synchronizable event} (see @secref["sync"]).
A TCP listener is @tech{ready for synchronization} when
@racket[tcp-accept] would not block; @resultItself{TCP listener}.}


@defproc[(tcp-connect [hostname string?]
                      [port-no port-number?]
                      [local-hostname (or/c string? #f) #f]
                      [local-port-no (or/c port-number? #f)
                                     #f])
          (values input-port? output-port?)]{

Attempts to connect as a client to a listening server.  The
@racket[hostname] argument is the server host's Internet address name,
and @racket[port-no] is the port number where the server is listening.

(If @racket[hostname] is associated with multiple addresses, they are
tried one at a time until a connection succeeds. The name
@racket["localhost"] generally specifies the local machine.)

The optional @racket[local-hostname] and @racket[local-port-no]
specify the client's address and port. If both are @racket[#f] (the
default), the client's address and port are selected automatically. If
@racket[local-hostname] is not @racket[#f], then
@racket[local-port-no] must be non-@racket[#f]. If
@racket[local-port-no] is non-@racket[#f] and @racket[local-hostname]
is @racket[#f], then the given port is used but the address is
selected automatically.

Two values are returned by @racket[tcp-connect]: an input port and an
output port. Data can be received from the server through the input
port and sent to the server through the output port.  If the server is
a Racket program, it can obtain ports to communicate to the
client with @racket[tcp-accept].  These ports are placed into the
management of the current custodian (see @secref["custodians"]).

Initially, the returned input port is block-buffered, and the returned
output port is block-buffered. Change the buffer mode using
@racket[file-stream-buffer-mode].

Both of the returned ports must be closed to terminate the TCP
connection. When both ports are still open, closing the output port
with @racket[close-output-port] sends a TCP close to the server (which
is seen as an end-of-file if the server reads the connection through a
port). In contrast, @racket[tcp-abandon-port] (see below) closes the
output port, but does not send a TCP close until the input port is
also closed.

Note that the TCP protocol does not support a state where one end is
willing to send but not read, nor does it include an automatic message
when one end of a connection is fully closed. Instead, the other end
of a connection discovers that one end is fully closed only as a
response to sending data; in particular, some number of writes on the
still-open end may appear to succeed, though writes will eventually
produce an error.

If a connection cannot be established by @racket[tcp-connect], the
@exnraise[exn:fail:network].}

@defproc[(tcp-connect/enable-break [hostname string?]
                      [port-no port-number?]
                      [local-hostname (or/c string? #f) #f]
                      [local-port-no (or/c port-number? #f)])
          (values input-port? output-port?)]{

Like @racket[tcp-connect], but breaking is enabled (see
@secref["breakhandler"]) while trying to connect. If breaking is
disabled when @racket[tcp-connect/enable-break] is called, then either
ports are returned or the @racket[exn:break] exception is raised, but
not both.}

@defproc[(tcp-accept [listener tcp-listener?])
         (values input-port? output-port?)]{

Accepts a client connection for the server associated with
@racket[listener]. If no client connection is waiting on the
listening port, the call to @racket[tcp-accept] will block. (See also
@racket[tcp-accept-ready?].)

Two values are returned by @racket[tcp-accept]: an input port and an
output port. Data can be received from the client through the input
port and sent to the client through the output port.  These ports are
placed into the management of the current custodian (see
@secref["custodians"]).

In terms of buffering and connection states, the ports act the same as
ports from @racket[tcp-connect].

If a connection cannot be accepted by @racket[tcp-accept], or if the
listener has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-accept/enable-break [listener tcp-listener?])
         (values input-port? output-port?)]{

Like @racket[tcp-accept], but breaking is enabled (see
@secref["breakhandler"]) while trying to accept a connection. If
breaking is disabled when @racket[tcp-accept/enable-break] is called,
then either ports are returned or the @racket[exn:break] exception is
raised, but not both.}


@defproc[(tcp-accept-ready? [listener tcp-listener?]) boolean?]{

Tests whether an unaccepted client has connected to the server
associated with @racket[listener]. If a client is
waiting, the return value is @racket[#t], otherwise it is
@racket[#f]. A client is accepted with the @racket[tcp-accept]
procedure, which returns ports for communicating with the client and
removes the client from the list of unaccepted clients.

If the listener has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-close [listener tcp-listener?]) void?]{

Shuts down the server associated with @racket[listener]. All
unaccepted clients receive an end-of-file from the server; connections
to accepted clients are unaffected.

If the listener has already been closed, the @exnraise[exn:fail:network].

The listener's port number may not become immediately available for
new listeners (with the default @racket[_reuse?] argument of
@racket[tcp-listen]). For further information, see Stevens's
explanation of the @tt{TIME_WAIT} TCP state.}


@defproc[(tcp-listener? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{TCP listener} created by
@racket[tcp-listen], @racket[#f] otherwise.}


@defproc[(tcp-accept-evt [listener tcp-listener?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
@tech{ready for synchronization} when @racket[tcp-accept] on @racket[listener] would
not block. The @tech{synchronization result} is a
list of two items, which correspond to the two results of
@racket[tcp-accept]. (If the event is not chosen in a @racket[syntax], no connections are
accepted.) The ports are placed into the management of the custodian
that is the current custodian (see @secref["custodians"]) at the time that
@racket[tcp-accept-evt] is called.}


@defproc[(tcp-abandon-port [tcp-port tcp-port?]) void?]{

Like @racket[close-output-port] or @racket[close-input-port]
(depending on whether @racket[tcp-port] is an input or output port),
but if @racket[tcp-port] is an output port and its associated input
port is not yet closed, then the other end of the TCP connection does
not receive a TCP close message until the input port is also
closed.

The TCP protocol does not include a ``no longer reading'' state on
connections, so @racket[tcp-abandon-port] is equivalent to
@racket[close-input-port] on input @tech{TCP ports}.}


@defproc[(tcp-addresses [tcp-port (or/c tcp-port? tcp-listener?)]
                        [port-numbers? any/c #f]) 
         (or/c (values string? string?)
               (values string? port-number?
                       string? listen-port-number?))]{

Returns two strings when @racket[port-numbers?] is @racket[#f] (the
default). The first string is the Internet address for the local
machine a viewed by the given @tech{TCP port}'s connection or for the
TCP listener. (For most machines, the answer corresponds to the
current machine's only Internet address, but when a machine serves
multiple addresses, the result is connection-specific or
listener-specific.) If a listener is given and it has no specific
host, the first string result is @racket["0.0.0.0"]. The second string
is the Internet address for the other end of the connection, or always
@racket["0.0.0.0"] for a listener.

If @racket[port-numbers?] is true, then four results are returned: a
string for the local machine's address, an exact integer between
@racket[1] and @racket[65535] for the local machine's port number, a
string for the remote machine's address, and an exact integer between
@racket[1] and @racket[65535] for the remote machine's port number or
@racket[0] for a listener.

If the given port has been closed, the @exnraise[exn:fail:network].}


@defproc[(tcp-port? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @deftech{TCP port}---which is a
port returned by @racket[tcp-accept], @racket[tcp-connect],
@racket[tcp-accept/enable-break], or
@racket[tcp-connect/enable-break]---@racket[#f] otherwise.}

@defthing[port-number? contract?]{
Equivalent to @racket[(between/c 1 65535)].

@history[#:added "6.3"]{}
}

@defthing[listen-port-number? contract?]{
Equivalent to @racket[(between/c 0 65535)].

@history[#:added "6.3"]{}
}

@;------------------------------------------------------------------------
@section[#:tag "udp"]{UDP}

@note-lib[racket/udp]

For information about UDP in general, see @italic{TCP/IP Illustrated,
Volume 1} by W. Richard Stevens.

@defproc[(udp-open-socket [family-hostname (or/c string? #f) #f]
                          [family-port-no (or/c port-number? #f) #f])
         udp?]{

Creates and returns a @deftech{UDP socket} to send and receive
datagrams (broadcasting is allowed). Initially, the socket is not
bound or connected to any address or port.

If @racket[family-hostname] or @racket[family-port-no] is not
@racket[#f], then the socket's protocol family is determined from
these arguments. The socket is @italic{not} bound to the hostname
or port number. For example, the arguments might be the hostname
and port to which messages will be sent through the socket, which
ensures that the socket's protocol family is consistent with the
destination. Alternately, the arguments might be the same as for
a future call to @racket[udp-bind!], which ensures that the
socket's protocol family is consistent with the binding. If
neither @racket[family-hostname] nor @racket[family-port-no] is
non-@racket[#f], then the socket's protocol family is IPv4.}

@defproc[(udp-bind! [udp-socket udp?]
                    [hostname-string (or/c string? #f)]
                    [port-no listen-port-number?]
		    [reuse? any/c #f])
         void?]{

Binds an unbound @racket[udp-socket] to the local port number
@racket[port-no].  If @racket[port-no] is 0 the @racket[udp-socket] is
bound to an ephemeral port, which can be determined by calling
@racket[udp-addresses].

If @racket[hostname-string] is @racket[#f], then the socket
accepts connections to all of the listening machine's IP
addresses at @racket[port-no]. Otherwise, the socket accepts
connections only at the IP address associated with the given
name. For example, providing @racket["127.0.0.1"] as
@racket[hostname-string] typically creates a listener that
accepts only connections to @racket["127.0.0.1"] from the local
machine.

A socket cannot receive datagrams until it is bound to a local address
and port. If a socket is not bound before it is used with a sending
procedure @racket[udp-send], @racket[udp-send-to], etc., the sending
procedure binds the socket to a random local port. Similarly, if an
event from @racket[udp-send-evt] or @racket[udp-send-to-evt] is chosen
for a synchronization (see @secref["sync"]), the socket is bound; if
the event is not chosen, the socket may or may not become bound. 

The binding of a bound socket cannot be changed, with one exception:
on some systems, if the socket is bound automatically when sending, if
the socket is disconnected via @racket[udp-connect!], and if the
socket is later used again in a send, then the later send may change
the socket's automatic binding.

If @racket[udp-socket] is already bound or closed, the
@exnraise[exn:fail:network].

If the @racket[reuse?] argument is true, then @racket[udp-bind!] will
set the @tt{SO_REUSEADDR} socket option before binding, permitting the
sharing of access to a UDP port between many processes on a single
machine when using UDP multicast.}

@defproc[(udp-connect! [udp-socket udp?]
                       [hostname-string (or/c string? #f)]
                       [port-no (or/c port-number? #f)])
         void?]{

Connects the socket to the indicated remote address and port if
@racket[hostname-string] is a string and @racket[port-no] is an exact
integer.

If @racket[hostname-string] is @racket[#f], then @racket[port-no] also
must be @racket[#f], and the port is disconnected (if connected). If
one of @racket[hostname-string] or @racket[port-no] is @racket[#f] and
the other is not, the @exnraise[exn:fail:contract].

A connected socket can be used with @racket[udp-send] (not
@racket[udp-send-to]), and it accepts datagrams only from the
connected address and port. A socket need not be connected to receive
datagrams.  A socket can be connected, re-connected, and disconnected
any number of times.

If @racket[udp-socket] is closed, the @exnraise[exn:fail:network].}


@defproc[(udp-send-to [udp-socket udp?]
                      [hostname string?]
                      [port-no port-number?]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Sends @racket[(subbytes bytes start-pos end-pos)] as a datagram from
the unconnected @racket[udp-socket] to the socket at the remote
machine @racket[hostname-address] on the port @racket[port-no]. The
@racket[udp-socket] need not be bound or connected; if it is not
bound, @racket[udp-send-to] binds it to a random local port. If the
socket's outgoing datagram queue is too full to support the send,
@racket[udp-send-to] blocks until the datagram can be queued.

If @racket[start-pos] is greater than the length of @racket[bstr], or
if @racket[end-pos] is less than @racket[start-pos] or greater than
the length of @racket[bstr], the @exnraise[exn:fail:contract].

If @racket[udp-socket] is closed or connected, the
@exnraise[exn:fail:network].}

@defproc[(udp-send [udp-socket udp?]
                   [bstr bytes?]
                   [start-pos exact-nonnegative-integer? 0]
                   [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @racket[udp-send-to], except that @racket[udp-socket] must be
connected, and the datagram goes to the connection target.  If
@racket[udp-socket] is closed or unconnected, the
@exnraise[exn:fail:network].}

@defproc[(udp-send-to* [udp-socket udp?]
                       [hostname string?]
                       [port-no port-number?]
                       [bstr bytes?]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         boolean?]{

Like @racket[udp-send-to], but never blocks; if the socket's outgoing
queue is too full to support the send, @racket[#f] is returned,
otherwise the datagram is queued and the result is @racket[#t].}

@defproc[(udp-send* [udp-socket udp?]
                    [bstr bytes?]
                    [start-pos exact-nonnegative-integer? 0]
                    [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         boolean?]{

Like @racket[udp-send], except that (like @racket[udp-send-to]) it
never blocks and returns @racket[#f] or @racket[#t].}

@defproc[(udp-send-to/enable-break [udp-socket udp?]
                      [hostname string?]
                      [port-no port-number?]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @racket[udp-send-to], but breaking is enabled (see
@secref["breakhandler"]) while trying to send the datagram. If
breaking is disabled when @racket[udp-send-to/enable-break] is called,
then either the datagram is sent or the @racket[exn:break] exception
is raised, but not both.}


@defproc[(udp-send/enable-break [udp-socket udp?]
                   [bstr bytes?]
                   [start-pos exact-nonnegative-integer? 0]
                   [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         void]{

Like @racket[udp-send], except that breaks are enabled like
@racket[udp-send-to/enable-break].}


@defproc[(udp-receive! [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values exact-nonnegative-integer?
                 string?
                 port-number?)]{

Accepts up to @math{@racket[end-pos]-@racket[start-pos]} bytes of
@racket[udp-socket]'s next incoming datagram into @racket[bstr],
writing the datagram bytes starting at position @racket[start-pos]
within @racket[bstr]. The @racket[udp-socket] must be bound to a local
address and port (but need not be connected). If no incoming datagram
is immediately available, @racket[udp-receive!] blocks until one is
available.

Three values are returned: the number of received bytes (between
@racket[0] and @math{@racket[end-pos]-@racket[start-pos]}, a hostname
string indicating the source address of the datagram, and an integer
indicating the source port of the datagram. If the received datagram
is longer than @math{@racket[end-pos]-@racket[start-pos]} bytes, the
remainder is discarded.

If @racket[start-pos] is greater than the length of @racket[bstr], or
if @racket[end-pos] is less than @racket[start-pos] or greater than
the length of @racket[bstr], the @exnraise[exn:fail:contract].}

@defproc[(udp-receive!* [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values (or/c exact-nonnegative-integer? #f)
                 (or/c string? #f)
                 (or/c port-number? #f))]{

Like @racket[udp-receive!], except that it never blocks. If no
datagram is available, the three result values are all @racket[#f].}

@defproc[(udp-receive!/enable-break [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (values exact-nonnegative-integer?
                 string?
                 port-number?)]{

Like @racket[udp-receive!], but breaking is enabled (see
@secref["breakhandler"]) while trying to receive the datagram. If
breaking is disabled when @racket[udp-receive!/enable-break] is
called, then either a datagram is received or the @racket[exn:break]
exception is raised, but not both.}


@defproc[(udp-close [udp-socket udp?]) void?]{

Closes @racket[udp-socket], discarding unreceived datagrams.  If the
socket is already closed, the @exnraise[exn:fail:network].}


@defproc[(udp? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a socket created by
@racket[udp-open-socket], @racket[#f] otherwise.}


@defproc[(udp-bound? [udp-socket udp?]) boolean?]{

Returns @racket[#t] if @racket[udp-socket] is bound to a local address
and port, @racket[#f] otherwise.}


@defproc[(udp-connected? [udp-socket udp?]) boolean?]{

Returns @racket[#t] if @racket[udp-socket] is connected to a remote
address and port, @racket[#f] otherwise.}


@defproc[(udp-send-ready-evt [udp-socket udp?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
in a blocking state when @racket[udp-send-to] on @racket[udp-socket]
would block. The @tech{synchronization result} is the event itself.
}


@defproc[(udp-receive-ready-evt [udp-socket udp?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
in a blocking state when @racket[udp-receive!] on @racket[udp-socket]
would block. The @tech{synchronization result} is the event itself.
}

@defproc[(udp-send-to-evt [udp-socket udp?]
                      [hostname string?]
                      [port-no port-number?]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         evt?]{

Returns a @tech{synchronizable event}. The event is in a blocking
state when @racket[udp-send-to] on @racket[udp-socket] would
block. Otherwise, if the event is chosen in a synchronization, data is
sent as for @racket[(udp-send-to udp-socket hostname-address port-no
bstr start-pos end-pos)], and the synchronization result is
@|void-const|. (No bytes are sent if the event is not chosen.)}


@defproc[(udp-send-evt [udp-socket udp?]
                      [bstr bytes?]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         evt?]{

Returns a @tech{synchronizable event}. The event is @tech{ready for synchronization}
when @racket[udp-send] on @racket[udp-socket] would
not block. Otherwise, if the event is chosen in a synchronization, data is
sent as for @racket[(udp-send-to udp-socket bstr start-pos end-pos)],
and the @tech{synchronization result} is @|void-const|. (No bytes are sent if
the event is not chosen.) If @racket[udp-socket] is closed or
unconnected, the @exnraise[exn:fail:network] during a synchronization
attempt.}

@defproc[(udp-receive!-evt [udp-socket udp?]
                       [bstr (and/c bytes? (not immutable?))]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         evt?]{

Returns a @tech{synchronizable event}. The event is @tech{ready for synchronization}
when @racket[udp-receive] on @racket[udp-socket] would not
block. Otherwise, if the event is chosen in a synchronization, data is
received into @racket[bstr] as for @racket[(udp-receive! udp-socket
bytes start-pos end-pos)], and the @tech{synchronization result} is a list of
three values, corresponding to the three results from
@racket[udp-receive!]. (No bytes are received and the @racket[bstr]
content is not modified if the event is not chosen.)}

@defproc[(udp-addresses [udp-port udp?]
                        [port-numbers? any/c #f]) 
         (or/c (values string? string?)
               (values string? listen-port-number?
                       string? listen-port-number?))]{

Returns two strings when @racket[port-numbers?] is @racket[#f] (the
default). The first string is the Internet address for the local
machine a viewed by the given @tech{UDP socket}'s connection. (For most
machines, the answer corresponds to the current machine's only
Internet address, but when a machine serves multiple addresses, the
result is connection-specific.) The second string is the Internet
address for the other end of the connection.

If @racket[port-numbers?] is true, then four results are returned: a
string for the local machine's address, an exact integer between
@racket[1] and @racket[65535] for the local machine's port number 
or @racket[0] if the socket is unbound, a
string for the remote machine's address, and an exact integer between
@racket[1] and @racket[65535] for the remote machine's port number 
or @racket[0] if the socket is unconnected.

If the given port has been closed, the @exnraise[exn:fail:network].}


@deftogether[(
@defproc[(udp-multicast-join-group! [udp-socket udp?]
				    [multicast-addr string?]
				    [hostname (or/c string? #f)]) void?]
@defproc[(udp-multicast-leave-group! [udp-socket udp?]
				     [multicast-addr string?]
				     [hostname (or/c string? #f)]) void?]
)]{
Adds or removes @racket[udp-socket] to a named multicast group.

The @racket[multicast-addr] argument must be a valid IPv4 multicast
IP address; for example, @racket["224.0.0.251"] is the appropriate
address for the mDNS protocol. The @racket[hostname] argument selects the
interface that the socket uses to receive (not send) multicast datagrams;
if @racket[hostname] is @racket[#f] or @racket["0.0.0.0"], the kernel
selects an interface automatically.

Leaving a group requires the same @racket[multicast-addr] and
@racket[hostname] arguments that were used to join the group.}



@deftogether[(
@defproc[(udp-multicast-interface [udp-socket udp?]) string?]
@defproc[(udp-multicast-set-interface! [udp-socket udp?]
				       [hostname (or/c string? #f)])
	void?]
)]{

Retrieves or sets the interface that @racket[udp-socket] uses to
send (not receive) multicast datagrams. If the result or @racket[hostname] is either
@racket[#f] or @racket["0.0.0.0"], the kernel automatically selects an
interface when a multicast datagram is sent.}


@deftogether[(
@defproc[(udp-multicast-set-loopback! [udp-socket udp?] [loopback? any/c]) void?]
@defproc[(udp-multicast-loopback? [udp-socket udp?]) boolean?]
)]{

@margin-note{Loopback settings correspond to the
@as-index{@tt{IP_MULTICAST_LOOP}} setting of the socket.}

Sets or checks whether @racket[udp-socket] receives its own multicast
datagrams: a @racket[#t] result or a true value for @racket[loopback?]
indicates that self-receipt is enabled, and @racket[#f] indicates that
self-receipt is disabled.}


@deftogether[(
@defproc[(udp-multicast-set-ttl! [udp-socket udp?] [ttl byte?]) void?]
@defproc[(udp-multicast-ttl [udp-socket udp?]) byte?]
)]{

@margin-note{Time-to-live settings correspond to the
@as-index{@tt{IP_MULTICAST_TTL}} setting of the socket.}

Sets or retrieves the current time-to-live setting of
@racket[udp-socket].

The time-to-live setting should almost always be 1, and it is
important that this number is as low as possible. In fact, these
functions seldom should be used at all. See the documentation for your
platform's IP stack.}
