#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/thread
                     scheme/contract
                     scheme/tcp))

@mzlib[#:mode title thread]

@defproc[(coroutine [proc ((any/c . -> . void?) . -> . any/c)]) 
         coroutine?]{

Returns a coroutine object to encapsulate a thread that runs only when
allowed. The @racket[proc] procedure should accept one argument, and
@racket[proc] is run in the coroutine thread when
@racket[coroutine-run] is called. If @racket[coroutine-run] returns
due to a timeout, then the coroutine thread is suspended until a
future call to @racket[coroutine-run]. Thus, @racket[proc] only
executes during the dynamic extent of a @racket[coroutine-run] call.

The argument to @racket[proc] is a procedure that takes a boolean, and
it can be used to disable suspends (in case @racket[proc] has critical
regions where it should not be suspended). A true value passed to the
procedure enables suspends, and @racket[#f] disables
suspends. Initially, suspends are allowed.}


@defproc[(coroutine? [v any/c]) any]{

Returns @racket[#t] if @racket[v] is a coroutine produced by
@racket[coroutine], @racket[#f] otherwise.}


@defproc[(coroutine-run [until (or/c evt? real?)][coroutine coroutine?]) 
         boolean?]{

Allows the thread associated with @racket[coroutine] to execute for up
as long as @racket[until] milliseconds (of @racket[until] is a real
number) or @racket[until] is ready (if @racket[until] is an event). If
@racket[coroutine]'s procedure disables suspends, then the coroutine
can run arbitrarily long until it re-enables suspends.

The @racket[coroutine-run] procedure returns @racket[#t] if
@racket[coroutine]'s procedure completes (or if it completed earlier),
and the result is available via @racket[coroutine-result].  The
@racket[coroutine-run] procedure returns @racket[#f] if
@racket[coroutine]'s procedure does not complete before it is
suspended after @racket[timeout-secs]. If @racket[coroutine]'s
procedure raises an exception, then it is re-raised by
@racket[coroutine-run].}


@defproc[(coroutine-result [coroutine coroutine]) any]{

Returns the result for @racket[coroutine] if it has completed with a
value (as opposed to an exception), @racket[#f] otherwise.}


@defproc[(coroutine-kill [coroutine coroutine?]) void?]{

Forcibly terminates the thread associated with @racket[coroutine] if
it is still running, leaving the coroutine result unchanged.}


@defproc[(consumer-thread [f procedure?][init (-> any) void])
         (values thread? procedure?)]{

Returns two values: a thread descriptor for a new thread, and a
procedure with the same arity as @racket[f].

When the returned procedure is applied, its arguments are queued to be
passed on to @racket[f], and @|void-const| is immediately returned.
The thread created by @racket[consumer-thread] dequeues arguments and
applies @racket[f] to them, removing a new set of arguments from the
queue only when the previous application of @racket[f] has completed;
if @racket[f] escapes from a normal return (via an exception or a
continuation), the @racket[f]-applying thread terminates.

The @racket[init] argument is a procedure of no arguments; if it is
provided, @racket[init] is called in the new thread immediately after the
thread is created.}


@defproc[(run-server [port-no (integer-in 1 65535)]
                     [conn-proc (input-port? output-port? . -> . any)]
                     [conn-timeout (and/c real? (not/c negative?))]
                     [handler (exn? . -> . any/c) void]
                     [listen ((integer-in 1 65535) (one-of/c 5) (one-of/c #t) 
                              . -> . listener?) 
                                  tcp-listen]
                     [close (listener? . -> . any) tcp-close]
                     [accept (listener? . ->* . (input-port? output-port?)) tcp-accept]
                     [accept/break (listener? . ->* . (input-port? output-port?)) tcp-accept/enable-break])
         void?]{

Executes a TCP server on the port indicated by @racket[port-no]. When
a connection is made by a client, @racket[conn] is called with two
values: an input port to receive from the client, and an output port
to send to the client.

Each client connection is managed by a new custodian, and each call to
@racket[conn] occurs in a new thread (managed by the connection's
custodian). If the thread executing @racket[conn] terminates for any
reason (e.g., @racket[conn] returns), the connection's custodian is
shut down. Consequently, @racket[conn] need not close the ports
provided to it. Breaks are enabled in the connection thread if breaks
are enabled when @racket[run-server] is called.

To facilitate capturing a continuation in one connection thread and
invoking it in another, the parameterization of the
@racket[run-server] call is used for every call to
@racket[handler]. In this parameterization and for the connection's
thread, the @racket[current-custodian] parameter is assigned to the
connection's custodian.

If @racket[conn-timeout] is not @racket[#f], then it must be a
non-negative number specifying the time in seconds that a connection
thread is allowed to run before it is sent a break signal. Then, if
the thread runs longer than @racket[(* conn-timeout 2)] seconds, then
the connection's custodian is shut down. If @racket[conn-timeout] is
@racket[#f], a connection thread can run indefinitely.

If @racket[handler] is provided, it is passed exceptions related
to connections (i.e., exceptions not caught by @racket[conn-proc], or
exceptions that occur when trying to accept a connection). The default
handler ignores the exception and returns @|void-const|.

The @racket[run-server] function uses @racket[listen], @racket[close],
@racket[accept] and @racket[accept/break] in the same way as it might
use @racket[tcp-listen], @racket[tcp-close], @racket[tcp-accept], and
@racket[tcp-accept/enable-break] to accept connections. Provide
alternate procedures to use an alternate communication protocol (such
as SSL) or to supply optional arguments in the use of
@racket[tcp-listen]. The @racket[listener?] part of the contract
indicates that the procedures must all work on the same kind of
listener value.

The @racket[run-server] procedure loops to serve client connections,
so it never returns. If a break occurs, the loop will cleanly shut
down the server, but it will not terminate active connections.}
