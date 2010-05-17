#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/thread
                     scheme/contract
                     scheme/tcp))

@mzlib[#:mode title thread]

@defproc[(coroutine [proc ((any/c . -> . void?) . -> . any/c)]) 
         coroutine?]{

Returns a coroutine object to encapsulate a thread that runs only when
allowed. The @scheme[proc] procedure should accept one argument, and
@scheme[proc] is run in the coroutine thread when
@scheme[coroutine-run] is called. If @scheme[coroutine-run] returns
due to a timeout, then the coroutine thread is suspended until a
future call to @scheme[coroutine-run]. Thus, @scheme[proc] only
executes during the dynamic extent of a @scheme[coroutine-run] call.

The argument to @scheme[proc] is a procedure that takes a boolean, and
it can be used to disable suspends (in case @scheme[proc] has critical
regions where it should not be suspended). A true value passed to the
procedure enables suspends, and @scheme[#f] disables
suspends. Initially, suspends are allowed.}


@defproc[(coroutine? [v any/c]) any]{

Returns @scheme[#t] if @scheme[v] is a coroutine produced by
@scheme[coroutine], @scheme[#f] otherwise.}


@defproc[(coroutine-run [until (or/c evt? real?)][coroutine coroutine?]) 
         boolean?]{

Allows the thread associated with @scheme[coroutine] to execute for up
as long as @scheme[until] milliseconds (of @scheme[until] is a real
number) or @scheme[until] is ready (if @scheme[until] is an event). If
@scheme[coroutine]'s procedure disables suspends, then the coroutine
can run arbitrarily long until it re-enables suspends.

The @scheme[coroutine-run] procedure returns @scheme[#t] if
@scheme[coroutine]'s procedure completes (or if it completed earlier),
and the result is available via @scheme[coroutine-result].  The
@scheme[coroutine-run] procedure returns @scheme[#f] if
@scheme[coroutine]'s procedure does not complete before it is
suspended after @scheme[timeout-secs]. If @scheme[coroutine]'s
procedure raises an exception, then it is re-raised by
@scheme[coroutine-run].}


@defproc[(coroutine-result [coroutine coroutine]) any]{

Returns the result for @scheme[coroutine] if it has completed with a
value (as opposed to an exception), @scheme[#f] otherwise.}


@defproc[(coroutine-kill [coroutine coroutine?]) void?]{

Forcibly terminates the thread associated with @scheme[coroutine] if
it is still running, leaving the coroutine result unchanged.}


@defproc[(consumer-thread [f procedure?][init (-> any) void])
         (values thread? procedure?)]{

Returns two values: a thread descriptor for a new thread, and a
procedure with the same arity as @scheme[f].

When the returned procedure is applied, its arguments are queued to be
passed on to @scheme[f], and @|void-const| is immediately returned.
The thread created by @scheme[consumer-thread] dequeues arguments and
applies @scheme[f] to them, removing a new set of arguments from the
queue only when the previous application of @scheme[f] has completed;
if @scheme[f] escapes from a normal return (via an exception or a
continuation), the @scheme[f]-applying thread terminates.

The @scheme[init] argument is a procedure of no arguments; if it is
provided, @scheme[init] is called in the new thread immediately after the
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

Executes a TCP server on the port indicated by @scheme[port-no]. When
a connection is made by a client, @scheme[conn] is called with two
values: an input port to receive from the client, and an output port
to send to the client.

Each client connection is managed by a new custodian, and each call to
@scheme[conn] occurs in a new thread (managed by the connection's
custodian). If the thread executing @scheme[conn] terminates for any
reason (e.g., @scheme[conn] returns), the connection's custodian is
shut down. Consequently, @scheme[conn] need not close the ports
provided to it. Breaks are enabled in the connection thread if breaks
are enabled when @scheme[run-server] is called.

To facilitate capturing a continuation in one connection thread and
invoking it in another, the parameterization of the
@scheme[run-server] call is used for every call to
@scheme[handler]. In this parameterization and for the connection's
thread, the @scheme[current-custodian] parameter is assigned to the
connection's custodian.

If @scheme[conn-timeout] is not @scheme[#f], then it must be a
non-negative number specifying the time in seconds that a connection
thread is allowed to run before it is sent a break signal. Then, if
the thread runs longer than @scheme[(* conn-timeout 2)] seconds, then
the connection's custodian is shut down. If @scheme[conn-timeout] is
@scheme[#f], a connection thread can run indefinitely.

If @scheme[handler] is provided, it is passed exceptions related
to connections (i.e., exceptions not caught by @scheme[conn-proc], or
exceptions that occur when trying to accept a connection). The default
handler ignores the exception and returns @|void-const|.

The @scheme[run-server] function uses @scheme[listen], @scheme[close],
@scheme[accept] and @scheme[accept/break] in the same way as it might
use @scheme[tcp-listen], @scheme[tcp-close], @scheme[tcp-accept], and
@scheme[tcp-accept/enable-break] to accept connections. Provide
alternate procedures to use an alternate communication protocol (such
as SSL) or to supply optional arguments in the use of
@scheme[tcp-listen]. The @scheme[listener?] part of the contract
indicates that the procedures must all work on the same kind of
listener value.

The @scheme[run-server] procedure loops to serve client connections,
so it never returns. If a break occurs, the loop will cleanly shut
down the server, but it will not terminate active connections.}
