#lang scribble/manual
@(require scribble/eval
          scribble/struct
          scribble/decode
          racket/contract
          racket/place/distributed
          racket/sandbox
          racket/class
          (except-in "mz.rkt" log-message)
          (for-label (except-in racket/base log-message)
                     racket/place/define-remote-server
                     racket/place/distributed 
                     racket/class
                     racket/contract
                     racket/place
                     racket/place/private/async-bi-channel
                     racket/place/define-remote-server))


@(define evaler (make-base-eval))
@(interaction-eval #:eval evaler (require racket/place/distributed
                                          racket/class
                                          racket/place/define-remote-server))


@title[#:tag "distributed-places"]{Distributed Places}
@guidealso["distributed-places"]

@defmodule[racket/place/distributed]

Distributed places support programs whose computation may span
physical machines. The design relies on machine @deftech{nodes} that
perform computation.  The programmer configures a new distributed
system using a declarative syntax and callbacks.  A node begins life
with one initial place: the @deftech{message router}. @;{See
@figure-ref["node-places"].} After a node has been configured, its
message router is activated by calling the @racket[message-router]
function. The message router listens on a TCP port for incoming
connections from other nodes in the distributed system. Places can be
spawned within the node by sending place-spawn request messages to the
node's message router.

The distributed places implementation relies on two assumptions:

@itemlist[
@item{The user's @filepath{.ssh/config} and
 @filepath{.ssh/authorized_keys} files are configured correctly to
 allow passwordless connection to remote hosts via public key authentication.}
@item{Distributed places does not support the specification of ssh usernames.
 If a non-default ssh username is required the @filepath{.ssh/config} file
 should be used to specifiy the username.}
@item{All machines run the same version of Racket.  Futures versions of distributed
 places may use the zo binary data format for serialization.}
]

The following example illustrates a configuration and use of
distributed places that starts a new node on the current machine and
passes it a @racket["Hello World"] string:


@(begin
#reader scribble/comment-reader
[examples
(module hello-world-example racket/base
  (require racket/place/distributed
           racket/place)

  (provide hello-world)

  (define (hello-world)
    (place ch
      (printf "hello-world received: ~a\n" (place-channel-get ch))
      (place-channel-put ch "Hello World\n")
      (printf "hello-world sent: Hello World\n" )))


  (module+ main
    ;; 1) spawns a node running at "localhost" and listenting on port
    ;; 6344 for incomming connections.
    ;; 2) connects to the node running at localhost:6344 and creates a
    ;; place on that node by calling the hello-world procedure from
    ;; the current module.
    ;; 3) returns a remote-node% instance (node) and a
    ;; remote-connection% instance (pl) for communicating with the
    ;; new node and place
    (define-values (node pl)
      (spawn-node-supervise-place-at "localhost"
                                     #:listen-port 6344
                                     #:thunk #t
                                     (quote-module-path "..")
                                     'hello-world))

    ;; starts a message router which adds three event-container<%>s to
    ;; its list of events to handle: the node and two after-seconds
    ;; event containers . Two seconds after the launch of the message-router, a
    ;; message will be sent to the pl place.  After six seconds, the
    ;; program and all spawned nodes and places will terminate.
    (message-router
      node
      (after-seconds 2
        (*channel-put pl "Hello")
        (printf "message-router received: ~a\n" (*channel-get pl)))

      (after-seconds 6
        (exit 0)))))
]
)

@defproc[(message-router [ec (is-a?/c event-container<%>)] ...+) void?]{
  Waits in an endless loop for one of many events to become ready.  The
  @racket[message-router] procedure constructs a @racket[node%]
  instance to serve as the message router for the node. The
  @racket[message-router] procedure then adds all the declared
  @racket[event-container<%>]s to the @racket[node%] and finally calls
  the never ending loop @method[node% sync-events] method, which handles
  events for the node.
}

@defproc[(spawn-node-with-place-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any #f]
           [#:racket-path racket-path string-path? (racket-path)]
           [#:ssh-bin-path ssh-path string-path? (ssh-bin-path)]
           [#:distributed-launch-path launcher-path string-path? (path->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]
           [#:named place-name (or/c #f symbol?) #f]
           [#:thunk thunk (or/c #f #t) #f]) (is-a?/c remote-connection%)]{

Spawns a new remote node at @racket[hostname] with one instance place specified by
the @racket[instance-module-path] and @racket[instance-place-function-name].

When @racket[thunk] is @racket[#f], the place is created as the result of the framework
calling @racket[(dynamic-place instance-module-path instance-place-function-name)].
in the new node.

When @racket[thunk] is @racket[#t] the
@racket[instance-place-function-name] function should use
@racket[dynamic-place] or @racket[place] to create and return an
initial place in the new node.

When the @racket[place-name] symbol is present a named place is
created.  The @racket[place-name] symbol is used to establish later
connections to the named place.}

The result is a @racket[remote-node%] instance, not a
@racket[remote-connection%].  Use @method[remote-node%
get-first-place] on the result to obtain a @racket[remote-connection%].

The @racket[restart-on-exit] argument can be @racket[#t] to instruct
the @racket[remote-connection%] instance to respawn the place on the
remote node should it exit or terminate at any time. It can also be a
procedure of zero arguments to implement the restart procedure, or it
can be an object that support a @racket[restart] method that takes a
@tech{place} argument.}

@defproc[(spawn-node-supervise-place-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any #f]
           [#:racket-path racket-path string-path? (racket-path)]
           [#:ssh-bin-path ssh-path string-path? (ssh-bin-path)]
           [#:distributed-launch-path launcher-path string-path? (path->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]
           [#:named named (or/c #f string?) #f]
           [#:thunk thunk (or/c #f #t) #f]) (values (is-a?/c remote-node%) (is-a?/c remote-connection%))]{

Like @racket[spawn-node-with-dynamic-place-at], but the result is two values: the
new @racket[remote-node%] and its @racket[remote-connection%] instance.}

@defproc[(spawn-remote-racket-node
           [hostname string?]
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]
           [#:racket-path racket-path string-path? (racket-path)]
           [#:ssh-bin-path ssh-path string-path? (ssh-bin-path)]
           [#:distributed-launch-path launcher-path string-path? (path->string distributed-launch-path)]
           [#:use-current-ports use-current-ports #f])  (is-a?/c remote-node%)]{

Spawns a new remote node at @racket[hostname] and returns a @racket[remote-node%] handle.}

@defproc[(create-place-node
           [hostname string?]
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]
           [#:racket-path racket-path string-path? (racket-path)]
           [#:ssh-bin-path ssh-path string-path? (ssh-bin-path)]
           [#:distributed-launch-path launcher-path string-path? (path->string distributed-launch-path)]
           [#:use-current-ports use-current-ports boolean? #t])  (is-a?/c remote-node%)]{

Like @racket[spawn-remote-racket-node], but the @racket[current-output-port] and @racket[current-error-port]
are used as the standard ports for the spawned process instead of new pipe ports.}

@defproc[(supervise-place-at
           [remote-node (is-a?/c remote-node%)]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]
           [#:named named (or/c #f symbol?) #f]
           [#:thunk thunk (or/c #f #t) #f]) (is-a?/c remote-connection%)]{

When @racket[thunk] is @racket[#f], creates a new place on @racket[remote-node] by using
@racket[dynamic-place] to invoke
@racket[instance-place-function-name] from the module
@racket[instance-module-path].

When @racket[thunk] is @racket[#t], creates a new place at @racket[remote-node] by executing the thunk
exported as @racket[instance-place-function-name] from the module
@racket[instance-module-path]. The function should use
@racket[dynamic-place] or @racket[place] to create and return a place in the new
node.

When the @racket[place-name] symbol is present a named place is
created.  The @racket[place-name] symbol is used to establish later
connections to the named place.}

@defproc[(supervise-process-at
           [hostname string?]
           [commandline-argument string?] ...+
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]) (is-a?/c remote-process%)]{
Spawns an attached external process at host @racket[hostname].
}

@defproc[(supervise-thread-at
           [remote-node (is-a?/c remote-node%)]
           [instance-module-path module-path?]
           [instance-thunk-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]) (is-a?/c remote-connection%)]{
Creates a new threadon the @racket[remote-node] by using
@racket[dynamic-require] to invoke
@racket[instance-place-function-name] from the module
@racket[instance-module-path].}


@defproc[(restart-every [seconds (number?)]
                        [#:retry retry (or/c number? #f) #f]
                        [#:on-final-fail on-final-fail (or/c #f (-> any/c)) #f])
         (is-a/c respawn-and-fire%)]{

Returns a @racket[restarter%] instance that should be supplied to a @racket[#:restart-on-exit] argument.
}


@defform[(every-seconds seconds-expr body ....)]{

Returns a @racket[respawn-and-fire%] instance that should be supplied
to a @racket[message-router].  The @racket[respawn-and-fire%] instance
executes @racket[body]s once every @math{N} seconds,
where @math{N} is the result of @racket[seconds-expr].}


@defform[(after-seconds seconds-expr body ....)]{

Returns a @racket[after-seconds%] instance that should be supplied to
a @racket[message-router].  The @racket[after-seconds%] instance
executes the @racket[body]s after a delay of @math{N} seconds from the
start of the event loop, where @math{N} is the result of
@racket[seconds-expr].}


@defproc[(connect-to-named-place [node  (is-a?/c remote-node%)] [name symbol?]) (is-a?/c remote-connection%)]{
Connects to a named place on the @racket[node] named @racket[name] and returns a @racket[remote-connection%] object.
}

@defproc[(log-message [severity (or/c 'fatal 'error 'warning 'info 'debug)] [msg string?]) 
         void?]{

 Logs a message at the root node.}


@definterface[event-container<%> ()]{
  All objects that are supplied to the @racket[message-router] must
  implement the @racket[event-container<%>] interface.  The
  @racket[message-router] calls the @racket[register] method on each
  supplied @racket[event-container<%>] to obtain a list of events
  on which the event loop should wait.

  @defmethod[(register [events (listof events?)]) (listof events?)]{
    Returns the list of events inside the @racket[event-container<%>] that
    should be waited on by the @racket[message-router].
  }

The following classes all implement @racket[event-container<%>] and
can be supplied to a @racket[message-router]:
@racket[spawned-process%], @racket[place-socket-bridge%],
@racket[node%], @racket[remote-node%], @racket[remote-connection%],
@racket[place%] @racket[connection%], @racket[respawn-and-fire%], and
@racket[after-seconds%].

}

@defclass[spawned-process% object% (event-container<%>)
  (defmethod (get-pid) exact-positive-integer?) ]{

@defconstructor[([cmdline-list (listof (or/c string? path?))]
                 [parent  (is-a?/c remote-node%) #f]
                 )]{
The @racket[cmdline-list] is a list of command line arguments of type @racket[string] and/or @racket[path].

The @racket[parent] argument is a @racket[remote-node%] instance that will be notified when the process dies via
a @racket[(send parent process-died this)] call.
}
}

@;{@examples[ #:eval evaler
(new spawned-process% [cmdline-list
  (list (ssh-bin-path) "localhost" (racket-path) "-tm" distributed-launch-path "spawn" (->string 6340))])
]
}

@defclass[place-socket-bridge% object% (event-container<%>)
  (defmethod (get-sc-id) exact-positive-integer?) ]{

  @defconstructor[([pch place-channel?]
                   [sch (is-a?/c socket-connection%)]
                   [id exact-positive-integer?]
                   )]{
    The @racket[pch] argument is a @racket[place-channel].  Messages
    received on @racket[pch] are forwarded to the socket-connection%
    @racket[sch] via a @racket[dcgm] message. e.g.
    @racket[(sconn-write-flush sch (dcgm DCGM-TYPE-INTER-DCHANNEL id id msg))]
    The @racket[id] is a @racket[exact-positive-integer] that identifies
    the socket-connection subchannel for this inter-node place connection.
  }
}

@defclass[socket-connection% object% (event-container<%>)]{
  @defconstructor[([host (or/c string? #f) #f]
                   [port (or/c port-no? #f) #f]
                   [retry-times exact-nonnegative-integer? 30]
                   [delay number? 1]
                   [background-connect? any/c #f]
                   [in (or/c input-port? #f) #f]
                   [out (or/c output-port #f) #f]
                   [remote-node (or/c (is-a?/c remote-node%) #f) #f]
                   )]{
    When a @racket[host] and @racket[port] are supplied a new tcp
    connection is established.  If a @racket[input-port?] and
    @racket[output-port?] are supplied as @racket[in] and @racket[out],
    the ports are used as a connection to the remote host.  The
    @racket[retry-times] argument specifies how many times to retry the
    connection attempt should it fail to connect and defaults to 30 retry
    attempts.  Often a remote node is still booting up when a connection
    is attempted and the connection needs to be retried several times.
    The @racket[delay] argument specifies how many seconds to wait between
    retry attempts.  The @racket[background-connect?] argument defaults to
    @racket[#t] and specifies that the constructor should retry
    immediately and that connecion establishment should occur in the
    background.  Finally, the @racket[remote-node] argument specifies the
    @racket[remote-node%] instance that should be notified should the
    connection fail.
  }
}

@defclass[node% object% (event-container<%>)]{

The @racket[node%] instance controls a distributed places node. It
launches places and routes inter-node place messages in the
distributed system.  The @racket[message-router] form constructs a
@racket[node%] instance under the hood.  Newly spawned nodes also have
a @racket[node%] instance in their initial place that serves as the
node's message router.

@defconstructor[([listen-port tcp-listen-port? #f])]{
 Constructs a @racket[node%] that will listen on @racket[listen-port] for inter-node connections.}

@defmethod[(sync-events) void?]{
 Starts the never ending event loop for this distributed places node.
}
}

@(define-syntax-rule (one-sided-note one-sided-place?)
     (list
      @t{The @racket[one-sided-place?] argument is an internal use
      argument for launching remote places from within a place using
      the old design pattern.}))

@defclass[remote-node% object% (event-container<%>)]{

  The @racket[node%] instance controls a distributed places node. It
  launches compute places and routes inter-node place messages in the
  distributed system.  This is the remote api to a distributed places
  node. Instances of @racket[remote-node%] are returned by
  @racket[spawn-remote-racket-node],
  @racket[spawn-node-supervise-dynamic-place-at], and
  @racket[spawn-node-supervise-place-thunk-at].

  @defconstructor[([listen-port tcp-listen-port? #f]
                   [restart-on-exit any/c #f])]{
    Constructs a @racket[node%] that will listen on
    @racket[listen-port] for inter-node connections.

    When set to true the @racket[restart-on-exit] parameter causes the
    specified node to be restarted when the ssh session spawning the node
    dies.
  }

  @defmethod[(get-first-place) (is-a?/c remote-connection%)]{
    Returns the @racket[remote-connection%] object instance for the first place spawned on this node.
  }
  @defmethod[(get-first-place-channel) place-channel?]{
    Returns the communication channel for the first place spawned on this node.
  }
  @defmethod[(get-log-prefix) string?]{
    Returns @racket[(format "PLACE ~a:~a" host-name listen-port)]
  }

  @defmethod[(launch-place
               [place-exec list?]
               [#:restart-on-exit restart-on-exit any/c #f]
               [#:one-sided-place? one-sided-place? any/c #f]) (is-a?/c remote-connection%)]{
    Launches a place on the remote node represented by this @racket[remote-node%] instance.

    The @racket[place-exec] argument describes how the remote place should be launched,
    and it should have one of the following shapes:
      @itemize[@item{@racket[(list 'place _place-module-path _place-thunk)]}
               @item{@racket[(list 'dynamic-place _place-module-path _place-func)]}]
      The difference between these two launching methods is that
      the @racket['place] version of @racket[place-exec] expects a
      thunk to be exported by the module
      @racket[place-module-path].  Executing the thunk is expected to
      create a new place and return a place descriptor to the newly
      created place. The @racket['dynamic-place] version of
      @racket[place-exec] expects place-func to be a function taking a
      single argument, the initial channel argument, and calls
      @racket[dynamic-place] on behalf of the user and creates the new
      place from the @racket[place-module-path] and
      @racket[place-func].

    The @racket[restart-on-exit] argument is treated in the same way
    as for @racket[spawn-node-with-dynamic-place-at].

    @one-sided-note[one-sided-place?]
  }

  @defmethod[(remote-connect [name string?]) remote-connection%]{
    Connects to a named place on the remote node represented by this @racket[remote-node%] instance.
  }

  @defmethod[(send-exit) void?]{
    Sends a message instructing the remote node represented by this
    @racket[remote-node%] instance to exit immediately
  }
}

@defproc[(node-send-exit [remote-node% node]) void?]{
Sends @racket[node] a message telling it to exit immediately.
}
@defproc[(node-get-first-place [remote-node% node]) (is-a?/c remote-connection%)]{
Returns the @racket[remote-connection%] instance of the first place spawned at this node
}

@defproc[(distributed-place-wait [remote-connection% place]) void?]{
Waits for @racket[place] to terminate.}

@defclass[remote-connection% object% (event-container<%>)]{

The @racket[remote-connection%]
instance provides a remote api to a place
running on a remote distributed places node. It launches a
places or connects to a named place and routes inter-node place messages to the remote place.

@defconstructor[([node  (is-a?/c remote-node%)]
                 [place-exec list?]
                 [name string?]
                 [restart-on-exit #f]
                 [one-sided-place? #f]
                 [on-channel #f])]{
 Constructs a @racket[remote-connection%] instance.

 The @racket[place-exec] argument describes how the remote place should be launched
 in the same way as for @xmethod[remote-node% launch-place].

 The @racket[restart-on-exit] argument is treated in the same way
 as for @racket[spawn-node-with-dynamic-place-at].

 @one-sided-note[one-sided-place?]

 See @racket[set-on-channel!] for description of @racket[on-channel] argument.
}

@defmethod[(set-on-channel! [callback (-> channel msg void?)]) void?]{
 Installs a handler function that handles messages from the remote place.
 The @racket[setup/distributed-docs] module uses this callback to handle job completion messages.
}
}


@defclass[place% object% (event-container<%>)]{

  The @racket[place%] instance represents a place launched on a
  distributed places node at that node. It launches a compute places and
  routes inter-node place messages to the place.

  @defconstructor[([node (is-a?/c remote-connection%)]
                 [place-exec list?]
                 [ch-id exact-positive-integer?]
                 [sc (is-a?/c socket-connection%)]
                 [on-place-dead (-> event void?) default-on-place-dead])]{
    Constructs a @racket[remote-connection%] instance.
    The @racket[place-exec] argument describes how the remote place should be launched
     in the same way as for @xmethod[remote-node% launch-place].
    The @racket[ch-id] and @racket[sc] arguments are internally used to
    establish routing between the remote node spawning this place and the
    place itself.  The @racket[on-place-dead] callback handles the event
    when the newly spawned place terminates.
  }

 @defmethod[(wait-for-die) void?]{
   Blocks and waits for the subprocess representing the @racket[remote-node%] to exit.
 }
}

@defclass[connection% object% (event-container<%>)]{

The @racket[connection%] instance represents a connection to a
named-place instance running on the current node. It routes inter-node
place messages to the named place.

@defconstructor[([node  (is-a?/c remote-node%)]
                 [name string?]
                 [ch-id exact-positive-integer?]
                 [sc (is-a?/c socket-connection%)])]{
 Constructs a @racket[remote-connection%] instance.
 The @racket[place-exec] argument describes how the remote place should be launched
 in the same way as for @xmethod[remote-node% launch-place].
 The @racket[ch-id] and @racket[sc] arguments are internally used to
 establish routing between the remote node and this named-place.
 }
}

@defclass[respawn-and-fire% object% (event-container<%>)]{

 The @racket[respawn-and-fire%] instance represents a thunk that should
 execute every @racket[n] seconds.

@defconstructor[([seconds (and/c real? (not/c negative?))]
                 [thunk (-> void?)])]{
 Constructs a @racket[respawn-and-fire%] instance that when placed
 inside a @racket[message-router] construct causes the supplied
 thunk to execute every @racket[n] seconds.
}
}

@defclass[after-seconds% object% (event-container<%>)]{

 The @racket[after-seconds%] instance represents a thunk that should
 execute after @racket[n] seconds.

@defconstructor[([seconds (and/c real? (not/c negative?))]
                 [thunk (-> void?)])]{
 Constructs an @racket[after-seconds%] instance that when placed
 inside a @racket[message-router] construct causes the supplied
 thunk to execute after @racket[n] seconds.
}
}

@defclass[restarter% after-seconds% (event-container<%>)]{

 The @racket[restarter%] instance represents a restart strategy.

@defconstructor[([seconds number?]
                 [retry (or/c number? #f) #f]
                 [on-final-fail (or/c #f (-> any/c)) #f])]{
 Constructs an @racket[restarter%] instance that when supplied to a
 @racket[#:restart-on-exit] argument, attempts to restart the process
 every @racket[seconds].  The @racket[retry] argument specifies how
 many time to attempt to restart the process before giving up.  If the
 process stays alive for @racket[(* 2 seconds)] the attempted retries
 count is reset to @racket[0].  The @racket[on-final-fail] thunk is
 called when the number of retries is exceeded
}
}

@defthing[distributed-launch-path path?]{
Contains the local path to the distributed places launcher.  The
distributed places launcher is the bootsrap file that launches the
message router on a new node.
}

@defproc[(ssh-bin-path) string?]{
Returns the path to the ssh binary on the local system in string form.
}
@examples[ #:eval evaler
(ssh-bin-path)
]

@defproc[(racket-path) path?]{
Returns the path to the currently executing Racket binary on the local system.
}

@defproc[(build-distributed-launch-path [collects-path path-string?]) string?]{
Returns the path to the distributed places launch file.
The function can take an optional argument specifying the path to the collects directory.
}

@;{
@defproc[(build-node-args . list?) list?]{
Takes all the positional and keyword arguments pass to it and builds a 
@racket[(list (list keywords ...) (list keyword-arguments ...) (list positional-args ...))] 
suitable as an argument to  @racket[(lambda (x) (apply keyword-apply spawn-node-at x))].}
}

@defproc[(spawn-node-at [hostname string?] 
           [#:listen-port port port-no? DEFAULT-ROUTER-PORT]
           [#:racket-path racket-path string-path? (racket-path)]
           [#:ssh-bin-path ssh-path string-path? (ssh-bin-path)]
           [#:distributed-launch-path launcher-path string-path? (path->string distributed-launch-path)]) channel?]{
  Spawns a node in the background using a Racket thread and returns a channel that becomes ready with a @racket[remote-node%]
  once the node has spawned successfully 
}

@defproc[(spawn-nodes/join [nodes-descs list?]) void?]{
Spawns a list of nodes by calling @racket[(lambda (x) (apply keyword-apply spawn-node-at x))] for each node description in
@racket[nodes-descs] and then waits for each node to spawn.
}


@defproc[(*channel-put [ch (or/c place-channel? async-bi-channel? 
                                 channel? (is-a?/c remote-connection%))] 
                       [msg any]) 
         void?]{
Sends @racket[msg] over @racket[ch] channel.
}

@defproc[(*channel-get [ch (or/c place-channel? async-bi-channel? 
                                 channel? (is-a?/c remote-connection%))]) 
         any]{
Returns a message received on @racket[ch] channel.
}

@defproc[(*channel? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is one of @racket[place-channel?], @racket[async-bi-channel?],
@racket[channel?], or @racket[(is-a?/c remote-connection%)].
}


@defproc[(send-new-place-channel-to-named-dest [ch *channel?] [src-id any] 
                                               [dest-list (listof string? port-no? string?)])
         place-channel?]{
Creates and returns a new place channel connection to a named place at @racket[dest-list].
The @racket[dest-list] argument is a list of a remote-hostname remote-port and named-place name.
The channel @racket[ch] should be a connection to a @racket[message-router].
}

@defproc[(mr-spawn-remote-node [mrch *channel?] [host string?] [#:listen-port listen-port port-no? DEFAULT-ROUTER-PORT]
                               [#:solo solo boolean? #f]) void?]{
Sends a message to a message router over @racket[mrch] channel asking the message router to spawn a new node
at @racket[host] listening on port @racket[listen-port].  If the @racket[#:solo] keyword argument is supplied
the new node is not folded into the complete network with other nodes in the distributed system.
}

@defproc[(mr-supervise-named-dynamic-place-at [mrch *channel?] [dest (listof string? port-no?)] [name string?] [path string?] [func symbol?]) void?]{
Sends a message to a message router over @racket[mrch] channel asking the message router to spawn
a named place at @racket[dest] named @racket[name].  The place is spawned at the remote node by calling
dynamic place with module-path @racket[path] and function @racket[func].  The @racket[dest] parameter should be a
list of remote-hostname and remote-port.
}

@defproc[(mr-connect-to [mrch *channel?] [dest (listof string? port-no?)] [name string?]) void?]{
Sends a message to a message router over @racket[mrch] channel asking the message router to create a new 
connection to the named place named @racket[name] at @racket[dest].
The @racket[dest] parameter should be a list of remote-hostname and remote-port.
}

@defproc[(start-message-router/thread [#:listen-port listen-port port-no? DEFAULT-ROUTER-PORT]
                                      [#:nodes nodes list? null]) (values thread? channel?)]{
Starts a message router in a Racket thread connected to @racket[nodes], listening on port @racket[listen-port], and 
returns a @racket[channel?] connection to the message router.
}

@defproc[(port-no? [no (and/c exact-nonnegative-integer?  (integer-in 0 65535))]) boolean?]{
Returns @racket[#t] if @racket[no] is a @racket[exact-nonnegative-integer?] between @racket[0] and @racket[65535].
}

@defthing[DEFAULT-ROUTER-PORT port-no?]{
The default port for distributed places message router.
}

@defclass[named-place-typed-channel% object% () ]{

  @defconstructor[([ch place-channel?])]{
    The @racket[ch] argument is a @racket[place-channel].  
  }
  @defmethod[(get [type symbol?]) any]{
    Returns the first message received on @racket[ch] that has the type @racket[type]. Messages are lists and their type is the first 
item of the list which should be a @racket[symbol?].  Messages of other types that are received are queued for later @racket[get] requests.
  }
}

@defproc[(tc-get [type symbol?] [ch place-channel?]) void?]{
Gets a message of type @racket[type] from the @racket[named-place-typed-channel%] @racket[ch].
}

@;{@examples[ #:eval evaler
(racket-path)
]}

@;{
@defproc[(->string) string?]{
Coerces strings, numbers, symbols, and paths to a string.
}
@examples[ #:eval evaler
(->string "hello")
(->string 1)
(->string 'bye)
(->string (build-path "ridge"))
(->string #"bytes")
]

@defproc[(->number) number?]{
Coerces strings, numbers, to a number.
}
@examples[ #:eval evaler
(->number "100")
(->number 1)
]

@defproc[(->path) path?]{
Coerces paths and strings to a path.
}
@examples[ #:eval evaler
(->path "/usr/bin")
(->path (build-path "ridge"))
]

@defproc[(->length) path?]{
Returns the length of strings, bytes, and lists.
}
@examples[ #:eval evaler
(->length "Boo")
(->length #"Woo")
(->length (list 1 2 3 4))
]
}

@defproc[(write-flush [datum any] [port port?]) void?]{
Writes @racket[datum] to @racket[port] and then flushes @racket[port].
}

@defproc[(printf/f [format string?] [args any] ...) void?]{
Calls @racket[printf] followed by a call to @racket[flush-output].
}

@defproc[(displayln/f [item any]) void?]{
Calls @racket[displayln] followed by a call to @racket[flush-output].
}

@examples[ #:eval evaler
(write-flush "Hello World" (current-output-port))
]

@;@include-section["define-remote-server.scrbl"]
@section{Define Remote Server}

@defmodule[racket/place/define-remote-server]

@deftogether[(@defform[(define-remote-server [name identifier?] rpc-forms ...+)]
              @defform[(define-named-remote-server [name identifier?] rpc-forms ...+)])]{
The @racket[define-remote-server] and @racket[define-named-remote-server] forms
are nearly identical.  The @racket[define-remote-server] form should be used
with @racket[supervise-dynamic-place-at] to build a private rpc server, while
the @racket[define-named-remote-server] form should be used with
@racket[supervise-named-dynamic-place-at] to build a rpc server inside a named
place.

The @racket[define-named-remote-server] form takes an identifier and a
list of custom expressions as its arguments.  From the identifier a
function is created by prepending the @tt{make-} prefix. This
procedure takes a single argument a @racket[place-channel]. In the
example below, the @racket[make-tuple-server] identifier is the
@racket[place-function-name] given to the
@racket[supervise-named-dynamic-place-at] form to spawn an rpc server.
The server created by the @racket[make-tuple-server] procedure sits in
a loop waiting for rpc requests from the @racket[define-rpc] functions
documented below.

@defform[(define-state id value)]{
 Expands to a @@racket[define], which is closed over by the @racket[define-rpc] functions
 to form local state.
}

@defform[(define-rpc (id args ...) body ...)]{
 Expands to a client rpc function @tt{name-id} which sends @racket[id] and @racket[args ...] to
 the rpc server @racket[rpc-place] and waits for a response.
 @racket[(define (name-id rpc-place args ...) body)]
}

@defform[(define-cast (id args ...) body ...)]{
 Expands to a client rpc function @tt{name-id} which sends @racket[id] and @racket[args ...] to
 the rpc server @racket[rpc-place] but does not receive any response. A cast is a one-way communication
 technique.
 @racket[(define (name-id rpc-place args ...) body)]
}

The
@racket[define-state] custom form translates into a simple
@racket[define] form, which is closed over by the @racket[define-rpc]
forms.

The @racket[define-rpc] form is expanded into two parts. The first
part is the client stubs that call the rpc functions. The client
function name is formed by concatenating the
@racket[define-named-remote-server] identifier, @tt{tuple-server},
with the RPC function name @tt{set} to form @racket[tuple-server-set].
The RPC client functions take a destination argument which is a
@racket[remote-connection%] descriptor and then the RPC function
arguments. The RPC client function sends the RPC function name,
@racket[set], and the RPC arguments to the destination by calling an
internal function @racket[named-place-channel-put]. The RPC client
then calls @racket[named-place-channel-get] to wait for the RPC
response.

The second expansion part of @racket[define-rpc] is the server
implementation of the RPC call.  The server is implemented by a match
expression inside the @racket[make-tuple-server] function.  The match
clause for @racket[tuple-server-set] matches on messages beginning
with the @racket['set] symbol. The server executes the RPC call with
the communicated arguments and sends the result back to the RPC
client.

The @racket[define-cast] form is similar to the @racket[define-rpc] form
except there is no reply message from the server to client
}

@examples[ #:eval evaler
(module tuple-server-example racket/base
  (require racket/match
           racket/place/define-remote-server)

  (define-named-remote-server tuple-server
    (define-state h (make-hash))
    (define-rpc (set k v)
      (hash-set! h k v)
      v)
    (define-rpc (get k)
      (hash-ref h k #f))
    (define-cast (hello)
      (printf "Hello from define-cast\n")
      (flush-output))))
]

@examples[ #:eval evaler
(module bank-server-example racket/base
  (require racket/match
           racket/place/define-remote-server)

  (define-remote-server bank
    (define-state accounts (make-hash))
    (define-rpc (new-account who)
       (match (hash-has-key? accounts who)
         [#t '(already-exists)]
         [else
           (hash-set! accounts who 0)
           (list 'created who)]))
    (define-rpc (remove who amount)
       (cond
         [(hash-ref accounts who (lambda () #f)) =>
            (lambda (balance)
              (cond [(<= amount balance)
                     (define new-balance (- balance amount))
                     (hash-set! accounts who new-balance)
                     (list 'ok new-balance)]
                    [else
                      (list 'insufficient-funds balance)]))]
         [else
           (list 'invalid-account who)]))
    (define-rpc (add who amount)
      (cond
         [(hash-ref accounts who (lambda () #f)) =>
            (lambda (balance)
              (define new-balance (+ balance amount))
              (hash-set! accounts who new-balance)
              (list 'ok new-balance))]
         [else
           (list 'invalid-account who)]))))
]


@defproc[(log-to-parent [msg string?] [#:severity severity symbol? 'info]) void?]{
  The @racket[log-to-parent] procedure can be used inside a
  @racket[define-remote-server] or @racket[define-named-remote-server] form to
  send a logging message to the remote owner of the rpc server.
}
@section{Async Bidirectional Channels}

@defmodule[racket/place/private/async-bi-channel]

@defproc[(make-async-bi-channel) async-bi-channel?]{
Creates and returns an opaque structure, which is the async bidirectional channel.
}

@defproc[(async-bi-channel? [ch any]) boolean?]{
A predicate that returns @racket[#t] for async bidirectional channels.
}

@defproc[(async-bi-channel-get [ch async-bi-channel?]) any]{
Returns the next available message from the async bidirectional channel @racket[ch].
}

@defproc[(async-bi-channel-put [ch async-bi-channel?] [msg any]) void?]{
Sends message @racket[msg] to the remote end of the async bidirectional channel @racket[ch].
}

@(close-eval evaler)
@include-section["rmpi.scrbl"]
