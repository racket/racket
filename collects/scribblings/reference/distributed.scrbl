#lang scribble/manual
@(require scribble/eval
          scribble/struct
          scribble/decode
          racket/contract
          racket/place/distributed
          racket/sandbox
          racket/class)
@(require (for-label racket/place/distributed racket/class))


@(define evaler (make-base-eval))
@(interaction-eval #:eval evaler (require racket/place/distributed
                                          racket/class
                                          racket/place/define-remote-server))

@(define output-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket/base)))

@(define-syntax interaction-output
  (syntax-rules ()
    [(_ #:eval evaluator e)
        (begin
          (interaction #:eval evaluator e)
          (printf "K ~a\n" (get-output evaluator)))]))

@title[#:tag "distributed-places"]{Distributed Places}

@defmodule[racket/place/distributed]

Distributed Places is a prototype of a distributed computing framework for
Racket.

Distributed Places' distributed computing design is centered around
machine nodes that do computation.  The user/programmer configures a
new distributed system using a declarative syntax and callbacks.  A
node begins life with one initial place, the message router. @;{See
@figure-ref["node-places"]}. Once the node has been configured the
message router is activated by calling the @racket[message-router]
function. The message router listens on a TCP port for incoming
connections from other nodes in the distributed system. Places
can be spawned within the node by sending place-spawn request messages
to the node's message router.

The use of Distributed Places is predicated on a couple assumptions:

@itemlist[
@item{ .ssh/config and authorized_keys are configured correctly to
 allow passwordless connection to remote hosts using public key
 authentication.}
@item{The same user account is used across all nodes in the
 distributed network.}
@item{All machines run the same version of Racket.}
]

@examples[
(module hello-world-example racket/base
  (require racket/place/distributed
           racket/place)

  (provide main
           hello-world)

  (define (hello-world)
    (place ch
      (printf "hello-world received: ~a\n" (place-channel-get ch))
      (define HW "Hello World")
      (place-channel-put ch (format "~a\n" HW))
      (printf "hello-world sent: ~a\n" HW)))


  (define (main)
    (define-values (vm pl)
      (spawn-vm-supervise-place-thunk-at "localhost"
                                           #:listen-port 6344
                                           (get-current-module-path)
                                           'hello-world))
    (message-router
      vm
      (after-seconds 2
        (dplace-put pl "Hello")
        (printf "message-router received: ~a\n" (dplace-get pl)))

      (after-seconds 6
        (exit 0))
      )))

(require 'hello-world-example)
]

@defproc[(message-router [ec events-container<%>?] ...+) void?]{
  Waits in an endless loop for one of many events to become ready.  The
  @racket[message-router] procedure constructs a @racket[node%]
  instance to serve as the message router for then node. The
  @racket[message-router] procedure then adds all the declared
  @racket[events-container<%>]s to the @racket[node%] and finally calls
  the never ending loop @racket[sync-events] method, which handles the
  events for the node.
}

@(define (p . l) (decode-paragraph l))
@(define spawn-vm-note
    (make-splice
     (list
      @p{This function returns a @racket[remote-node%] instance not a @racket[remote-place%]
      Call @racket[(send vm get-first-place)] to obtain the @racket[remote-place%] instance.})) )

@(define spawn-vm-dynamic-note
   (make-splice
     (list
       @p{
Spawns a new remote node at @racket[hostname] with one instance place specified by
the @racket[instance-module-path] and @racket[instance-place-function-name]
parameters. This procedure constructs the new remote-place by calling
@racket[(dynamic-place instance-module-path instance-place-function-name)].
       })))

@defproc[(spawn-vm-with-dynamic-place-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any? #f]
           [#:racket-path racketpath string-path? (racket-path)]
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]
           [#:launcher-path launcherpath string-path? (->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place?]{
@|spawn-vm-dynamic-note|
@|spawn-vm-note|
}

@defproc[(spawn-vm-supervise-dynamic-place-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any? #f]
           [#:racket-path racketpath string-path? (racket-path)]
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]
           [#:launcher-path launcherpath string-path? (->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]) (values remote-node%? remote-place%?)]{
@|spawn-vm-dynamic-note|
The new @racket[remote-vm%] and @racket[remote-place%] instances make up the two return values.
}

@(define place-thunk-function
    (make-splice
     (list
      @p{
The @racket[instance-thunk-function-name] procedure is
responsible for creating the place and returning the newly constructed
the place descriptor.  The
@racket[instance-thunk-function-name] procedure should
accomplish this by calling either @racket[dynamic-place] or
@racket[place] inside the thunk.
      })) )

@(define spawn-vm-thunk-note
   (make-splice
     (list
       @p{
Spawns a new remote node at @racket[hostname] with one instance place.
the @racket[instance-module-path] and @racket[instance-thunk-function-name]
parameters. This procedure constructs the new remote-place by calling
dynamically requiring the
@racket[instance-thunk-function-name] and invoking the
@racket[instance-thunk-function-name].
       }
       @p{
@racket[((dynamic-require instance-module-path instance-thunk-function-name))]
       })))
@defproc[(spawn-vm-with-place-thunk-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-thunk-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any? #f]
           [#:racket-path racketpath string-path? (racket-path)]
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]
           [#:launcher-path launcherpath string-path? (->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place%?]{
@|spawn-vm-thunk-note|
@|place-thunk-function|
@|spawn-vm-note|
}
@defproc[(spawn-vm-supervise-place-thunk-at
           [hostname string?]
           [instance-module-path module-path?]
           [instance-thunk-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]
           [#:initial-message initial-message any? #f]
           [#:racket-path racketpath string-path? (racket-path)]
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]
           [#:launcher-path launcherpath string-path? (->string distributed-launch-path)]
           [#:restart-on-exit restart-on-exit any/c #f]) (values remote-vm%? remote-place%?)]{
@|spawn-vm-thunk-note|
@|place-thunk-function|
The new @racket[remote-vm%] and @racket[remote-place%] instances make up the two return values.
}

@defproc[(spawn-remote-racket-vm
           [hostname string?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]
           [#:racket-path racketpath string-path? (racket-path)]
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]
           [#:launcher-path launcherpath string-path? (->string distributed-launch-path)]) remote-node%?]{
Spawns a new remote node at @racket[hostname] and returns a @racket[remote-node%] handle.
}
@defproc[(supervise-dynamic-place-at
           [remote-vm remote-vm?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by using
@racket[dynamic-place] to invoke
@racket[instance-place-function-name] from the module
@racket[instance-module-path].
}

@defproc[(supervise-place-thunk-at
           [remote-vm remote-vm?]
           [instance-module-path module-path?]
           [instance-thunk-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by executing the thunk
@racket[instance-thunk-function-name] from the module
@racket[instance-module-path].

@|place-thunk-function|
}

@defproc[(supervise-process-at
           [hostname string?]
           [commandline-argument string?] ...+
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]) remote-process%?]{
Spawns an attached external process at host @racket[hostname].
}

@defproc[(supervise-named-dynamic-place-at
           [remote-vm remote-vm?]
           [place-name symbol?]
           [instance-module-path module-path?]
           [instance-place-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by using
@racket[dynamic-place] to invoke
@racket[instance-place-function-name] from the module
@racket[instance-module-path]. The @racket[place-name] symbol
is used to establish later connections to the named place.
}

@defproc[(supervise-named-place-thunk-at
           [remote-vm remote-vm?]
           [place-name symbol?]
           [instance-module-path module-path?]
           [instance-thunk-function-name symbol?]
           [#:restart-on-exit restart-on-exit any/c #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by executing the thunk
@racket[instance-thunk-function-name] from the module
@racket[instance-module-path].  The @racket[place-name] symbol
is used to establish later connections to the named place.


@|place-thunk-function|
}

@defform[(restart-every [seconds (and/c real? nonnegative?)]
                        [#:retry retry (or/c nonnegative-integer? #f) #f]
                        [#:on-final-fail on-final-fail (or/c #f (-> any/c)) #f])]{

Returns a @racket[restarter%] instance that should be supplied to a @racket[#:restart-on-exit] argument.
}

@defform[(every-seconds seconds body ....)]{
Returns a @racket[respawn-and-fire%] instance that should be supplied to a @racket[message-router].
The @racket[respawn-and-fire%] instance executes the body expressions every @racket[seconds].
}

@defform[(after-seconds seconds body ....)]{
Returns a @racket[after-seconds%] instance that should be supplied to a @racket[message-router].
Executes the body expressions after a delay of @racket[seconds] from the start of the event loop.
}

@defproc[(connect-to-named-place [vm remote-node%?] [name symbol?]) remote-connection%?]{
Connects to a named place on the @racket[vm] named @racket[name] and returns a @racket[remote-connection%] object.
}

@defproc[(log-message [severity symbol?] [msg string?]) void?]{
 Logs a message, which traversed the process tree until it reaches the root, where it is printed to the console.
}

@definterface[event-container<%> ()]{
  All objects that are supplied to the @racket[message-router] must
  implement the @racket[event-container<%>] interface.  The
  @racket[message-router] calls the @racket[register] method on each
  supplied @racket[event-container<%>] to obtain a list of events the
  event loop should wait for.

  @defmethod[(register [events (listof events?)]) (listof events?)]{
    Returns the list of events inside the @racket[event-container<%>] that
    should be waited on by the @racket[message-router].
  }

The following classes all implement @racket[event-container<%>] and
can be supplied to a @racket[message-router]:
@racket[spawned-process%], @racket[place-socket-bridge%],
@racket[node%], @racket[remote-node%], @racket[remote-place%],
@racket[place%] @racket[connection%], @racket[respawn-and-fire%], and
@racket[after-seconds%].

}

@defclass[spawned-process% object% (event-container<%>)
  (defmethod (get-pid) exact-positive-integer?) ]{

@defconstructor[([cmdline-list (listof (or/c string? path?))]
                 [parent remote-node%? #f]
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
                 [sch socket-connection%?]
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

@(define place-exec-note
    (make-splice
     (list
      @p{The @racket[place-exec] argument describes how the remote place should be launched.}
      @itemize[@item{@racket[(list 'place place-module-path place-thunk)]}
               @item{@racket[(list 'dynamic-place place-module-path place-func)]}]
      @p{The difference between these two launching methods is that
      the @racket['place] version of @racket[place-exec] expects a
      thunk, zero argument function, to be exported by the module
      @racket[place-module-path].  Executing the thunk is expected to
      create a new place and return a place descriptor to the newly
      created place. The @racket['dynamic-place] version of
      @racket[place-exec] expects place-func to be a function taking a
      single argument, the initial channel argument, and calls
      @racket[dynamic-place] on behalf of the user and creates the new
      place from the @racket[place-module-path] and
      @racket[place-func].}
)))

@(define one-sided-note
    (make-splice
     (list
      @p{The @racket[#:one-sided-place?] argument is an internal use
      argument for launching remote places from within a place using
      the old design pattern 1.})))

@(define restart-on-exit-note
    (make-splice
     (list
      @p{The @racket[#:restart-on-exit] boolean argument instructs the
      remote-place% instance to respawn the place on the remote node
      should it exit or terminate at any time.  This boolean needs to
      be expanded to a restart criteria object in the future.})))


@defclass[remote-node% object% (event-container<%>)]{

  The @racket[node%] instance controls a distributed places node. It
  launches compute places and routes inter-node place messages in the
  distributed system.  This is the remote api to a distributed places
  node. Instances of @racket[remote-node%] are returned by
  @racket[spawn-remote-racket-vm],
  @racket[spawn-vm-supervise-dynamic-place-at], and
  @racket[spawn-vm-supervise-place-thunk-at].

  @defconstructor[([listen-port tcp-listen-port? #f]
                   [restart-on-exit any/c #f])]{
    Constructs a @racket[node%] that will listen on
    @racket[listen-port] for inter-node connections.

    When set to true the @racket[restart-on-exit] parameter causes the
    specified node to be restarted when the ssh session spawning the node
    dies.
  }

  @defmethod[(get-first-place) remote-place%?]{
    Returns the @racket[remote-place%] object instance for the first place spawned on this node.
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
               [#:one-sided-place? one-sided-place? any/c #f]) remote-place%?]{
    Launches a place on the remote node represented by this @racket[remote-node%] instance.
    @|place-exec-note|
    @|one-sided-note|
    @|restart-on-exit-note|
  }

  @defmethod[(remote-connect [name string?]) remote-connection%]{
    Connects to a named place on the remote node represented by this @racket[remote-node%] instance.
  }

  @defmethod[(send-exit) void?]{
    Sends a message instructing the remote node represented by this
    @racket[remote-node%] instance to exit immediately
  }
}

@defclass[remote-place% object% (event-container<%>)]{

The @racket[remote-place%] instance provides a remote api to a place
running on a remote distributed places node. It launches a compute
places and routes inter-node place messages to the remote place.

@defconstructor[([vm remote-node%?]
                 [place-exec list?]
                 [restart-on-exit #f]
                 [one-sided-place? #f]
                 [on-channel #f])]{
 Constructs a @racket[remote-place%] instance.
 @|place-exec-note|
 @|one-sided-note|
 @|restart-on-exit-note|

 See @racket[set-on-channel!] for description of @racket[on-channel] argument.
}

@defmethod[(set-on-channel! [callback (-> channel msg void?)]) void?]{
 Installs a handler function that handles messages from the remote place.
 The @racket[setup/distributed-docs] module uses this callback to handle job completion messages.
}
}

@defproc[(dplace-put [pl remote-place%?] [msg any/c]) void?]{
 This function is used inside @racket[message-router] callbacks.
 It sends messages to remote places.
}

@defproc[(dplace-get [pl remote-place%?]) any/c]{
 This function is used inside @racket[message-router] callbacks.
 It takes the current delimited continuation and resumes it when a message arrives from @racket[pl].
}

@defclass[remote-connection% object% (event-container<%>)]{

The @racket[remote-connection%] instance provides a remote api to a named place
running on a remote distributed places node. It connects to a named compute
places and routes inter-node place messages to the remote place.

@defconstructor[([vm remote-node%?]
                 [name string?]
                 [restart-on-exit #f]
                 [on-channel #f])]{
 Constructs a @racket[remote-place%] instance.
 @|restart-on-exit-note|

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

  @defconstructor[([vm remote-place%?]
                 [place-exec list?]
                 [ch-id exact-positive-integer?]
                 [sc socket-connection%?]
                 [on-place-dead (-> event void?) default-on-place-dead])]{
    Constructs a @racket[remote-place%] instance.
    @|place-exec-note|
    The @racket[ch-id] and @racket[sc] arguments are internally used to
    establish routing between the remote node spawning this place and the
    place itself.  The @racket[on-place-dead] callback handles the event
    when the newly spawned place terminates.
  }

 @defmethod[(wait-for-die) void?]{
   Blocks and waits for the subprocess representing the @racket[remote-node%] to exit.
 }
}

@defproc[(node-send-exit [remote-node% node]) void?]{
Sends @racket[node] a message telling it to exit immediately.
}
@defproc[(node-get-first-place [remote-node% node]) remote-place%?]{
Returns the @racket[remote-place%] instance of the first place spawned at this node
}

@defclass[connection% object% (event-container<%>)]{

The @racket[connection%] instance represents a connection to a
named-place instance running on the current node. It routes inter-node
place messages to the named place.

@defconstructor[([vm remote-node%?]
                 [name string?]
                 [ch-id exact-positive-integer?]
                 [sc socket-connection%?])]{
 Constructs a @racket[remote-place%] instance.
 @|place-exec-note|
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

@defconstructor[([seconds (and/c real? (not/c negative?))]
                 [retry (or/c #f nonnegative-integer?) #f]
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



@defform[(define-remote-server name forms ...)]{

Creates a @racket[make-name] function that spawns a place running a instance of the @racket[name]
remote server.  The server sits in a loop waiting for rpc requests from the @racket[define-rpc] functions
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
}

@examples[ #:eval evaler
(define-named-remote-server
   tuple-server

   (define-state h (make-hash))
   (define-rpc (set k v)
     (hash-set! h k v)
     v)
   (define-rpc (get k)
     (hash-ref h k #f)))]

@examples[ #:eval evaler
(define-remote-server
  bank

  (define-state accounts (make-hash))
  (define-rpc (new-account who)
     (match (hash-has-key? accounts who)
       [#t '(already-exists)]
       [else
         (hash-set! accounts who 0)
         (list 'created who)]))
  (define-rpc (removeM who amount)
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
         (list 'invalid-account who)])))]

@defthing[distributed-launch-path path?]{
Contains the path to the distributed places launcher.
}

@defproc[(ssh-bin-path) string?]{
Returns the path to the ssh binary on the local system in string form.
}
@examples[ #:eval evaler
(ssh-bin-path)
]

@defproc[(racket-path) path?]{
Returns the path to the currently executing racket binary on the local system.
}

@;{@examples[ #:eval evaler
(racket-path)
]}

@defform[(get-current-module-path)]{
Returns the path to the current module.
}
@examples[ #:eval evaler
(begin
  (module my-module racket/base
    (require racket/place/distributed)
    (get-current-module-path))
  (require 'my-module))
]

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

@defproc[(write-flush [datum any?] [port port?]) (void)]{
Writes @racket[datum] to @racket[port] and then flushes @racket[port].
}
@examples[ #:eval evaler
(write-flush "Hello World" (current-output-port))
]

@(close-eval evaler)
