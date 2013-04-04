#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "managers"]{Continuation Managers}

Since Racket servlets store their continuations on the server, they take
up memory on the server. Furthermore, garbage collection can not be used
to free this memory, because there are roots outside the system: users'
browsers, bookmarks, brains, and notebooks. Therefore, some other strategy
must be used if memory usage is to be controlled. This functionality is
pluggable through the @deftech{manager} interface.

@; ------------------------------------------------------------
@section[#:tag "manager"]{General}
@(require (for-label web-server/managers/manager)
          (for-label web-server/servlet/servlet-structs))

@defmodule[web-server/managers/manager]{

This module defines the manager interface. It is required by
the users and implementors of managers.

@defstruct[manager ([create-instance ((-> void) . -> . number?)]
                    [adjust-timeout! (number? number? . -> . void)]
                    [clear-continuations! (number? . -> . void)]
                    [continuation-store! (number? any/c 
                                                  (or/c false/c
                                                        (request? . -> . can-be-response?))
                                                  . -> . (list/c number? number?))]
                    [continuation-lookup (number? number? number? . -> . any/c)]
                    [continuation-peek (number? number? number? . -> . any/c)])]{
 @racket[create-instance] is called to initialize a instance, to hold the
 continuations of one servlet session. It is passed
 a function to call when the instance is expired. It runs the id of the
 instance.

 @racket[adjust-timeout!] is a to-be-deprecated function that takes an
 instance-id and a number. It is specific to the timeout-based manager
 and will be removed.

 @racket[clear-continuations!] expires all the continuations of an instance.

 @racket[continuation-store!] is given an instance-id, a continuation value,
 and a function to include in the exception thrown if the continuation is
 looked up and has been expired. The two numbers returned are a
 continuation-id and a nonce.

 @racket[continuation-lookup] finds the continuation value associated with
 the instance-id, continuation-id, and nonce triple it is given.
 
 @racket[continuation-peek] is identical to @racket[continuation-lookup] except that
 its use must not affect the resource management policy decisions on the instance or
 continuation accessed. It is intended to be used by debuggers and benchmarks.
}

@defstruct[(exn:fail:servlet-manager:no-instance exn:fail)
           ([expiration-handler 
             (or/c false/c
                   (request? . -> . can-be-response?))])]{
 This exception should be thrown by a manager when an instance is looked
 up that does not exist.
}

@defstruct[(exn:fail:servlet-manager:no-continuation exn:fail)
           ([expiration-handler 
             (or/c false/c
                   (request? . -> . can-be-response?))])]{
 This exception should be thrown by a manager when a continuation is
 looked up that does not exist.
}

}

@; ------------------------------------------------------------
@section[#:tag "none"]{No Continuations}
@(require (for-label web-server/managers/none))

@defmodule[web-server/managers/none]{

This module defines a manager constructor:

@defproc[(create-none-manager 
          (instance-expiration-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))))
         manager?]{
 This manager does not actually store any continuation or instance data.
 You could use it if you know your servlet does not use the continuation
 capturing functions and want the server to not allocate meta-data
 structures for each instance.
 
 If you @emph{do} use a continuation capturing function, the continuation is
 simply not stored. If the URL is visited, the @racket[instance-expiration-handler]
 is called with the request.
}

If you are considering using this manager, also consider using the
Web Language. (See @secref["stateless"].)

}

@; ------------------------------------------------------------
@section[#:tag "timeouts"]{Timeouts}
@(require (for-label web-server/managers/timeouts))

@defmodule[web-server/managers/timeouts]{

This module defines a manager constructor:

@defproc[(create-timeout-manager 
          [instance-exp-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))]
          [instance-timeout number?]
          [continuation-timeout number?])
         manager?]{
 Instances managed by this manager will be expired @racket[instance-timeout]
 seconds after the last time it is accessed. If an expired instance is
 looked up, the @racket[exn:fail:servlet-manager:no-instance] exception
 is thrown with @racket[instance-exp-handler] as the expiration handler.

 Continuations managed by this manager will be expired @racket[continuation-timeout]
 seconds after the last time it is accessed. If an expired continuation is looked
 up, the @racket[exn:fail:servlet-manager:no-continuation] exception
 is thrown with @racket[instance-exp-handler] as the expiration handler, if
 no expiration-handler was passed to @racket[continuation-store!].

 @racket[adjust-timeout!] corresponds to @racket[reset-timer!] on the timer
 responsible for the servlet instance.
}

This manager has been found to be... problematic... in large-scale
deployments of the @web-server .

}

@; ------------------------------------------------------------
@section[#:tag "lru"]{LRU}
@(require (for-label web-server/managers/lru))

@defmodule[web-server/managers/lru]{

This module defines a manager constructor:

@defproc[(create-LRU-manager
          [instance-expiration-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))]
          [check-interval integer?]
          [collect-interval integer?]
          [collect? (-> boolean?)]
          [#:initial-count initial-count integer? 1]
          [#:inform-p inform-p (integer? . -> . void) (lambda _ (void))])
         manager?]{
 Instances managed by this manager will be expired if there are no
 continuations associated with them, after the instance is unlocked.
 If an expired instance is looked up, the
 @racket[exn:fail:servlet-manager:no-instance] exception
 is thrown with @racket[instance-exp-handler] as the expiration handler.

 Continuations managed by this manager are given a "Life Count" of
 @racket[initial-count] initially. If an expired continuation is looked
 up, the @racket[exn:fail:servlet-manager:no-continuation] exception
 is thrown with @racket[instance-exp-handler] as the expiration handler, if
 no expiration-handler was passed to @racket[continuation-store!].

 Every @racket[check-interval] seconds @racket[collect?] is called to determine
 if the collection routine should be run. Every @racket[collect-interval] seconds
 the collection routine is run.

 Every time the collection routine runs, the "Life Count" of every
 continuation is decremented by @racket[1]. If a continuation's count
 reaches @racket[0], it is expired. The @racket[inform-p] function
 is called if any continuations are expired, with the number of
 continuations expired.
}
                  
The recommended usage of this manager is codified as the following function:

@defproc[(make-threshold-LRU-manager 
          [instance-expiration-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))]
          [memory-threshold number?])
         manager?]{
 This creates an LRU manager with the following behavior:
 The memory limit is set to @racket[memory-threshold] bytes. Continuations start with @racket[24]
 life points. Life points are deducted at the rate of one every @racket[10] minutes, or one
 every @racket[5] seconds when the memory limit is exceeded. Hence the maximum life time for
 a continuation is @racket[4] hours, and the minimum is @racket[2] minutes.
 
 If the load on the server spikes---as indicated by memory usage---the server will quickly expire
 continuations, until the memory is back under control. If the load
 stays low, it will still efficiently expire old continuations.
}
 
}
