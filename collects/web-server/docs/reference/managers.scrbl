#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:style 'toc]{Continuation Managers}

Since Scheme servlets store their continuations on the server, they take
up memory on the server. Furthermore, garbage collection can not be used
to free this memory, because there are roots outside the system: users' 
browsers, bookmarks, brains, and notebooks. Therefore, some other strategy
must be used if memory usage is to be controlled. This functionality is
pluggable through the manager interface.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "manager"]{General}

@file{managers/manager.ss} defines the manager interface. It is required by
the users and implementers of managers.

@defstruct[manager ([create-instance (any/c (-> void) . -> . number?)]
                    [adjust-timeout! (number? number? . -> . void)]
                    [instance-lookup-data (number? . -> . any/c)]
                    [instance-lock! (number? . -> . void)]
                    [instance-unlock! (number? . -> . void)]
                    [clear-continuations! (number? . -> . void)]
                    [continuation-store! (number? any/c expiration-handler? . -> . (list/c number? number?))]
                    [continuation-lookup (number? number? number? . -> . any/c)])]{
 @scheme[create-instance] is called to initialize a instance, to hold the
 continuations of one servlet session. It is passed some arbitrary data and 
 a function to call when the instance is expired. It runs the id of the
 instance.
 
 @scheme[adjust-timeout!] is a to-be-deprecated function that takes an
 instance-id and a number. It is specific to the timeout-based manager
 and will be removed.
 
 @scheme[instance-lookup-data] accesses the arbitrary data passed into
 @scheme[create-instance] match by the given instance-id.
 
 @scheme[instance-lock!] and @scheme[instance-unlock!] lock and unlock
 access to a particular instance.
 
 @scheme[clear-continuations!] expires all the continuations of an instance.
 
 @scheme[continuation-store!] is given an instance-id, a continuation value,
 and a function to include in the exception thrown if the continuation is 
 looked up and has been expired. The two numbers returned are a
 continuation-id and a random nonce.
 
 @scheme[continuation-lookup] finds the continuation value associated with
 the instance-id, continuation-id, and nonce triple it is given.
}
                                                                                  
@defstruct[(exn:fail:servlet-manager:no-instance exn:fail) 
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]{
 This exception should be thrown by a manager when an instance is looked
 up that does not exist.
}
                                                      
@defstruct[(exn:fail:servlet-manager:no-continuation exn:fail)
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [expiration-handler expiration-handler?])]{
 This exception should be thrown by a manager when a continuation is
 looked up that does not exist.
}                                                       

@; ------------------------------------------------------------
@section[#:tag "none"]{No Continuations}

@file{managers/none.ss} defines a manager constructor:

@defproc[(create-none-manager (instance-expiration-handler expiration-handler?))
         manager?]{
 This manager does not actually store any continuation or instance data.
 You could use it if you know your servlet does not use the continuation
 capturing functions and want the server to not allocate meta-data
 structures for each instance.
} 

If you are considering using this manager, also consider using the
Web Language. (See @secref["lang"].)

@; ------------------------------------------------------------
@section[#:tag "timeouts"]{Timeouts}

@; ------------------------------------------------------------
@section[#:tag "lru"]{LRU}

XXX