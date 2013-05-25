#lang scribble/doc 
@(require "common.rkt" 
          (for-label racket/future 
                     future-visualizer/trace)) 

@title[#:tag "futures-trace"]{Futures Tracing} 

@defmodule[future-visualizer/trace]

The @deftech{futures trace} module exposes low-level information about 
the execution of parallel programs written using @racket[future].   


@deftogether[(
  @defform[(trace-futures e ...)] 
  @defproc[(trace-futures-thunk [thunk (-> any)]) (listof indexed-future-event?)]
)]{
 The @racket[trace-futures] macro and @racket[trace-futures-thunk] function 
 track the execution of a program using futures and return the program 
 trace as a list of @racket[indexed-future-event] structures.
 
 This program:
 
 @racketblock[ 
    (require racket/future 
             future-visualizer/trace) 
     
    (trace-futures  
     (let ([f (future (lambda () ...))]) 
       ... 
       (touch f)))
 ]
 
 Is equivalent to:
 
 @racketblock[ 
    (require racket/future 
             future-visualizer/trace) 
                                      
    (start-future-tracing!) 
    (let ([f (future (lambda () ...))]) 
      ... 
      (touch f)) 
    (stop-future-tracing!)
    (timeline-events)
 ]
}

@deftogether[(
  @defproc[(start-future-tracing!) void?]
  @defproc[(stop-future-tracing!) void?]
  @defproc[(timeline-events) (listof indexed-future-event?)]
)]{
 The @racket[start-future-tracing!] procedure enables the collection 
 of future-related execution data.  This function should be called immediately 
 prior to executing code the programmer wishes to profile. 
 
 The @racket[stop-future-tracing!] procedure must be used to indicate the 
 end of code the programmer wishes to trace.  Tracing works by simply using a 
 log receiver to record all future-related log events; this procedure logs a 
 special message that is well-known to the log receiver to mean 'stop recording'.
 
 The @racket[timeline-events] procedure returns the program trace as 
 a list of @racket[indexed-future-event] structures. 
}
  
@defstruct[indexed-future-event ([index exact-nonnegative-integer?] 
                                 [event any])]{
  Represents an individual log message in a program trace.  In addition to 
  future events, the tracing code also records garbage collection events; hence 
  the @racket[event] field may contain either a @racket[future-event] or gc-info 
  @(tech "prefab" #:doc '(lib "scribblings/reference/reference.scrbl")) struct (see @refsecref["garbagecollection"]), 
  where the latter describes a GC operation.  Because multiple 
  @racket[future-event] structures may contain identical timestamps, the 
  @racket[index] field ranks them in the order in which they were recorded 
  in the log output.
}
                                                                      
@defstruct[future-event ([future-id (or exact-nonnegative-integer? #f)]
                         [proc-id exact-nonnegative-integer?] 
                         [action symbol?] 
                         [time-id real?] 
                         [prim-name (or symbol? #f)] 
                         [user-data (or #f symbol? exact-nonnegative-integer?)]) 
                        #:prefab]{
 Represents a future event as logged by the run-time system. See
@refsecref["future-logging"] for more information.}
