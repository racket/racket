#lang scribble/doc
@(require "web-server.rkt")

@title[]{Serializable Continuations}

@(require (for-label web-server/lang/abort-resume
                     "dummy-stateless-servlet.rkt"
                     racket/serialize))

@defmodule[web-server/lang/abort-resume]{

The main purpose of the stateless language is to provide serializable continuations to your servlet.
    
@defproc[(call-with-serializable-current-continuation [response-generator (continuation? . -> . any)])
         any]{
 Captures the current continuation in a serializable way and calls @racket[response-generator] with it, returning the result.
          
 This potentially uses resources of the current servlet's @racket[manager] if @racket[serial->native] and @racket[native->serial] were used
 to capture an untransformable context.
}

@defform[(serial->native expr)]{
 @racket[serial->native] informs the serializing runtime that @racket[expr] is potentially a call to an untransformed context. 
 This sets up the necessary information for
 @racket[native->serial] to signal to @racket[call-with-serializable-current-continuation] to capture the native (and thus unserializable) section
 of the context and store it on the server.
}

@defform[(native->serial expr)]{
 @racket[native->serial] informs the serializing runtime that @racket[expr] marks first expression after returning from an untransformed context.
 This captures the
 untransformed context such that @racket[call-with-serializable-current-continuation] can store it on the server and reference it from serializable
 continuations. 

 For example,
 @racketblock[
 (build-list 
  3 
  (lambda (i) 
    (call-with-serializable-current-continuation 
     (lambda (k) (serialize k)))))
 ]
 will fail at runtime because @racket[build-list] is not transformed. However,
 @racketblock[
 (serial->native 
  (build-list
   3
   (lambda (i)
     (native->serial
      (call-with-serializable-current-continuation 
       (lambda (k) (serialize k)))))))
 ]
 will succeed and @racket[k] will reference a cell in the current servlet's @racket[manager] that stores the part of the continuation in
 @racket[build-list].
}

}
