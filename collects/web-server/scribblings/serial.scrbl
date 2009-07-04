#lang scribble/doc
@(require "web-server.ss")

@title[]{Serializable Continuations}

@(require (for-label web-server/lang/abort-resume
                     "dummy-stateless-servlet.ss"
                     scheme/serialize))

@defmodule[web-server/lang/abort-resume]{

The main purpose of the stateless language is to provide serializable continuations to your servlet.
    
@defproc[(call-with-serializable-current-continuation [response-generator (continuation? . -> . any)])
         any]{
 Captures the current continuation in a serializable way and calls @scheme[response-generator] with it, returning the result.
          
 This potentially uses resources of the current servlet's @scheme[manager] if @scheme[serial->native] and @scheme[native->serial] were used
 to capture an untransformable context.
}

@defform[(serial->native expr)]{
 @scheme[serial->native] informs the serializing runtime that @scheme[expr] is potentially a call to an untransformed context. 
 This sets up the necessary information for
 @scheme[native->serial] to signal to @scheme[call-with-serializable-current-continuation] to capture the native (and thus unserializable) section
 of the context and store it on the server.
}

@defform[(native->serial expr)]{
 @scheme[native->serial] informs the serializing runtime that @scheme[expr] marks first expression after returning from an untransformed context.
 This captures the
 untransformed context such that @scheme[call-with-serializable-current-continuation] can store it on the server and reference it from serializable
 continuations. 

 For example,
 @schemeblock[
 (build-list 
  3 
  (lambda (i) 
    (call-with-serializable-current-continuation 
     (lambda (k) (serialize k)))))
 ]
 will fail at runtime because @scheme[build-list] is not transformed. However,
 @schemeblock[
 (serial->native 
  (build-list
   3
   (lambda (i)
     (native->serial
      (call-with-serializable-current-continuation 
       (lambda (k) (serialize k)))))))
 ]
 will succeed and @scheme[k] will reference a cell in the current servlet's @scheme[manager] that stores the part of the continuation in
 @scheme[build-list].
}

}
