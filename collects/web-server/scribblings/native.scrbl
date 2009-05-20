#lang scribble/doc
@(require "web-server.ss")

@title[]{Native Interfaces}

@(require (for-label scheme
                     web-server/lang/native
                     web-server/lang/abort-resume))

@defmodule[web-server/lang/native]{

It is sometimes inconvenient to use @scheme[serial->native] and @scheme[native->serial] throughout your program.
This module provides a macro for creating wrappers.

@defform[#:literals (ho) (define-native (native arg-spec ...) original) #:contracts ([arg-spec ho] [arg-spec _])]{
 Builds an interface around @scheme[original] named @scheme[native] such that calls to @scheme[native] are wrapped in @scheme[serial->native]
 and all arguments marked with @scheme[ho] in @scheme[arg-spec] are assumed to procedures and are wrapped in @scheme[native->serial].
 
 For example,
 @schemeblock[
  (define-native (build-list/native _ ho) build-list)
 ]
 
 is equivalent to
 @schemeblock[
  (define (build-list/native fst snd)
    (serial->native
     (build-list 
      fst
      (lambda args
        (native->serial
         (apply snd args))))))
  ]
 }
        
}
