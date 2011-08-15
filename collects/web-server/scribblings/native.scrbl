#lang scribble/doc
@(require "web-server.rkt")

@title[]{Native Interfaces}

@(require (for-label racket
                     web-server/lang/native
                     web-server/lang/abort-resume))

@defmodule[web-server/lang/native]{

It is sometimes inconvenient to use @racket[serial->native] and
@racket[native->serial] throughout your program.  This module provides a
macro for creating wrappers.

@defform[#:literals (ho)
         (define-native (native arg-spec ...) original)
         #:contracts ([arg-spec ho] [arg-spec _])]{
 Builds an interface around @racket[original] named @racket[native] such
 that calls to @racket[native] are wrapped in @racket[serial->native]
 and all arguments marked with @racket[ho] in @racket[arg-spec] are
 assumed to procedures and are wrapped in @racket[native->serial].

 For example,
 @racketblock[
  (define-native (build-list/native _ ho) build-list)
 ]

 is equivalent to
 @racketblock[
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
