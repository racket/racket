#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/sendevent))

@(begin
   (define-syntax-rule (bind id)
     (begin
       (require (for-label scheme/gui/base))
       (define id (racket send-event))))
   (bind mred-send-event))

@mzlib[#:mode title sendevent]

The @racketmodname[mzlib/sendevent] library provides a
@racket[send-event] function that works only on Mac OS X, and only
when running in GRacket (though the library can be loaded in Racket).

@defproc[(send-event [receiver-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [event-class-bytes (lambda (s) (and (bytes? s)
                                                         (= 4 (bytes-length s))))]
                     [event-id-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [direct-arg-v any/c (void)]
                     [argument-list list? null])
         any/c]{

Calls @|mred-send-event| @racketmodname[scheme/gui/base], if
available, otherwise raises @racket[exn:fail:unsupported].}
