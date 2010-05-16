#lang scribble/doc
@(require "common.ss"
          (for-label mzlib/sendevent))

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require (for-label scheme/gui/base))
     (define id (scheme send-event))))
  (bind mred-send-event))

@mzlib[#:mode title sendevent]

The @schememodname[mzlib/sendevent] library provides a
@scheme[send-event] function that works only on Mac OS X, and only
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

Calls @|mred-send-event| @schememodname[scheme/gui/base], if
available, otherwise raises @scheme[exn:fail:unsupported].}
