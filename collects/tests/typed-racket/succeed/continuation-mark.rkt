#lang racket/load

;; successful typed-untyped interaction with cont marks

(module typed typed/racket
  (provide call-f)

  (: key (Continuation-Mark-Keyof String))
  (define key (make-continuation-mark-key))

  (: call-f (((Continuation-Mark-Keyof String) -> String) -> String))
  (define (call-f f)
    (with-continuation-mark key "hello" (f key))))

(module untyped racket
  (require 'typed)

  (call-f
   (λ (key)
     (string-append (continuation-mark-set-first #f key)
                    " world"))))

(require 'untyped)

