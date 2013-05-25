#;
(exn-pred exn:fail:contract?)
#lang racket/load

;; check typed-untyped interaction with call/cc

;; continuations & prompt tags
(module typed typed/racket
  (provide tag call-f)

  (: tag (Prompt-Tagof String (Integer -> String)))
  (define tag (make-continuation-prompt-tag))

  (: call-f (((Prompt-Tagof String (Integer -> String)) -> String) -> String))
  (define (call-f f)
    (call-with-continuation-prompt
     (λ () (f tag))
     tag
     (λ: ([x : Integer]) (number->string x)))))

;; call/cc
(module untyped racket
  (require 'typed)

  ;; construct an abortive continuation
  (define (make-abort-k tag)
    (call-with-continuation-prompt
     (λ () (call/cc (λ (k) k) tag))
     tag))

  (call-f
   (λ (tag) ((make-abort-k tag) 'bad))))

(require 'untyped)
