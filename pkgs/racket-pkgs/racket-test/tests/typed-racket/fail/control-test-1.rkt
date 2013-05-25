#;
(exn-pred exn:fail:contract?)
#lang racket/load

;; check typed-untyped interaction with continuations

;; continuations & prompt tags
(module typed typed/racket
  (provide call-f)

  (: tag (Prompt-Tagof String (Integer -> String)))
  (define tag (make-continuation-prompt-tag))

  (: call-f (((Prompt-Tagof String (Integer -> String)) -> String) -> String))
  (define (call-f f)
    (call-with-continuation-prompt
     (λ () (f tag))
     tag
     (λ: ([x : Integer]) (number->string x)))))

(module untyped racket
  (require 'typed)

  (call-f
   (λ (tag)
     (abort-current-continuation tag "bad"))))

(require 'untyped)
