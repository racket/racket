#;
(exn-pred exn:fail:contract?)
#lang racket/load

;; check typed-untyped interaction with cont marks

(module typed typed/racket
  (provide call-f)

  (: key (Continuation-Mark-Keyof (Integer -> String)))
  (define key (make-continuation-mark-key))

  (: call-f (((Continuation-Mark-Keyof (Integer -> String)) -> String)
             -> String))
  (define (call-f f)
    (with-continuation-mark
     key (λ (n) (number->string n))
     (f key))))

(module untyped racket
  (require 'typed)

  (call-f
   (λ (key)
     (string-append "hello "
                    ((continuation-mark-set-first #f key) 'bad)))))

(require 'untyped)

