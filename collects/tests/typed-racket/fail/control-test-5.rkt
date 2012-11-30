#;
(exn-pred exn:fail:contract?)
#lang racket/load

;; check typed-untyped interaction with cont marks

(module typed typed/racket
  (provide key f)

  (: key (Continuation-Mark-Keyof String))
  (define key (make-continuation-mark-key))

  (: f (-> String))
  (define (f)
    (apply string-append
           (continuation-mark-set->list
            (current-continuation-marks)
            key))))

(module untyped racket
  (require 'typed)

  (with-continuation-mark
   key 'hello ; should be string
   (f)))

(require 'untyped)

