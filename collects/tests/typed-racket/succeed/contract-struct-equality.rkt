#lang racket/load

(module typed typed/racket
 (provide g)
 (struct: (A) foo ((v : A)))
 (: f (foo Byte))
 (define f (foo 2))
 (: g (-> (foo Byte)))
 (define (g) f))

(module typed-client typed/racket
  (require 'typed)
  (unless (equal? (g) (g))
    (error 'typed2 "Failed")))


(module untyped-client racket
  (require 'typed)
  (unless (equal? (g) (g))
    (error 'typed2 "Failed")))

(require 'typed-client)
(require 'untyped-client)
