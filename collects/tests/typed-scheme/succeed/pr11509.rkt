#lang racket/load

(module server racket
  (define-struct pt [x y])
  (define-struct (cpt pt) [color])
  (provide (all-defined-out)))

(module client racket
  (require 'server)
  (match (cpt 100 200 'red)
    [(cpt x y c) (list x y c)]))

(module tclient typed/racket
  (require-typed-struct
   pt ([x : Integer] [y : Integer])
   'server)
  (require-typed-struct
   (cpt pt) ([color : Symbol])
   'server)
  (match (cpt 100 200 'red)
    [(cpt x y c) (list x y c)]))

(require 'client)

(require 'tclient)
