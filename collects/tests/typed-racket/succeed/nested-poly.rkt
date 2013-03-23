#lang typed-scheme

(require typed-racket/base-env/extra-procs)

(: f (All (A ...) (All (B ...) (A ... A -> Integer))))

(define (f . xs) 5)

(: map-with-funcs
   (All (A ...)
        (All (B ...)
             ((B ... B -> A) ... A ->
              (B ... B -> (values A ... A))))))
(define (map-with-funcs . fs)
  (lambda as
    (apply values (map (plambda: (C) ([f : (B ... B -> C)])
                                 (apply f as))
                        fs))))

(inst map-with-funcs Integer Integer Integer Integer)
