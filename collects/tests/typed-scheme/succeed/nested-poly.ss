#lang typed-scheme

(require typed-scheme/private/extra-procs)

(: f (All (A ...) (All (B ...) (A ... A -> Integer))))

(define (f . xs) 5)

(: map-with-funcs 
   (All (A ...)
        (All (B ...)
             ((B ... B -> A) ... A ->
              (B ... B -> (values A ... A))))))
(define (map-with-funcs . fs)
  (lambda as
    (apply values (map (lambda: ([f : (B ... B -> A)])
                                 (apply f as))
                        fs))))

(inst map-with-funcs Integer Integer Integer Integer)
