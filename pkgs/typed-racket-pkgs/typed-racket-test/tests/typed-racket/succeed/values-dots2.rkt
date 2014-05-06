#lang typed/racket/base

(: map-with-funcs (All (b c d e ) ((b c d e -> b)
                                  (b c d e -> c) 
                                  (b c d e -> d) 
                                  (b c d e -> e) 
                                  -> 
                                  (b c d e -> (List b c d e)))))
(define (map-with-funcs f1 f2 f3 f4)
  (lambda (b0 c0 d0 e0 )
    (list (f1 b0 c0 d0 e0 )
          (f2 b0 c0 d0 e0 )
          (f3 b0 c0 d0 e0 )
          (f4 b0 c0 d0 e0 ))))

(map-with-funcs + - * /)
