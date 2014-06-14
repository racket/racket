#lang typed/scheme/base

(: map-with-funcs (All (b ...) ((List (b ... b -> b) ... b ) -> (b ... b -> (values b ... b)))))
(define (map-with-funcs fs)
  (lambda bs
    (apply values (map (plambda: (c) ([f : (b ... b -> c)])
                         (apply f bs)) fs))))

(map-with-funcs (list + - /))

