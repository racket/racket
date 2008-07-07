#;
(exn-pred 2)
#lang typed-scheme

(require typed-scheme/private/extra-procs)

(: map-with-funcs (All (b ...) ((b ... b -> b) ... b -> (b ... b -> (values b ... b)))))
(define (map-with-funcs . fs)
  (lambda bs
    (apply values* (map (lambda: ([f : (b ... b -> b)])
                          (apply f bs)) fs))))

(map-with-funcs (lambda () 1))

(map-with-funcs (lambda: ([x : Integer] [y : Integer] . [z : Integer *])
                         (+ x y)))
