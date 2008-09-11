#;
(exn-pred 7)
#lang typed-scheme

(require typed-scheme/private/extra-procs)

(: map-with-funcs (All (b ...) ((b ... b -> b) ... b -> (b ... b -> (values b ... b)))))
(define (map-with-funcs . fs)
  (lambda bs
    (apply values (map (lambda: ([f : (b ... b -> b)])
                         (apply f bs)) fs))))

(map-with-funcs (lambda () 1))

(map-with-funcs (lambda: ([x : Integer] [y : Integer] . [z : Integer *])
                         (+ x y)))

(map-with-funcs (lambda: ([x : Integer] [y : Integer])
                         (+ x y)))

(map-with-funcs + - * / string-append)

((map-with-funcs + - * /) 1 2 3)
((map-with-funcs + - * /) 1 2 3 4 5)
((map-with-funcs + - * /) 1 2 3 "foo")
