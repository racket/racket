#lang typed/racket

(struct: (a) foo ((v : ((baz a) -> Symbol))))
(struct: (a) bar ((v : ((foo a) -> Symbol))))
(struct: (a) baz ((v : ((bar a) -> Symbol))))

(ann (ann (foo (lambda (x) 'x)) (foo String)) (foo Symbol))
