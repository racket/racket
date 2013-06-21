#;#;
#<<END
TR info: dead-substructs.rkt 28:4 make-child1 -- struct constructor
TR info: dead-substructs.rkt 29:4 make-child2 -- struct constructor
END
#<<END
1
2

END

#lang typed/scheme
#:optimize

;; originally from nucleic3
;; cond on substructs, branches were considered dead

(define-struct: parent ((x : Integer)))
(define-struct: (child1 parent) ((y : Integer)))
(define-struct: (child2 parent) ((y : Float)))

(: f (parent -> Integer))
(define (f x)
  (cond [(child1? x) 1]
        [(child2? x) 2]
        [else (error "eh?")]))

(f (make-child1 1 2))
(f (make-child2 1 2.0))
