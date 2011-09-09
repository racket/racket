#lang typed/racket
(define-struct: (A) Base ([prevbase : (Block A)]
                          [elems : (Vectorof A)]))
(define-struct: Mt ())

(define-type-alias Block (All (A) (U Mt (Base A))))

(: get-base : (All (A) ((Block A) -> (Base A))))
(define (get-base block)
 (if (Mt? block)
     (error "" 'get-base)
     (make-Base (Base-prevbase block)
                (Base-elems block))))

(: get-base2 : (All (A) ((Block A) -> (Base A))))
(define (get-base2 block)
 (if (Base? block)
     (make-Base (Base-prevbase block)
                (Base-elems block))
     (error "" 'get-base)))
