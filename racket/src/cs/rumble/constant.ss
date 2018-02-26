(define null '())
(define eof #!eof)

(define (void . args) (chez:void))
(define (void? v) (eq? v (chez:void)))
