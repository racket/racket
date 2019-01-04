(define null '())
(define eof #!eof)

(define (void . args) (#%void))
(define (void? v) (eq? v (#%void)))
