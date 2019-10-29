(define null '())
(define eof #!eof)

(define void
  (case-lambda
   [() (#%void)]
   [(arg) (#%void)]
   [args (#%void)]))
(define (void? v) (eq? v (#%void)))
