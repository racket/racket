#lang scheme
(require '#%flfxnum)

(provide fl+ fl- fl* fl/
         flabs flsqrt flexp fllog
         flsin flcos fltan flasin flacos flatan
         flfloor flceiling flround fltruncate
         fl= fl< fl<= fl> fl>= flmin flmax
         ->fl fl->exact-integer
         flvector? flvector make-flvector 
         flvector-length flvector-ref flvector-set!
         flreal-part flimag-part make-flrectangular
         in-flvector for/flvector for*/flvector)

(define (in-flvector* flv)
  (let ((n (flvector-length flv)))
    (make-do-sequence 
     (lambda ()
       (values (lambda (i) (flvector-ref flv i))
               add1
               0
               (lambda (i) (fx< i n))
               (lambda (x) #t)
               (lambda (i x) #t))))))

(define-sequence-syntax in-flvector
  (lambda () (syntax in-flvector*))
  (lambda (stx)
    (syntax-case stx ()
      (((x) (in-flvector flv-expr))
       (syntax/loc stx
         (() (:do-in (((v) flv-expr))
                     (when (not (flvector? v))
                       (error 'in-flvector "expecting a flvector, got ~a" v))
                     ((i 0) (n (flvector-length v)))
                     (fx< i n)
                     (((x) (flvector-ref v i)))
                     #t
                     #t
                     ((add1 i) n))))))))

(define (list->flvector l)
  (let ((n (length l)))
    (let ((v (make-flvector n)))
      (for ((i (in-range n))
            (x (in-list l)))
        (flvector-set! v i x))
      v)))

(define-syntax for/flvector
  (lambda (stx)
    (syntax-case stx ()
      ((for/flvector (for-clause ...) body)
       (syntax/loc stx
         (list->flvector (for/list (for-clause ...) body))))
      ((for/flvector len-expr (for-clause ...) body)
       (syntax/loc stx
         (let ((flv (make-flvector len-expr)))
           (for ((i (in-naturals))
                 for-clause 
                 ...)
             (flvector-set! flv i body))
           flv))))))

(define-syntax for*/flvector
  (lambda (stx)
    (syntax-case stx ()
      ((for*/flvector (for-clause ...) body)
       (syntax/loc stx
         (list->flvector (for*/list (for-clause ...) body))))
      ((for*/flvector length-expr (for-clause ...) body)
       (syntax/loc stx
         (for*/flvector (for-clause ...) body))))))