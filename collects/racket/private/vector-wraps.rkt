#lang racket/base

(require '#%flfxnum
         "for.rkt"
         (for-syntax racket/base))

(provide define-vector-wraps)

(define-syntax-rule (define-vector-wraps
                      fXvector-str
                      fXvector? fXvector-length fXvector-ref fXvector-set! make-fXvector
                      unsafe-fXvector-ref unsafe-fXvector-set! unsafe-fXvector-length
                      in-fXvector*
                      in-fXvector
                      for/fXvector
                      for*/fXvector
                      fXvector-copy)
  (...
   (begin
     (define-:vector-like-gen :fXvector-gen unsafe-fXvector-ref)

     (define-in-vector-like in-fXvector*
       fXvector-str fXvector? fXvector-length :fXvector-gen)

     (define-sequence-syntax in-fXvector
       (lambda () #'in-fXvector*)
       (make-in-vector-like 'in-fXvector
                            fXvector-str
                            #'fXvector?
                            #'unsafe-fXvector-length
                            #'in-fXvector*
                            #'unsafe-fXvector-ref))

     (define (list->fXvector l)
       (let ((n (length l)))
         (let ((v (make-fXvector n)))
           (for ((i (in-range n))
                 (x (in-list l)))
             (fXvector-set! v i x))
           v)))

     (define-syntax (for/fXvector stx)
       (syntax-case stx ()
         ((for/fXvector (for-clause ...) body ...)
          (syntax/loc stx
            (list->fXvector 
             (for/list (for-clause ...) body ...))))
         ((for/fXvector #:length length-expr (for-clause ...) body ...)
          (syntax/loc stx
            (let ((len length-expr))
              (unless (exact-nonnegative-integer? len)
                (raise-type-error 'for/fXvector "exact nonnegative integer" len))
              (let ((v (make-fXvector len)))
                (for/fold ((i 0))
                    (for-clause ... 
                                #:when (< i len))
                  (fXvector-set! v i (begin body ...))
                  (add1 i))
                v))))))

     (define-syntax (for*/fXvector stx)
       (syntax-case stx ()
         ((for*/fXvector (for-clause ...) body ...)
          (syntax/loc stx
            (list->fXvector 
             (for*/list (for-clause ...) body ...))))
         ((for*/fXvector #:length length-expr (for-clause ...) body ...)
          (syntax/loc stx
            (let ((len length-expr))
              (unless (exact-nonnegative-integer? len)
                (raise-type-error 'for*/fXvector "exact nonnegative integer" len))
              (let ((v (make-fXvector len)))
                (for*/fold ((i 0))
                    (for-clause ...
                                #:when (< i len))
                  (fXvector-set! v i (begin body ...))
                  (add1 i))
                v))))))

     (define (fXvector-copy flv [start 0] [end (and (fXvector? flv) (fXvector-length flv))])
       (unless (fXvector? flv)
         (raise-type-error 'fXvector-copy fXvector-str flv))
       (unless (exact-nonnegative-integer? start)
         (raise-type-error 'fXvector-copy "non-negative exact integer" start))
       (unless (exact-nonnegative-integer? end)
         (raise-type-error 'fXvector-copy "non-negative exact integer" end))
       (let ([orig-len (fXvector-length flv)])
         (unless (<= start end orig-len)
           (unless (<= start orig-len)
             (raise-mismatch-error 'fXvector-copy 
                                   (format "start index ~s out of range [~a, ~a] for ~a: "
                                           start 0 orig-len fXvector-str)
                                   flv))
           (raise-mismatch-error 'fXvector-copy 
                                 (format "end index ~s out of range [~a, ~a] for ~a: "
                                         end start orig-len fXvector-str)
                                 flv)))
       (let* ([len (- end start)]
              [vec (make-fXvector len)])
         (for ([i (in-range len)])
           (unsafe-fXvector-set! vec i (unsafe-fXvector-ref flv (+ i start))))
         vec)))))
