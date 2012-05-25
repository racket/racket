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
                (raise-argument-error 'for/fXvector "exact-nonnegative-integer?" len))
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
                (raise-argument-error 'for*/fXvector "exact-nonnegative-integer?" len))
              (let ((v (make-fXvector len)))
                (for*/fold ((i 0))
                    (for-clause ...
                                #:when (< i len))
                  (fXvector-set! v i (begin body ...))
                  (add1 i))
                v))))))

     (define (fXvector-copy flv [start 0] [end (and (fXvector? flv) (fXvector-length flv))])
       (unless (fXvector? flv)
         (raise-argument-error 'fXvector-copy (string-append fXvector-str "?") flv))
       (unless (exact-nonnegative-integer? start)
         (raise-argument-error 'fXvector-copy "exact-nonnegative-integer?" start))
       (unless (exact-nonnegative-integer? end)
         (raise-argument-error 'fXvector-copy "exact-nonnegative-integer?" end))
       (let ([orig-len (fXvector-length flv)])
         (unless (<= start end orig-len)
           (unless (<= start orig-len)
             (raise-range-error 'fXvector-copy fXvector-str "starting "
                                start flv 0 orig-len))
           (raise-mismatch-error 'fXvector-copy fXvector-str "ending "
                                 end flv start orig-len)))
       (let* ([len (- end start)]
              [vec (make-fXvector len)])
         (for ([i (in-range len)])
           (unsafe-fXvector-set! vec i (unsafe-fXvector-ref flv (+ i start))))
         vec)))))
