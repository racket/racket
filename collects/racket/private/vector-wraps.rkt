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

     (define-for-syntax (for_/fXvector stx for_/fXvector-stx for_/fold/derived-stx wrap-all?)
       (syntax-case stx ()
         [(for*/fXvector (for-clause ...) body ...)
          (with-syntax ([orig-stx stx]
                        [for_/fold/derived for_/fold/derived-stx])
            (syntax/loc stx
              (list->fXvector
               (reverse
                (for_/fold/derived
                 orig-stx
                 ([l null])
                 (for-clause ...) 
                 (cons (let () body ...) l))))))]
         [(for*/fXvector #:length length-expr (for-clause ...) body ...)
          (with-syntax ([orig-stx stx]
                        [(limited-for-clause ...)
                         ;; If `wrap-all?', wrap all binding clauses. Otherwise, wrap
                         ;; only the first and the first after each keyword clause:
                         (let loop ([fcs (syntax->list #'(for-clause ...))] [wrap? #t])
                           (cond
                            [(null? fcs) null]
                            [(keyword? (syntax-e (car fcs)))
                             (if (null? (cdr fcs))
                                 fcs
                                 (list* (car fcs) (cadr fcs) (loop (cddr fcs) #t)))]
                            [(not wrap?)
                             (cons (car fcs) (loop (cdr fcs) #f))]
                            [else
                             (define fc (car fcs))
                             (define wrapped-fc
                               (syntax-case fc ()
                                 [[ids rhs]
                                  (or (identifier? #'ids)
                                      (let ([l (syntax->list #'ids)])
                                        (and l (andmap identifier? l))))
                                  (syntax/loc fc [ids (stop-after
                                                       rhs
                                                       (lambda x
                                                         (= i len)))])]
                                 [_ fc]))
                             (cons wrapped-fc
                                   (loop (cdr fcs) wrap-all?))]))]
                        [for_/fXvector for_/fXvector-stx]
                        [for_/fold/derived for_/fold/derived-stx])
            (syntax/loc stx
              (let ([len length-expr])
                (unless (exact-nonnegative-integer? len)
                  (raise-argument-error 'for_/fXvector "exact-nonnegative-integer?" len))
                (let ([v (make-fXvector len)])
                  (unless (zero? len)
                    (for_/fold/derived
                     orig-stx 
                     ([i 0])
                     (limited-for-clause ...)
                     (fXvector-set! v i (let () body ...))
                     (add1 i)))
                  v))))]))

     (define-syntax (for/fXvector stx)
       (for_/fXvector stx #'for/fXvector #'for/fold/derived #f))

     (define-syntax (for*/fXvector stx)
       (for_/fXvector stx #'for*/fXvector #'for*/fold/derived #t))

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
