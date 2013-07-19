#lang racket/base

(require '#%flfxnum
         "for.rkt"
         racket/unsafe/ops
         (for-syntax racket/base))

(provide define-vector-wraps)

(define-syntax-rule (define-vector-wraps
                      fXvector-str
                      fX?-str fX?
                      fXvector? fXvector-length fXvector-ref fXvector-set! make-fXvector
                      unsafe-fXvector-ref unsafe-fXvector-set! unsafe-fXvector-length
                      in-fXvector*
                      in-fXvector
                      for/fXvector
                      for*/fXvector
                      fXvector-copy
                      fXzero)
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

     (define (unsafe-fXvector-copy! vec dest-start flv start end)
       (let ([len (- end start)])
         (for ([i (in-range len)])
           (unsafe-fXvector-set! vec (unsafe-fx+ i dest-start) 
                                 (unsafe-fXvector-ref flv (unsafe-fx+ i start))))))

     (define (grow-fXvector vec)
       (define n (fXvector-length vec))
       (define new-vec (make-fXvector (* 2 n)))
       (unsafe-fXvector-copy! new-vec 0 vec 0 n)
       new-vec)
     
     (define (shrink-fXvector vec i)
       (define new-vec (make-fXvector i))
       (unsafe-fXvector-copy! new-vec 0 vec 0 i)
       new-vec)

     (define (not-an-fX who v)
       (raise-argument-error who fX?-str v))

     (define-for-syntax (for_/fXvector stx orig-stx for_/fXvector-stx for_/fold/derived-stx wrap-all?)
       (syntax-case stx ()
         [(for*/fXvector (for-clause ...) body ...)
          (with-syntax ([orig-stx orig-stx]
                        [for_/fold/derived for_/fold/derived-stx]
                        [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))])
            (syntax/loc stx
              (let-values ([(vec i)
                            (for_/fold/derived
                             orig-stx
                             ([vec (make-fXvector 16)]
                              [i 0])
                             (for-clause ...) 
                             middle-body ...
                             (let ([new-vec (if (eq? i (unsafe-fXvector-length vec))
                                                (grow-fXvector vec)
                                                vec)])
                               (let ([elem (let () last-body ...)])
                                 (if (fX? elem)
                                     (unsafe-fXvector-set! new-vec i elem)
                                     (not-an-fX 'for*/fXvector elem)))
                               (values new-vec (unsafe-fx+ i 1))))])
                (shrink-fXvector vec i))))]
         [(for*/fXvector #:length length-expr #:fill fill-expr (for-clause ...) body ...)
          (with-syntax ([orig-stx orig-stx]
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
                                                         (unsafe-fx= i len)))])]
                                 [_ fc]))
                             (cons wrapped-fc
                                   (loop (cdr fcs) wrap-all?))]))]
                        [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))]
                        [for_/fXvector for_/fXvector-stx]
                        [for_/fold/derived for_/fold/derived-stx])
            (syntax/loc stx
              (let ([len length-expr])
                (unless (exact-nonnegative-integer? len)
                  (raise-argument-error 'for_/fXvector "exact-nonnegative-integer?" len))
                (let ([fill fill-expr])
                  (let ([v (make-fXvector len fill)])
                    (unless (zero? len)
                      (for_/fold/derived
                       orig-stx 
                       ([i 0])
                       (limited-for-clause ...)
                       middle-body ...
                       (let ([elem (let () last-body ...)])
                         (if (fX? elem)
                             (unsafe-fXvector-set! v i elem)
                             (not-an-fX 'for*/vector elem)))
                       (unsafe-fx+ 1 i)))
                    v)))))]
         [(_ #:length length-expr (for-clause ...) body ...)
          (for_/fXvector #'(fv #:length length-expr #:fill fXzero (for-clause ...) body ...) 
                         orig-stx for_/fXvector-stx for_/fold/derived-stx wrap-all?)]))

     (define-syntax (for/fXvector stx)
       (for_/fXvector stx stx #'for/fXvector #'for/fold/derived #f))

     (define-syntax (for*/fXvector stx)
       (for_/fXvector stx stx #'for*/fXvector #'for*/fold/derived #t))

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
           (unsafe-fXvector-set! vec i (unsafe-fXvector-ref flv (unsafe-fx+ i start))))
         vec)))))
