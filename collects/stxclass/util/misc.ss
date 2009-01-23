#lang scheme/base
(require syntax/kerncase
         syntax/stx)

(provide with-temporaries
         generate-temporary
         generate-n-temporaries

         chunk-kw-seq/no-dups
         chunk-kw-seq
         reject-duplicate-chunks
         check-id
         check-nat/f
         check-string
         check-idlist)


;; Generating temporaries

;; with-temporaries
(define-syntax-rule (with-temporaries (temp-name ...) . body)
  (with-syntax ([(temp-name ...) (generate-temporaries (quote-syntax (temp-name ...)))])
    . body))

;; generate-temporary : any -> identifier
(define (generate-temporary [stx 'g])
  (car (generate-temporaries (list stx))))

;; generate-n-temporaries : exact-nonnegative-integer -> (listof identifier)
(define (generate-n-temporaries n)
  (generate-temporaries
   (for/list ([i (in-range n)])
     (string->symbol (format "g~sx" i)))))


;; Parsing keyword arguments

;; chunk-kw-seq/no-dups : syntax
;;                        alist[keyword => (listof (stx -> any))]
;;   -> (values (listof (cons kw (cons stx(kw) (listof any)))) stx)
(define (chunk-kw-seq/no-dups stx kws #:context [ctx #f])
  (let-values ([(chunks rest) (chunk-kw-seq stx kws #:context ctx)])
    (reject-duplicate-chunks chunks)
    (values chunks rest)))

;; chunk-kw-seq : stx
;;                alist[keyword => (listof (stx -> any))
;;   -> (values (listof (cons kw (cons stx(kw) (listof any)))) stx)
(define (chunk-kw-seq stx kws #:context [ctx #f])
  (define (loop stx rchunks)
    (syntax-case stx ()
      [(kw . more)
       (and (keyword? (syntax-e #'kw)) (assq (syntax-e #'kw) kws))
       (let* ([kw-value (syntax-e #'kw)]
              [arity (cdr (assq kw-value kws))]
              [args+rest (stx-split #'more arity)])
         (if args+rest
             (loop (cdr args+rest)
                   (cons (list* kw-value #'kw (car args+rest)) rchunks))
             (raise-syntax-error #f "too few arguments for keyword" #'kw ctx)))]
      [(kw . more)
       (keyword? (syntax-e #'kw))
       (raise-syntax-error #f "unexpected keyword" ctx #'kw)]
      [_
       (values (reverse rchunks) stx)]))
  (loop stx null))

;; reject-duplicate-chunks : (listof (cons kw (cons stx(kw) (listof any)))) -> void
(define (reject-duplicate-chunks chunks #:context [ctx #f])
  (define kws (make-hasheq))
  (define (loop chunks)
    (when (pair? chunks)
      (let ([kw (caar chunks)])
        (when (hash-ref kws kw #f)
          (raise-syntax-error #f "duplicate keyword argument" (cadar chunks) ctx))
        (hash-set! kws kw #t))
      (loop (cdr chunks))))
  (loop chunks))

;; stx-split : stx nat -> (cons (listof stx) stx)
(define (stx-split stx procs)
  (define (loop stx procs acc)
    (cond [(null? procs)
           (cons (reverse acc) stx)]
          [(stx-pair? stx)
           (loop (stx-cdr stx) (cdr procs) (cons ((car procs) (stx-car stx)) acc))]
          [else #f]))
  (loop stx procs null))

;; check-id : stx -> identifier
(define (check-id stx)
  (unless (identifier? stx)
    (raise-syntax-error 'pattern "expected identifier" stx))
  stx)

;; check-string : stx -> stx
(define (check-string stx)
  (unless (string? (syntax-e stx))
    (raise-syntax-error #f "expected string" stx))
  stx)

;; nat/f : any -> boolean
(define (nat/f x)
  (or (not x) (exact-nonnegative-integer? x)))

;; check-nat/f : stx -> stx
(define (check-nat/f stx)
  (let ([d (syntax-e stx)])
    (unless (nat/f d)
      (raise-syntax-error #f "expected exact nonnegative integer or #f" stx))
    stx))

;; check-idlist : stx -> (listof identifier)
(define (check-idlist stx)
  (unless (and (stx-list? stx) (andmap identifier? (stx->list stx)))
    (raise-syntax-error #f "expected list of identifiers" stx))
  (stx->list stx))
