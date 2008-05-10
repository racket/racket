
#lang scheme/base
(require (for-syntax scheme/base
                     scheme/struct-info)
         syntax/boundmap
         syntax/stx)
(provide make

         chunk-kw-seq/no-dups
         chunk-kw-seq
         reject-duplicate-chunks
         check-id
#|
         monomap?
         monomap-get
         monomap-put!
         monomap-map
         monomap-for-each
         monomap-domain
         monomap-range

         isomap?
         isomap-get
         isomap-reverse-get
         isomap-put!
         isomap-map
         isomap-for-each
         isomap-domain
         isomap-range
         
         make-bound-id-monomap
         make-free-id-monomap
         make-hash-monomap
         (rename-out [-make-isomap make-isomap])
|#
         )

(define-syntax (make stx)
  (syntax-case stx ()
    [(make S expr ...)
     (unless (identifier? #'S)
       (raise-syntax-error #f "not an identifier" stx #'S))
     (let ()
       (define (no-info) (raise-syntax-error #f "not a struct" stx #'S))
       (define info
         (extract-struct-info
          (syntax-local-value #'S no-info)))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(expr ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s)"
                    (syntax-e #'S)
                    num-slots)
            stx)))
       (with-syntax ([constructor constructor])
         #'(constructor expr ...)))]))

(define (chunk-kw-seq/no-dups stx kws #:context [ctx #f])
  (let-values ([(chunks rest) (chunk-kw-seq stx kws #:context ctx)])
    (reject-duplicate-chunks chunks)
    (values chunks rest)))

;; chunk-kw-seq : stx
;;                alist[keyword => (listof (stx -> any))
;;             -> (listof (cons kw (cons stx(kw) (listof any)))) stx
(define (chunk-kw-seq stx kws #:context [ctx #f])
  (define (loop stx rchunks)
    (syntax-case stx ()
      [(kw . more)
       (and (keyword? (syntax-e #'kw)) (assq (syntax-e #'kw) kws))
       (let* ([kw-value (syntax-e #'kw)]
              [arity (cdr (assq kw-value kws))]
              [args+rest (stx-split #'more arity)])
         (if args+rest
             (loop (cdr args+rest) (cons (list* kw-value #'kw (car args+rest)) rchunks))
             (raise-syntax-error #f "too few arguments for keyword" #'kw ctx)))]
      [(kw . more)
       (keyword? (syntax-e #'kw))
       (raise-syntax-error #f "unexpected keyword" #'kw ctx)]
      [_
       (values (reverse rchunks) stx)]))
  (loop stx null))

(define (reject-duplicate-chunks chunks #:context [ctx #f])
  (define (loop chunks)
    (when (pair? chunks)
      (let* ([kw (caar chunks)]
             [dup (assq kw (cdr chunks))])
        (when dup
          (raise-syntax-error #f "duplicate keyword argument" (cadr dup) ctx))
        (loop (cdr chunks)))))
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


#|
;; Mappings

(define dummy (box #f))
(define fdummy (lambda () dummy))
(define (false/p) #f)

;; --

(define-struct monomap (table getter putter mapper foreacher injfail))

(define (monomap-get im key [fail false/p])
  ((monomap-getter im) (monomap-table im) key fail))

(define (monomap-put! im key val)
  (let ([val ((monomap-getter im) (monomap-table im) key fdummy)])
    (unless (eq? val dummy)
      ((monomap-injfail im) key val))
    ((monomap-putter im) (monomap-table im) key val)))

(define (monomap-map im p)
  ((monomap-mapper im) (monomap-table im) p))

(define (monomap-for-each im p)
  ((monomap-foreacher im) (monomap-table im) p)
  (void))

(define (monomap-domain im)
  (monomap-map (lambda (k v) k)))

(define (monomap-range im)
  (monomap-map (lambda (k v) v)))

(define (make-bound-id-monomap fail)
  (make-monomap (make-bound-identifier-mapping)
               bound-identifier-mapping-get
               bound-identifier-mapping-put!
               bound-identifier-mapping-map
               bound-identifier-mapping-for-each
               fail))

(define (make-free-id-monomap fail)
  (make-monomap (make-module-identifier-mapping)
               module-identifier-mapping-get
               module-identifier-mapping-put!
               module-identifier-mapping-map
               module-identifier-mapping-for-each
               fail))

(define (make-hash-monomap fail)
  (make-monomap (make-hash-table)
               hash-table-get
               hash-table-put!
               hash-table-map
               hash-table-for-each
               fail))

(define-struct isomap (forward backward))

(define (isomap-get im k [fail false/p])
  (monomap-get (isomap-forward im) k fail))
(define (isomap-put! im k v)
  (monomap-put! (isomap-forward im) k v)
  (monomap-put! (isomap-backward im) k v))
(define (isomap-map im p)
  (monomap-map (isomap-forward im) p))
(define (isomap-for-each im p)
  (monomap-for-each (isomap-forward im) p))

(define (isomap-reverse-get im k [fail false/p])
  (monomap-get (isomap-backward im) k fail))

(define (isomap-domain im)
  (monomap-domain (isomap-forward im)))
(define (isomap-range im)
  (monomap-domain (isomap-backward im)))

(define (-make-isomap fmake rmake ffail rfail)
  (make-isomap (fmake ffail)
               (rmake rfail)))
|#
