
#lang scheme/base
(require (for-syntax scheme/base
                     scheme/struct-info)
         syntax/boundmap
         syntax/kerncase
         syntax/stx)

(provide make

         chunk-kw-seq/no-dups
         chunk-kw-seq
         reject-duplicate-chunks
         check-id
         check-nat/f
         check-string
         check-idlist

         head-local-expand-and-categorize-syntaxes
         categorize-expanded-syntaxes
         head-local-expand-syntaxes)

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
             (loop (cdr args+rest)
                   (cons (list* kw-value #'kw (car args+rest)) rchunks))
             (raise-syntax-error #f "too few arguments for keyword" #'kw ctx)))]
      [(kw . more)
       (keyword? (syntax-e #'kw))
       (raise-syntax-error #f "unexpected keyword" #'kw ctx)]
      [_
       (values (reverse rchunks) stx)]))
  (loop stx null))

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

(define (check-string stx)
  (unless (string? (syntax-e stx))
    (raise-syntax-error #f "expected string" stx))
  stx)

;; nat/f : any -> boolean
(define (nat/f x)
  (or (not x) (exact-nonnegative-integer? x)))

(define (check-nat/f stx)
  (let ([d (syntax-e stx)])
    (unless (nat/f d)
      (raise-syntax-error #f "expected exact nonnegative integer or #f" stx))
    stx))

(define (check-idlist stx)
  (unless (and (stx-list? stx) (andmap identifier? (stx->list stx)))
    (raise-syntax-error #f "expected list of identifiers" stx))
  (stx->list stx))


;; head-local-expand-syntaxes : syntax boolean boolean -> stxs ^ 6
;; Setting allow-def-after-expr? allows def/expr interleaving.
(define (head-local-expand-and-categorize-syntaxes x allow-def-after-expr?)
  (define estxs (head-local-expand-syntaxes x allow-def-after-expr?))
  (define-values (defs vdefs sdefs exprs)
    (categorize-expanded-syntaxes estxs))
  (values estxs estxs defs vdefs sdefs exprs))

(define (categorize-expanded-syntaxes estxs0)
  (let loop ([estxs estxs0] [defs null] [vdefs null] [sdefs null] [exprs null])
    (cond [(pair? estxs)
           (let ([ee (car estxs)])
             (syntax-case ee (begin define-values define-syntaxes)
               [(define-values . _)
                (loop (cdr estxs)
                      (cons ee defs)
                      (cons ee vdefs)
                      sdefs
                      exprs)]
               [(define-syntaxes (var ...) rhs)
                (loop (cdr estxs)
                      (cons ee defs)
                      vdefs
                      (cons ee sdefs)
                      exprs)]
               [_
                (loop (cdr estxs)
                      defs
                      vdefs
                      sdefs
                      (cons ee exprs))]))]
          [(null? estxs)
           (values (reverse defs)
                   (reverse vdefs)
                   (reverse sdefs)
                   (reverse exprs))])))

;; head-local-expand-syntaxes : syntax boolean -> (listof syntax)
(define (head-local-expand-syntaxes x allow-def-after-expr?)
  (let ([intdef (syntax-local-make-definition-context)]
        [ctx '(block)])
    (let loop ([x x] [ex null] [expr? #f])
      (cond [(stx-pair? x)
             (let ([ee (local-expand (stx-car x)
                                     ctx
                                     (kernel-form-identifier-list)
                                     intdef)])
               (syntax-case ee (begin define-values define-syntaxes)
                 [(begin e ...)
                  (loop (append (syntax->list #'(e ...)) (stx-cdr x)) ex expr?)]
                 [(begin . _)
                  (raise-syntax-error #f "bad begin form" ee)]
                 [(define-values (var ...) rhs)
                  (andmap identifier? (syntax->list #'(var ...)))
                  (begin
                    (when (and expr? (not allow-def-after-expr?))
                      (raise-syntax-error #f "definition after expression" ee))
                    (syntax-local-bind-syntaxes (syntax->list #'(var ...)) #f intdef)
                    (loop (stx-cdr x) (cons ee ex) expr?))]
                 [(define-values . _)
                  (raise-syntax-error #f "bad define-values form" ee)]
                 [(define-syntaxes (var ...) rhs)
                  (andmap identifier? (syntax->list #'(var ...)))
                  (begin
                    (when (and expr? (not allow-def-after-expr?))
                      (raise-syntax-error #f "definition after expression" ee))
                    (syntax-local-bind-syntaxes (syntax->list #'(var ...))
                                                #'rhs
                                                intdef)
                    (loop (stx-cdr x) (cons ee ex) expr?))]
                 [(define-syntaxes . _)
                  (raise-syntax-error #f "bad define-syntaxes form" ee)]
                 [_
                  (loop (stx-cdr x) (cons ee ex) #t)]))]
            [(stx-null? x)
             (reverse ex)]))))


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
