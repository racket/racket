#lang racket/base

(require racket/contract
         compiler/zo-parse)

(define (prefix-syntax-start pre)
  (length (prefix-toplevels pre)))

(define (prefix-lift-start pre)
  (define syntax-start (prefix-syntax-start pre))
  (define total-stxs (length (prefix-stxs pre)))
  (+ syntax-start total-stxs (if (zero? total-stxs) 0 1)))

(struct nothing ())

(define-syntax-rule (eprintf* . args) (void))

(define (build-form-memo inner-update #:void? [void? #f])
  (define memo (make-hasheq))
  (define (update form . args)
    (eprintf* "Updating on ~a\n" form)
    (define fin
      (cond
        [(hash-ref memo form #f) 
         => (λ (x)
              (eprintf* "Found in memo table\n")
              x)]
        [else
         (eprintf* "Not in memo table\n")
         (let ()
           (define ph (make-placeholder (nothing)))
           (hash-set! memo form ph)
           (define nv (nothing))
           (dynamic-wind void
                         (λ ()
                           (set! nv (apply inner-update form args)))
                         (λ ()
                           (if (nothing? nv)
                               (eprintf* "inner-update returned nothing (or there was an escape) on ~a\n" form)
                               (begin
                                 (placeholder-set! ph nv)
                                 (hash-set! memo form nv)))))
           nv)]))
    (eprintf* "Updating on ~a ---->\n ~a\n" form fin)
    fin)
  (define (first-update form . args)
    (eprintf* "Top level update on ~a\n" form)
    (define final (apply update form args))
    (eprintf* "Top level update on ~a ---->\n ~a\n" form final)
    (define fin (make-reader-graph final))
    (eprintf* "Top level update on ~a ---->\n ~a [after reader-graph]\n" form fin)
    fin)
  (values first-update update))

(define lang-info/c
  (or/c #f (vector/c module-path? symbol? any/c)))


(define (build-compiled-path base name)
  (build-path 
   (cond [(path? base) base]
	 [(eq? base 'relative) 'same]
	 [(eq? base #f) (error 'batch "Impossible")])
   "compiled"
   name))


(provide/contract
 [prefix-syntax-start (prefix? . -> . exact-nonnegative-integer?)]
 [prefix-lift-start (prefix? . -> . exact-nonnegative-integer?)]
 [eprintf ((string?) () #:rest (listof any/c) . ->* . void)]
 [build-form-memo 
  (((unconstrained-domain-> any/c))
   (#:void? boolean?)
   . ->* .
   (values (unconstrained-domain-> any/c)
           (unconstrained-domain-> any/c)))]
 [lang-info/c contract?]
 [build-compiled-path ((or/c path-string? (symbols 'relative) false/c) path-string? . -> . (or/c path-string? (symbols 'same 'up)))])
