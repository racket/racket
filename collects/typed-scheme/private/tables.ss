#lang scheme/base

(require mzlib/plt-match)

(provide (all-defined-out))

;; a table is represented by an association list, (cons key value)

;; alist->eq : alist -> table
(define (alist->eq l) l)

;; to-sexp : table -> Listof(List k v)
(define (to-sexp t)
  (map (match-lambda [(cons k v) (list k v)]) t))

;; union/value : table(k,v) table(k,v) (v v -> v) -> table(k,v)
(define (union/value t1 t2 f)
  (define ks1 (map car t1))
  (define ks2 (map car t2))
  ;; everything but the common ones
  (define t1* (filter (match-lambda [(cons k v) (not (memq k ks2))]) t1))
  (define t2* (filter (match-lambda [(cons k v) (not (memq k ks1))]) t2))
  (define pre-result (append t1* t2*))
  ;; the common ones
  (define *t1 (filter (match-lambda [(cons k v) (memq k ks2)]) t1))
  (define *t2 (filter (match-lambda [(cons k v) (memq k ks1)]) t2))
  (define merged (map (match-lambda [(cons k v1)
                                     (let ([v2 (cdr (assq k *t2))])
                                       (cons k (f v1 v2)))])
                      *t1))
  (append pre-result merged))

(define (make-eq) null)

(define (lookup k t) 
  (cond [(assq k t) => cdr]
        [else #f]))

(define (insert k v t)
  (cons (cons k v) t))


