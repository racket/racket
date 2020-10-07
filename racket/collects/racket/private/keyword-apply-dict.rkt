#lang racket/base

(provide keyword-apply/dict)

(require "dict.rkt")

;; Proc [Dictof Kw Any] Any ... [Listof Any] -> Any
(define keyword-apply/dict
  (let ()
    ;; keys : [Dictof Kw Any] -> [Listof Kw]
    ;; Produces the sorted list of keys
    (define (keys kws)
      (unless (dict? kws)
        (raise-argument-error 'keyword-apply/dict "dict" kws))
      (define ks (dict-keys kws))
      (unless (andmap keyword? ks)
        (raise-argument-error 'keyword-apply/dict
                              "dict with keyword keys"
                              kws))
      (sort ks keyword<?))

    ;; vals : [Dictof Kw Any] [Listof Kw] -> Any
    ;; Produces the list of vals in the same order as ks
    (define (vals kws ks)
      (for/list ([k (in-list ks)]) (dict-ref kws k)))

    ;; check-dup : [Listof Kw] [Listof Kw] -> Void
    (define (check-dup ks1 ks2)
      (for ([k1 (in-list ks1)] #:when (memq k1 ks2))
        (raise-mismatch-error
         'keyword-apply/dict
         "keyword duplicated in dict and direct keyword arguments: "
         k1)))

    ;; Proc [Dictof Kw Any] Any ... [Listof Any] -> Any
    ;; Used when keyword-apply/dict itself isn't used with keyword arguments
    (define keyword-apply/dict
      (case-lambda
        [(f kws args)
         (define ks (keys kws))
         (keyword-apply f ks (vals kws ks) args)]
        [(f kws arg . rst)
         (define ks (keys kws))
         (apply keyword-apply f ks (vals kws ks) arg rst)]))

    ;; [Listof Kw] [Listof Any] Proc [Dictof Kw Any] Any ... [Listof Any] -> Any
    ;; Used when keyword-apply/dict itself is passed keyword arguments
    ;; Direct keywords are in ks1, dict is kws2
    (define kw-proc-keyword-apply/dict
      (case-lambda
        [(ks1 vs1 f kws2 args)
         (define ks2 (keys kws2))
         (check-dup ks1 ks2)
         (keyword-apply keyword-apply ks1 vs1 f ks2 (vals kws2 ks2) args '())]
        [(ks1 vs1 f kws2 arg . rst)
         (define ks2 (keys kws2))
         (check-dup ks1 ks2)
         (keyword-apply keyword-apply ks1 vs1 f ks2 (vals kws2 ks2) arg rst)]))

    (make-keyword-procedure kw-proc-keyword-apply/dict keyword-apply/dict)))

