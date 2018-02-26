#lang racket/base
(require "../common/set.rkt")

(provide clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!
         
         cache-or-reuse-set
         cache-or-reuse-hash)

(define cache (box (make-weak-box #f)))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define c (weak-box-value (unbox* cache)))
     (when c
       (hash-remove! c sym))]
    [()
     (define c (weak-box-value (unbox* cache)))
     (when c
       (hash-clear! c))]))

(struct entry (scs smss phase binding)
  #:authentic)

(define (resolve-cache-get sym phase scs smss)
  (define c (weak-box-value (unbox* cache)))
  (and c
       (let ([v (hash-ref c sym #f)])
         (and v
              (eqv? phase (entry-phase v))
              (set=? scs (entry-scs v))
              (set=? smss (entry-smss v))
              (entry-binding v)))))

(define (resolve-cache-set! sym phase scs smss b)
  (define wb (unbox* cache))
  (define c (weak-box-value wb))
  (cond
   [(not c)
    (box-cas! cache wb (make-weak-box (make-hasheq)))
    (resolve-cache-set! sym phase scs smss b)]
   [else
    (hash-set! c sym (entry scs smss phase b))]))

;; ----------------------------------------

;; For scope sets and propagation hashes, we don't intern, but we
;; approximate interning by checking against a small set of recently
;; allocated scope sets or propagation hashes. That's good enough to
;; find sharing for a deeply nested sequence of `let`s from a
;; many-argument `or`, for example, where the interleaving of original
;; an macro-introduced syntax prevents the usual
;; child-is-same-as-parent sharing detecting from working well enough.

(define NUM-CACHE-SLOTS 8)

(define cached-sets (make-weak-box (make-vector NUM-CACHE-SLOTS #f)))
(define cached-sets-pos 0)

(define cached-hashes (make-weak-box (make-vector NUM-CACHE-SLOTS #f)))
(define cached-hashes-pos 0)

(define-syntax-rule (define-cache-or-reuse cache-or-reuse cached cached-pos same?)
  (define (cache-or-reuse s)
    (define vec (or (weak-box-value cached)
                    (let ([vec (make-vector NUM-CACHE-SLOTS #f)])
                      (set! cached (make-weak-box vec))
                      vec)))
    (or (for/or ([s2 (in-vector vec)])
          (and s2
               (same? s s2)
               s2))
        (begin
          (vector-set! vec cached-pos s)
          (set! cached-pos (modulo (add1 cached-pos) NUM-CACHE-SLOTS))
          s))))

(define-cache-or-reuse cache-or-reuse-set cached-sets cached-sets-pos set=?)
(define-cache-or-reuse cache-or-reuse-hash cached-hashes cached-hashes-pos equal?)
