#lang racket/base
(require racket/private/place-local
         racket/fixnum
         "../common/set.rkt")

(provide cache-place-init!

         clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!

         resolve+shift-cache-get
         resolve+shift-cache-set!
         
         cache-or-reuse-set
         cache-or-reuse-hash)

;; ----------------------------------------

;; Cache bindings resolutions with a fairly weak
;; cache keyed on a symbol, phase, and scope sets.

(define (make-cache)
  (box (make-weak-box #f)))

(define-place-local cache (make-cache))

(define (resolve-cache-place-init!)
  (set! cache (make-cache)))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define c (weak-box-value (unbox* cache)))
     (when c
       (hash-remove! c sym))
     (set-box*! shifted-cache #f)]
    [()
     (define c (weak-box-value (unbox* cache)))
     (when c
       (hash-clear! c))
     (set-box*! shifted-cache #f)]))

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

;; Cache binding resolutions keyed on an identifier and
;; phase; this is a very small cache that is consulted
;; before the more general one above; it's even cheaper
;; to check, and it avoids re-shifting module bindings
;; when it hits. It can be especially effective when
;; comparing one identifier to a sequence of other
;; identifiers.

(define SHIFTED-CACHE-SIZE 16) ; power of 2

;; Cache box contains #f or a weak box of a vector:
(define-place-local shifted-cache (box #f))
(define-place-local shifted-cache-pos 0)

(struct shifted-entry (s phase binding)
  #:authentic)

(define (shifted-cache-vector)
  (define wb (unbox* shifted-cache))
  (cond
    [(and wb (weak-box-value wb))
     => (lambda (vec) vec)]
    [else
     (define vec (make-vector SHIFTED-CACHE-SIZE #f))
     (set-box*! shifted-cache (make-weak-box vec))
     vec]))

(define (resolve+shift-cache-get s phase)
  (define vec (shifted-cache-vector))
  (for/or ([e (in-vector vec)])
    (and e
         (eq? s (shifted-entry-s e))
         (eqv? phase (shifted-entry-phase e))
         (shifted-entry-binding e))))

(define (resolve+shift-cache-set! s phase b)
  (define vec (shifted-cache-vector))
  (define p shifted-cache-pos)
  (vector*-set! vec p (shifted-entry s phase b))
  (set! shifted-cache-pos (fxand (fx+ 1 p) (fx- SHIFTED-CACHE-SIZE 1))))

;; ----------------------------------------

;; For scope sets and propagation hashes, we don't intern, but we
;; approximate interning by checking against a small set of recently
;; allocated scope sets or propagation hashes. That's good enough to
;; find sharing for a deeply nested sequence of `let`s from a
;; many-argument `or`, for example, where the interleaving of original
;; an macro-introduced syntax prevents the usual
;; child-is-same-as-parent sharing detecting from working well enough.

(define NUM-CACHE-SLOTS 8) ; power of 2

(define (make-cached-sets)
  (make-weak-box (make-vector NUM-CACHE-SLOTS #f)))

(define-place-local cached-sets (make-cached-sets))
(define-place-local cached-sets-pos 0)

(define (make-cached-hashes)
  (make-weak-box (make-vector NUM-CACHE-SLOTS #f)))

(define-place-local cached-hashes (make-cached-hashes))
(define-place-local cached-hashes-pos 0)

(define (sets-place-init!)
  (set! cached-sets (make-cached-sets))
  (set! cached-hashes (make-cached-hashes)))

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
          (vector*-set! vec cached-pos s)
          (set! cached-pos (fxand (fx+ 1 cached-pos) (fx- NUM-CACHE-SLOTS 1)))
          s))))

(define-cache-or-reuse cache-or-reuse-set cached-sets cached-sets-pos set=?)
(define-cache-or-reuse cache-or-reuse-hash cached-hashes cached-hashes-pos equal?)

;; ----------------------------------------

(define (cache-place-init!)
  (resolve-cache-place-init!)
  (sets-place-init!))
