#lang racket/base
(require racket/fixnum
         (only-in '#%kernel
                  vector*-set/copy vector*-extend))

;; This intmap implementation is really a functional growable vector,
;; because it's tailored to a case where we have keys near 0, where we
;; don't need to distinguish between #f-valued keys and removed keys,
;; and where it's ok for the intmap size to stay large after it is
;; made large (like a growable stack). To deal with negative keys, two
;; growable vectors are paired: one for negative keys and one for
;; non-negative keys.

(provide empty-intmap
         intmap-ref
         intmap-set
         intmap-remove)

;; Treelist-like representation: first vector index holds a number
;; to used as a right shift, effectively indicating the tree depth.
;; Indices 1 through `WIDTH` inclusive hold elements (in the case of 0)
;; or subtree (in the case of a non-0 multiple of `WIDTH-BITS`).
(define empty-treelist #(0))
(define WIDTH-BITS 4)
(define WIDTH (fxlshift 1 WIDTH-BITS))

(define (treelist-ref v i)
  (define shift (vector*-ref v 0))
  (if (fx= 0 shift)
      (vector*-ref v (fx+ i 1))
      (treelist-ref (vector*-ref v (fx+ (fxrshift i shift) 1))
                    (fxand i (fx- (fxlshift 1 shift) 1)))))

;; returns #f if the tree needs to be made deeper
(define (treelist-maybe-set v i val)
  (define shift (vector-ref v 0))
  (define len (vector*-length v))
  (cond
    [(fx= 0 shift)
     (define idx (fx+ i 1))
     (cond
       [(fx< idx len)
        (vector*-set/copy v idx val)]
       [(fx<= idx WIDTH)
        (if (fx= idx len)
            (vector*-extend v (fx+ idx 1) val)
            (vector*-set/copy (vector*-extend v (fx+ idx 1) #f) idx val))]
       [else #f])]
    [else
     (define idx (fx+ (fxrshift i shift) 1))
     (cond
       [(fx< idx len)
        ;; result cannot be #f, because `i` is in range for `v`
        (define new-sub (treelist-maybe-set (vector*-ref v idx)
                                            (fxand i (fx- (fxlshift 1 shift) 1))
                                            val))
        (vector*-set/copy v idx new-sub)]
       [(fx<= idx WIDTH)
        (treelist-maybe-set (vector*-extend v (fx+ idx 1) (vector (fx- shift WIDTH-BITS)))
                            i
                            val)]
       [else #f])]))

(define (treelist-set v i val)
  (or (treelist-maybe-set v i val)
      ;; deepen tree and try again:
      (let ([v (vector (fx+ (vector-ref v 0) WIDTH-BITS) v)])
        (treelist-set v i val))))

;; ----------------------------------------

;; An intmap pairs two "treelists"

(define empty-intmap (cons empty-treelist empty-treelist))

(define (intmap-ref im i)
  (if (fx< i 0)
      (treelist-ref (car im) (fx- -1 i))
      (treelist-ref (cdr im) i)))

(define (intmap-set im i v)
  (if (fx< i 0)
      (cons (treelist-set (car im) (fx- -1 i) v) (cdr im))
      (cons (car im) (treelist-set (cdr im) i v))))

(define (intmap-remove im i)
  (intmap-set im i #f))

;; ----------------------------------------

(module+ main
  (define im
    (for/fold ([im empty-intmap]) ([i (in-range 300)])
      (intmap-set (intmap-set im i i) (- -1 i) i)))
  (for ([i (in-range 300)])
    (unless (equal? (intmap-ref im i) i)
      (error "no" i))
    (unless (equal? (intmap-ref im (- -1 i)) i)
      (error "no" i)))
  (define evens-im
    (for/fold ([im im]) ([i (in-range 300)])
      (if (even? i)
          (intmap-remove im i)
          (intmap-remove im (- -1 i)))))
  (for ([i (in-range 300)])
    (unless (if (even? i)
                (equal? (intmap-ref im (- -1 i)) i)
                (equal? (intmap-ref im i) i))
      (error "no" i)))

  (define sparse-im
    (for/fold ([im empty-intmap]) ([i (in-range 10)])
      (intmap-set im (* i 10) i)))
  (for ([i (in-range 10)])
    (unless (equal? (intmap-ref sparse-im (* 10 i)) i)
      (error "no" i))))
