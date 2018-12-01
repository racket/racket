#lang racket/base
(require racket/fixnum
         (for-syntax racket/base))

;; Simplified version of Jon Zeppieri's intmap
;; implementation for Racket-on-Chez.
;; This one always has fixnum keys, doens't have
;; to hash, doesn't have to deal with collisions,
;; and doesn't need a wrapper to distinguish
;; the type and record the comparison function.

(provide empty-intmap
         intmap-count
         intmap-ref
         intmap-set
         intmap-remove)

(define empty-intmap #f)

(struct Br (count prefix mask left right) #:transparent)

(struct Lf (key value) #:transparent)

(define (intmap-count t)
  (cond
    [(not t) #f]
    [(Br? t) (Br-count t)]
    [else 1]))

(define (intmap-ref t key)
  (cond
   [(Br? t)
    (if (fx<= key (Br-prefix t))
        (intmap-ref (Br-left t) key)
        (intmap-ref (Br-right t) key))]
   [(Lf? t)
    (if (fx= key (Lf-key t))
        (Lf-value t)
        (not-found key))]
   [else (not-found key)]))

(define (not-found key)
  (error 'intmap-ref "not found: ~e" key))

(define (intmap-set t key val)
  (cond
    [(Br? t)
     (let ([p (Br-prefix t)]
           [m (Br-mask t)])
       (cond
         [(not (match-prefix? key p m))
          (join key (Lf key val) p t)]
         [(fx<= key p)
          (br p m (intmap-set (Br-left t) key val) (Br-right t))]
         [else
          (br p m (Br-left t) (intmap-set (Br-right t) key val))]))]
   [(Lf? t)
    (let ([j (Lf-key t)])
      (cond
        [(not (fx= j key))
         (join key (Lf key val) j t)]
        [else
         (Lf key val)]))]
   [else
    (Lf key val)]))

(define (join p0 t0 p1 t1)
  (let* ([m (branching-bit p0 p1)]
         [p (mask p0 m)])
    (if (fx<= p0 p1)
        (br p m t0 t1)
        (br p m t1 t0))))

(define (intmap-remove t key)
  (cond
   [(Br? t)
    (let ([p (Br-prefix t)]
          [m (Br-mask t)])
      (cond
       [(not (match-prefix? key p m))
        t]
       [(fx<= key p)
        (br/check-left p m (intmap-remove (Br-left t) key) (Br-right t))]
       [else
        (br/check-right p m (Br-left t) (intmap-remove (Br-right t) key))]))]
   [(Lf? t)
    (if (fx= key (Lf-key t))
        #f
        t)]
   [else
    #f]))

;; bit twiddling
(define-syntax-rule (match-prefix? h p m)
  (fx= (mask h m) p))

(define-syntax-rule (mask h m)
  (fxand (fxior h (fx- m 1)) (fxnot m)))

(define-syntax-rule (branching-bit p m)
  (highest-set-bit (fxxor p m)))

(define-syntax (if-64-bit? stx)
  (syntax-case stx ()
    [(_ 64-mode 32-mode)
     (if (eqv? 64 (system-type 'word))
         #'64-mode
         #'32-mode)]))

(define-syntax-rule (highest-set-bit x1)
  (let* ([x2 (fxior x1 (fxrshift x1 1))]
         [x3 (fxior x2 (fxrshift x2 2))]
         [x4 (fxior x3 (fxrshift x3 4))]
         [x5 (fxior x4 (fxrshift x4 8))]
         [x6 (fxior x5 (fxrshift x5 16))]
         [x7 (if-64-bit?
              (fxior x6 (fxrshift x6 3))
              x6)])
    (fxxor x7 (fxrshift x7 1))))

;; basic utils
(define (br p m l r)
  (let ([c (fx+ (intmap-count l) (intmap-count r))])
    (Br c p m l r)))

(define (br/check-left p m l r)
  (if l
      (br p m l r)
      r))

(define (br/check-right p m l r)
  (if r
      (br p m l r)
      l))
