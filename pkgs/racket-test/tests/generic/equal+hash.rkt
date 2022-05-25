#lang racket/base

;; vectors as method tables
(struct kons (kar kdr)
        #:methods gen:equal+hash
        [(define (equal-proc x y rec)
           (and (rec (kons-kar x) (kons-kar y))
                (rec (kons-kdr x) (kons-kdr y))))
         (define (hash-proc x rec)  12)
         (define (hash2-proc x rec) 13)])

(struct mkons (kar kdr) #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc x y rec)
     (and (rec (mkons-kar x) (mkons-kar y))
          (rec (mkons-kdr x) (mkons-kdr y))))
   (define (hash-proc x rec)  12)
   (define (hash2-proc x rec) 13)])

(module+ test
  (require rackunit)

  (test-case "kons as an immutable pair"
    (check-equal? (kons 1 2) (kons 1 2))
    (check-false (equal? (kons 1 2) 2))
    (check-false (equal? 2 (kons 1 2)))
    (check-false (equal? (kons 1 2) (kons 3 4)))
    (check-equal? (equal-hash-code (kons 1 2))
                  (equal-hash-code (kons 1 2)))
    (check equal-always? (kons 1 2) (kons 1 2))
    (check-false (equal-always? (kons 1 2) 2))
    (check-false (equal-always? 2 (kons 1 2)))
    (check-false (equal-always? (kons 1 2) (kons 3 4)))
    (check-equal? (equal-always-hash-code (kons 1 2))
                  (equal-always-hash-code (kons 1 2))))

  (test-case "mkons as a mutable pair"
    (check-equal? (mkons 1 2) (mkons 1 2))
    (check-false (equal-always? (mkons 1 2) (mkons 1 2)))
    (check-false (equal? (mkons 1 2) 2))
    (check-false (equal? 2 (mkons 1 2)))
    (check-false (equal? (mkons 1 2) (mkons 3 4)))
    (check-false (equal-always? (mkons 1 2) 2))
    (check-false (equal-always? 2 (mkons 1 2)))
    (check-false (equal-always? (mkons 1 2) (mkons 3 4)))
    (check-equal? (equal-hash-code (mkons 1 2))
                  (equal-hash-code (mkons 1 2)))
    (check-false (equal? (equal-always-hash-code (mkons 1 2))
                         (equal-always-hash-code (mkons 1 2)))))
  )

(define (get gs) ((getset-getter gs)))
(define (set gs new) ((getset-setter gs) new))
(struct getset (getter setter)
  #:methods gen:equal-mode+hash
  [(define (equal-mode-proc self other rec mode)
     (and mode (rec (get self) (get other))))
   (define (hash-mode-proc self rec mode)
     (if mode (rec (get self)) (eq-hash-code self)))])

(module+ test
  (test-case "getset equal-mode"
    (define x 1)
    (define y 2)
    (define gsx (getset (lambda () x) (lambda (new) (set! x new))))
    (define gsy (getset (lambda () y) (lambda (new) (set! y new))))
    (check-false (equal? gsx gsy))
    (check-false (equal-always? gsx gsy))
    (set gsx 3)
    (set gsy 3)
    (check-true (equal? gsx gsy))
    (check-false (equal-always? gsx gsy))
    (check-true (equal-always? gsx gsx))))

(struct mcell-honest-mutable (value) #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc self other rec)
     (rec (mcell-honest-mutable-value self)
          (mcell-honest-mutable-value other)))
   (define (hash-proc self rec)
     (+ (eq-hash-code struct:mcell-honest-mutable)
        (rec (mcell-honest-mutable-value self))))
   (define (hash2-proc self rec)
     (+ (eq-hash-code struct:mcell-honest-mutable)
        (rec (mcell-honest-mutable-value self))))])

(struct mcell-honest-mode (value) #:mutable
  #:methods gen:equal-mode+hash
  [(define (equal-mode-proc self other rec mode)
     (and mode
          (rec (mcell-honest-mode-value self)
               (mcell-honest-mode-value other))))
   (define (hash-mode-proc self rec mode)
     (if mode
         (+ (eq-hash-code struct:mcell-honest-mode)
            (rec (mcell-honest-mode-value self)))
         (eq-hash-code self)))])

#|
;; dishonestly implement one layer deep of equal-now explicitly,
;; while not declaring it mutable:
(struct bxwrp-dishonest-deeprec (box)
  #:methods gen:equal+hash
  [(define (equal-proc self other =?)
     (=? (unbox (bxwrp-dishonest-deeprec-box self))
         (unbox (bxwrp-dishonest-deeprec-box other))))
   (define (hash-proc self rec)
     (rec (unbox (bxwrp-dishonest-deeprec-box self))))
   (define (hash2-proc self rec)
     (rec (unbox (bxwrp-dishonest-deeprec-box self))))])

;; dishonestly delegate to equal-now instead of the recur procedure,
;; while not declaring it mutable:
(struct bxwrp-dishonest-equal (box)
  #:methods gen:equal+hash
  [(define (equal-proc self other =?)
     (equal? (bxwrp-dishonest-equal-box self)
             (bxwrp-dishonest-equal-box other)))
   (define (hash-proc self rec)
     (equal-hash-code (bxwrp-dishonest-equal-box self)))
   (define (hash2-proc self rec)
     (equal-hash-code (bxwrp-dishonest-equal-box self)))])
|#

(struct bxwrp-honest-mutable (box) #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc self other =?)
     (=? (unbox (bxwrp-honest-mutable-box self))
         (unbox (bxwrp-honest-mutable-box other))))
   (define (hash-proc self rec)
     (rec (unbox (bxwrp-honest-mutable-box self))))
   (define (hash2-proc self rec)
     (rec (unbox (bxwrp-honest-mutable-box self))))])
(define (set-bxwrp-honest-mutable-value! b v)
  (set-box! (bxwrp-honest-mutable-box b) v))

(struct bxwrp-honest-shallowrec (box)
  #:methods gen:equal+hash
  [(define (equal-proc self other =?)
     (=? (bxwrp-honest-shallowrec-box self)
         (bxwrp-honest-shallowrec-box other)))
   (define (hash-proc self rec)
     (rec (bxwrp-honest-shallowrec-box self)))
   (define (hash2-proc self rec)
     (rec (bxwrp-honest-shallowrec-box self)))])
(define (set-bxwrp-honest-shallowrec-value! b v)
  (set-box! (bxwrp-honest-shallowrec-box b) v))

(struct bxwrp-shalloweq (box)
  #:methods gen:equal+hash
  [(define (equal-proc self other =?)
     (eq? (bxwrp-shalloweq-box self)
          (bxwrp-shalloweq-box other)))
   (define (hash-proc self rec)
     (eq-hash-code (bxwrp-shalloweq-box self)))
   (define (hash2-proc self rec)
     (eq-hash-code (bxwrp-shalloweq-box self)))])
(define (set-bxwrp-shalloweq-value! b v)
  (set-box! (bxwrp-shalloweq-box b) v))

(struct bxwrp-honest-mode (box)
  #:methods gen:equal-mode+hash
  [(define (equal-mode-proc self other =? mode)
     (and mode
          (=? (unbox (bxwrp-honest-mode-box self))
              (unbox (bxwrp-honest-mode-box other)))))
   (define (hash-mode-proc self rec mode)
     (if mode (rec (unbox (bxwrp-honest-mode-box self))) (eq-hash-code self)))])
(define (set-bxwrp-honest-mode-value! b v)
  (set-box! (bxwrp-honest-mode-box b) v))

(module+ test
  (define-check (check-stable f x set-x! y)
    (define before (f x))
    (set-x! x y)
    (check-equal? (f x) before))

  (test-case "box equal+hash"
    (check-equal? (box 1) (box 1))
    (check-equal? (equal-hash-code (box 2))
                  (equal-hash-code (box 2)))
    (check-equal? (equal-secondary-hash-code (box 3))
                  (equal-secondary-hash-code (box 3)))
    (check-false (equal-always? (box 1) (box 1)))
    (check-false (chaperone-of? (box 1) (box 1)))
    (check-stable equal-always-hash-code (box 2) set-box! 7)
    (check-stable equal-always-secondary-hash-code (box 3) set-box! 13))

  (test-case "mcell-honest-mutable equal+hash"
    (check-equal? (mcell-honest-mutable 1)
                  (mcell-honest-mutable 1))
    (check-equal? (equal-hash-code (mcell-honest-mutable 2))
                  (equal-hash-code (mcell-honest-mutable 2)))
    (check-equal? (equal-secondary-hash-code
                   (mcell-honest-mutable 3))
                  (equal-secondary-hash-code
                   (mcell-honest-mutable 3)))
    (check-false (equal-always? (mcell-honest-mutable 1)
                                (mcell-honest-mutable 1)))
    (check-false (chaperone-of? (mcell-honest-mutable 1)
                                (mcell-honest-mutable 1)))
    (check-stable equal-always-hash-code
                  (mcell-honest-mutable 2)
                  set-mcell-honest-mutable-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (mcell-honest-mutable 3)
                  set-mcell-honest-mutable-value!
                  13))

  (test-case "mcell-honest-mode equal-mode+hash"
    (check-equal? (mcell-honest-mode 1)
                  (mcell-honest-mode 1))
    (check-equal? (equal-hash-code (mcell-honest-mode 2))
                  (equal-hash-code (mcell-honest-mode 2)))
    (check-equal? (equal-secondary-hash-code
                   (mcell-honest-mode 3))
                  (equal-secondary-hash-code
                   (mcell-honest-mode 3)))
    (check-false (equal-always? (mcell-honest-mode 1)
                                (mcell-honest-mode 1)))
    (check-false (chaperone-of? (mcell-honest-mode 1)
                                (mcell-honest-mode 1)))
    (check-stable equal-always-hash-code
                  (mcell-honest-mode 2)
                  set-mcell-honest-mode-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (mcell-honest-mode 3)
                  set-mcell-honest-mode-value!
                  13))

  (test-case "bxwrp-honest-mutable equal+hash"
    (check-equal? (bxwrp-honest-mutable (box 1))
                  (bxwrp-honest-mutable (box 1)))
    (check-equal? (equal-hash-code (bxwrp-honest-mutable (box 2)))
                  (equal-hash-code (bxwrp-honest-mutable (box 2))))
    (check-equal? (equal-secondary-hash-code
                   (bxwrp-honest-mutable (box 3)))
                  (equal-secondary-hash-code
                   (bxwrp-honest-mutable (box 3))))
    (check-false (equal-always? (bxwrp-honest-mutable (box 1))
                                (bxwrp-honest-mutable (box 1))))
    (check-false (chaperone-of? (bxwrp-honest-mutable (box 1))
                                (bxwrp-honest-mutable (box 1))))
    (check-stable equal-always-hash-code
                  (bxwrp-honest-mutable (box 2))
                  set-bxwrp-honest-mutable-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (bxwrp-honest-mutable (box 3))
                  set-bxwrp-honest-mutable-value!
                  13)
    (let* ([b (box 4)]
           [bw (bxwrp-honest-mutable b)])
      (check-false (equal-always? (bxwrp-honest-mutable b)
                                  (bxwrp-honest-mutable b)))
      (check-false (chaperone-of? (bxwrp-honest-mutable b)
                                  (bxwrp-honest-mutable b)))
      (check equal-always? bw bw)
      (check-equal? (equal-always-hash-code bw)
                    (equal-always-hash-code bw))
      (check-equal? (equal-always-secondary-hash-code bw)
                    (equal-always-secondary-hash-code bw))
      (check chaperone-of? bw bw)))

  (test-case "bxwrp-honest-shallowrec equal+hash"
    (check-equal? (bxwrp-honest-shallowrec (box 1))
                  (bxwrp-honest-shallowrec (box 1)))
    (check-equal? (equal-hash-code (bxwrp-honest-shallowrec (box 2)))
                  (equal-hash-code (bxwrp-honest-shallowrec (box 2))))
    (check-equal? (equal-secondary-hash-code
                   (bxwrp-honest-shallowrec (box 3)))
                  (equal-secondary-hash-code
                   (bxwrp-honest-shallowrec (box 3))))
    (check-false (equal-always? (bxwrp-honest-shallowrec (box 1))
                                (bxwrp-honest-shallowrec (box 1))))
    (check-false (chaperone-of? (bxwrp-honest-shallowrec (box 1))
                                (bxwrp-honest-shallowrec (box 1))))
    (check-stable equal-always-hash-code
                  (bxwrp-honest-shallowrec (box 2))
                  set-bxwrp-honest-shallowrec-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (bxwrp-honest-shallowrec (box 3))
                  set-bxwrp-honest-shallowrec-value!
                  13)
    (let* ([b (box 4)])
      (check equal-always?
             (bxwrp-honest-shallowrec b)
             (bxwrp-honest-shallowrec b))
      (check-equal? (equal-always-hash-code (bxwrp-honest-shallowrec b))
                    (equal-always-hash-code (bxwrp-honest-shallowrec b)))
      (check-equal? (equal-always-secondary-hash-code
                     (bxwrp-honest-shallowrec b))
                    (equal-always-secondary-hash-code
                     (bxwrp-honest-shallowrec b)))
      (check chaperone-of?
             (bxwrp-honest-shallowrec b)
             (bxwrp-honest-shallowrec b))))

  (test-case "bxwrp-shalloweq equal+hash"
    (check-false (equal? (bxwrp-shalloweq (box 1))
                         (bxwrp-shalloweq (box 1))))
    (check-false (equal-always? (bxwrp-shalloweq (box 1))
                                (bxwrp-shalloweq (box 1))))
    (check-false (chaperone-of? (bxwrp-shalloweq (box 1))
                                (bxwrp-shalloweq (box 1))))
    (check-stable equal-hash-code
                  (bxwrp-shalloweq (box 2))
                  set-bxwrp-shalloweq-value!
                  7)
    (check-stable equal-secondary-hash-code
                  (bxwrp-shalloweq (box 3))
                  set-bxwrp-shalloweq-value!
                  13)
    (check-stable equal-always-hash-code
                  (bxwrp-shalloweq (box 2))
                  set-bxwrp-shalloweq-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (bxwrp-shalloweq (box 3))
                  set-bxwrp-shalloweq-value!
                  13)
    (let* ([b (box 4)])
      (check-equal? (bxwrp-shalloweq b)
                    (bxwrp-shalloweq b))
      (check-equal? (equal-hash-code (bxwrp-shalloweq b))
                    (equal-hash-code (bxwrp-shalloweq b)))
      (check-equal? (equal-secondary-hash-code
                     (bxwrp-shalloweq b))
                    (equal-secondary-hash-code
                     (bxwrp-shalloweq b)))
      (check equal-always?
             (bxwrp-shalloweq b)
             (bxwrp-shalloweq b))
      (check-equal? (equal-always-hash-code (bxwrp-shalloweq b))
                    (equal-always-hash-code (bxwrp-shalloweq b)))
      (check-equal? (equal-always-secondary-hash-code
                     (bxwrp-shalloweq b))
                    (equal-always-secondary-hash-code
                     (bxwrp-shalloweq b)))
      (check chaperone-of?
             (bxwrp-shalloweq b)
             (bxwrp-shalloweq b))))

  (test-case "bxwrp-honest-mode equal+hash"
    (check-equal? (bxwrp-honest-mode (box 1))
                  (bxwrp-honest-mode (box 1)))
    (check-equal? (equal-hash-code (bxwrp-honest-mode (box 2)))
                  (equal-hash-code (bxwrp-honest-mode (box 2))))
    (check-equal? (equal-secondary-hash-code
                   (bxwrp-honest-mode (box 3)))
                  (equal-secondary-hash-code
                   (bxwrp-honest-mode (box 3))))
    (check-false (equal-always? (bxwrp-honest-mode (box 1))
                                (bxwrp-honest-mode (box 1))))
    (check-false (chaperone-of? (bxwrp-honest-mode (box 1))
                                (bxwrp-honest-mode (box 1))))
    (check-stable equal-always-hash-code
                  (bxwrp-honest-mode (box 2))
                  set-bxwrp-honest-mode-value!
                  7)
    (check-stable equal-always-secondary-hash-code
                  (bxwrp-honest-mode (box 3))
                  set-bxwrp-honest-mode-value!
                  13)
    (let* ([b (box 4)]
           [bw (bxwrp-honest-mode b)])
      (check-false (equal-always? (bxwrp-honest-mode b)
                                  (bxwrp-honest-mode b)))
      (check-false (chaperone-of? (bxwrp-honest-mode b)
                                  (bxwrp-honest-mode b)))
      (check equal-always? bw bw)
      (check-equal? (equal-always-hash-code bw)
                    (equal-always-hash-code bw))
      (check-equal? (equal-always-secondary-hash-code bw)
                    (equal-always-secondary-hash-code bw))
      (check chaperone-of? bw bw)))
  )

(define ((function-passthrough f) . args) (apply f args))

(struct overinclusive-arity1 ()
  #:methods gen:equal+hash
  [(define equal-proc (function-passthrough (λ (s o r) #t)))
   (define hash-proc (function-passthrough (λ (s r) 0)))
   (define hash2-proc (function-passthrough (λ (s r) 0)))])

(struct overinclusive-arity2 ()
  #:methods gen:equal-mode+hash
  [(define equal-mode-proc (function-passthrough (λ (s o r m) #t)))
   (define hash-mode-proc (function-passthrough (λ (s r m) 0)))])

(struct overinclusive-arity3 ()
  #:property prop:equal+hash
  (list (function-passthrough (λ (s o r) #t))
        (function-passthrough (λ (s r) 0))
        (function-passthrough (λ (s r) 0))))

(struct overinclusive-arity4 ()
  #:property prop:equal+hash
  (list (function-passthrough (λ (s o r m) #t))
        (function-passthrough (λ (s r m) 0))))

(module+ test
  (test-case "overinclusive arity equal+hash"
    (for ([oia (in-list (list overinclusive-arity1
                              overinclusive-arity2
                              overinclusive-arity3
                              overinclusive-arity4))])
      (check-true (equal? (oia) (oia)))
      (check-true (equal-always? (oia) (oia)))
      (check-equal? (equal-hash-code (oia))
                    (equal-hash-code (oia)))
      (check-equal? (equal-always-hash-code (oia))
                    (equal-always-hash-code (oia))))))
