#lang typed/racket/base

(require/typed typed/racket
               [integer-sqrt/remainder (Natural -> (Values Natural Natural))])
(require "types.rkt"
         "number-theory.rkt")

(provide faster-integer-root)

; This is intended to become a more efficient implementation 
; of integer-root than the one "number-theory.rkt".
; They do however only find the root of one exists,
; so they need to call the "slow" one when none exact root exists.

(: is-nth-root : N N N -> (U N False))
;    Return candidate if it's the nth root of a, otherwise #f
(define (is-nth-root a n candidate)
  (if (= (expt candidate n) a)
      candidate
      #f))

(: faster-integer-root : N N -> (U N False))
;   return the nth root of a, if it is a natural number,
;   otherwise return #f
(define (faster-integer-root a n)
  ; factor a = 2^d 3^e b^r  , where gcd(6,b)=1
  (cond
    [(= n 1) a]
    [(= n 2) (let-values ([(s r) (integer-sqrt/remainder a)])
               (if (zero? r)
                   s
                   #f))]
    [else
     (let ([d (max-dividing-power 2 a)])
       (if (not (divides? n d))
           #f ; ? ; (faster-integer-root (* 2 11) 4)
           (let ([e (max-dividing-power 3 a)])
             (if (not (divides? n e))
                 #f
                 (let* ([b-to-r (quotient a (* (expt 2 d) (expt 3 e)))]
                        ; factor n = 2^f c , where gcd(2,c)=1
                        [f         (max-dividing-power 2 n)]
                        [two-to-f  (expt 2 f)]
                        [c         (quotient n two-to-f)])
                   ;
                   (cond
                     [(integer-root/odd-odd b-to-r c) 
                      => (lambda (cth-root--of--b-to-r)
                           (cond
                             [(integer-root/power-of-two cth-root--of--b-to-r two-to-f)
                              => (lambda (nth-root--of--b-to-r)
                                   (* (expt 2 (quotient d n))
                                      (expt 3 (quotient e n))
                                      nth-root--of--b-to-r))]
                             [else #f]))]
                     [else #f]))))))]))

(: integer-root/odd-odd : N N -> (U N False))
(define (integer-root/odd-odd a n)
  ; INPUT   a odd, n odd
  ; OUTPUT  The n'th root of a, if it's an integer, #f otherwise
  (unless (and (odd? a) (odd? n))
    (error "integer-root/odd-odd: Both a and n must be odd; given " a n))
  ; Newton iteration with phi(y)=y^n-a and initial guess g0=1         
  (let ([candidate 
         ; Newton iteration with phi(y)=y^n-a and initial guess g0=1         
         (let* ([k (do: : N ([k : N 1 (add1 k)])
                     [(> (expt 2 (* n k)) a) k])]
                [r (integer-length k)])
           (let: loop : N
             ([i  : N 1] [gi : N 1] [si : N 1] [ti : N 1])
             ; (display `((k ,k) (r ,r) (i ,i) (gi ,gi) (si ,si) (ti ,ti)))   (newline)
             (cond
               [(< i r) (let* ([g_i+1 (modulo (- gi (* (- (* gi ti) a) si)) 
                                              (expt 2 (expt 2 i)))]
                               [t_i+1 (modulo (expt g_i+1 (assert (- n 1) natural?))
                                              (expt 2 (expt 2 (+ i 1))))])
                          (loop (+ i 1)
                                g_i+1
                                (modulo (- (* 2 si) (* n t_i+1 si si))
                                        (expt 2 (expt 2 i)))
                                t_i+1))]
               [else    (modulo (- gi (* (- (* gi ti) a) si))
                                (expt 2 (expt 2 i)))])))])
    (is-nth-root a n candidate)))

#;(define (integer-root/power-of-two  a n)
    ; INPUT   n a power of 2
    ;          gcd(6,a)=1
    ; OUTPUT 
    ;        
    (let ([phi  (lambda (y) (- (expt y n) a))]
          [Dphi (lambda (y) (* n (expt y (- n 1))))])
      (let ([candidate1 (p-adic-newton-iteration phi Dphi 3 11 1 (inverse (Dphi 1) 3))])
        (if (= (expt candidate1 n) a)
            candidate1
            (let ([candidate2 (p-adic-newton-iteration phi Dphi 3 11 2 (inverse (Dphi 2) 3))])
              (is-nth-root a n candidate2))))))

(: integer-root/power-of-two : N N -> (U N False))
(define (integer-root/power-of-two a n)
  ; INPUT    n = 2^d
  ; OUTPUT   an n'th root of a, or #f
  (let: loop : (U N False)
    ([d : Z (- (integer-length n) 1)]
     [b : N a])
    (if (= d 0)
        b
        (let-values ([(s r) (integer-sqrt/remainder b)])
          (if (not (zero? r))
              #f
              (loop (- d 1) s))))))

(: integer-root-factor : N N -> (List N N N N))
(define (integer-root-factor a n)
  ; factor a = 2^d 3^e b^r  , where gcd(6,b)=1
  (let* ([d      (max-dividing-power 2 a)]
         [e      (max-dividing-power 3 a)]
         [b-to-r (quotient a (* (expt 2 d) (expt 3 e)))]
         ; factor n = 2^f c , where gcd(2,c)=1
         [f         (max-dividing-power 2 n)]
         [two-to-f  (expt 2 f)]
         [c         (quotient n two-to-f)]
         [x (integer-root/odd-odd b-to-r c)]
         ;
         [b (if x
                (integer-root/power-of-two x two-to-f)
                (error 'integer-root-factor "internal error - send bug report"))]
         [b1 (if b b (error 'integer-root-factor "internal error - send bug report"))]
         [r (max-dividing-power b1 b-to-r)])
    (list d e b1 r)))

;;; ALGORITHM 9.22  p-adic Newton Iteration  [MCA, p.264]
; INPUT phi in Z[y] (represented a normal function f : Z -> Z)
;       p in Z, l>0,
;       g0 in Z with phi(g)=0 mod p,  phi'(go) invertible mod p
;       and a modular inverse s0 of phi'(g0) mod p
; OUTPUT
;       g in R with phi(g)=0 mod p^l  and  g=g0 mod p

(: p-adic-newton-iteration : (N -> N) (N -> N) N N N N -> N)
(define (p-adic-newton-iteration phi Dphi p l g0 s0)
  (let ([r (integer-length l)])
    (let: loop : N ([i  : N 1]
                    [gi : N g0]
                    [si : N s0])
      (cond
        [(< i r) (let ([g_i+1 (modulo (- gi (* (phi gi) si))
                                      (expt p (expt 2 i)))])
                   (loop (+ i 1)
                         g_i+1
                         (modulo (- (* 2 si) (* (Dphi g_i+1) si si)) 
                                 (expt p (expt 2 i)))))]
        [else    (modulo (- gi (* (phi gi) si)) 
                         (expt p l))]))))

;(= (p-adic-newton-iteration (lambda (y) (- (* y y y y) 1))
;                            (lambda (y) (* 4 (* y y y)))
;                            5
;                            4
;                            2
;                            3)
;   182)

;;; ALGORITHM 19.6  Floyd's cycle detection trick
; [MCA, p. 536]

; Let α = {0,...,p-1} be a finite set.
; A function f : α -> α and x0 in α generates an infinite sequence:
;    x0, x1=f(x0), x2=f(x1), ...
; An infinite sequence in a finite set will repeat.

; Floyd-detect-cycle returns an index i>0 s.t. x_i = x_2i
(: floyd-detect-cycle : ((Z -> Z) Z -> Z))
(define (floyd-detect-cycle f x0)
  (do ([xi x0 (f xi)]
       [yi x0 (f (f yi))]
       [i  0  (add1 i)])
    [(= xi yi) i]))
