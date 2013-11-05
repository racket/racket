#lang racket/base
(require racket/list
         racket/math
         "main.rkt")

(provide fade-pict
         slide-pict
         fade-around-pict
         sequence-animations
         reverse-animations
         fast-start
         fast-end
         fast-edges
         fast-middle
         split-phase)

(define (fail-gracefully t)
  (with-handlers ([exn:fail? (lambda (x) (values 0 0))])
    (t)))

(define single-pict (lambda (p) (if (list? p) (last p) p)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation combinators

;; "Morph" from one pict to another. Use `combine' to align
;; the picts relative to another. Only the bounding box is
;; actually morphed; the drawing part transitions by fading
;; the original `a' out and the new `b' in. The `n' argument
;; ranges from 0.0 (= `a') to 1.0 (= `b').
(define (fade-pict #:combine [combine cc-superimpose] n a b)
  ;; Combine ghosts of scaled pictures:
  (let ([orig (combine (cellophane a (- 1.0 n))
                       (cellophane b n))])
    (cond
     [(zero? n) (refocus orig a)]
     [(= n 1.0) (refocus orig b)]
     [else
      (let-values ([(atx aty) (ltl-find orig a)]
                   [(abx aby) (rbl-find orig a)]
                   [(btx bty) (ltl-find orig b)]
                   [(bbx bby) (rbl-find orig b)])
        (let ([da (+ aty (* (- bty aty) n))]
              [dd (- (pict-height orig)
                     (+ aby (* (- bby aby) n)))]
              [orig 
               ;; Generate intermediate last-pict
               (let ([ap (or (pict-last a) a)]
                     [bp (or (pict-last b) b)])
                 (let-values ([(al at) (lt-find orig (if (pair? ap) (cons a ap) (list a ap)))]
                              [(bl bt) (lt-find orig (if (pair? bp) (cons b bp) (list b bp)))]
                              [(ae) (single-pict ap)]
                              [(be) (single-pict bp)])
                   (let ([ar (+ al (pict-width ae))]
                         [ab (+ at (pict-height ae))]
                         [br (+ bl (pict-width be))]
                         [bb (+ bt (pict-height be))])
                     (let ([atl (+ at (pict-ascent ae))]
                           [abl (- ab (pict-descent ae))]
                           [btl (+ bt (pict-ascent be))]
                           [bbl (- bb (pict-descent be))]
                           [btw (lambda (a b)
                                  (+ a (* (- b a) n)))])
                       (let ([t (btw at bt)]
                             [l (btw al bl)])
                         (let ([b (max t (btw ab bb))]
                               [r (max l (btw ar br))])
                           (let ([tl (max t (min (btw atl btl) b))]
                                 [bl (max t (min (btw abl bbl) b))])
                             (let ([p (blank (- r l) (- b t)
                                             (- tl t) (- b bl))])
                               (let ([orig+p (pin-over orig l t p)])
                                 (use-last orig+p p))))))))))])
          (let ([p (make-pict (pict-draw orig)
                              (pict-width orig)
                              (pict-height orig)
                              da
                              dd
                              (list (make-child orig 0 0 1 1 0 0))
                              #f
                              (pict-last orig))])
            (let ([left (+ atx (* (- btx atx) n))]
                  [right (+ abx (* (- bbx abx) n))])
              (let ([hp (inset p
                               (- left)
                               0
                               (- right (pict-width p))
                               0)])
                (let-values ([(atx aty) (lt-find hp a)]
                             [(abx aby) (lb-find hp a)]
                             [(btx bty) (lt-find hp b)]
                             [(bbx bby) (lb-find hp b)])
                  (let ([top (+ aty (* (- bty aty) n))]
                        [bottom (+ aby (* (- bby aby) n))])
                    (inset hp
                           0
                           (- top)
                           0
                           (- bottom (pict-height hp))))))))))])))

;; Pin `p' into `base', sliding from `p-from' to `p-to'
;;  (which are picts within `base') as `n' goes from 0.0 to 1.0.
;; The `p-from' and `p-to' picts are typically ghosts of
;;   `p' within `base', but they can be any picts within
;;   `base'. The top-left locations of `p-from' and `p-to'
;;   determine the placement of the top-left of `p'.
(define (slide-pict base p p-from p-to n)
  (let-values ([(x1 y1) (fail-gracefully (lambda () (lt-find base p-from)))]
               [(x2 y2) (fail-gracefully (lambda () (lt-find base p-to)))])
    (pin-over base
              (+ x1 (* (- x2 x1) n))
              (+ y1 (* (- y2 y1) n))
              p)))

(define (fade-around-pict n base evolved)
  (define tg1 (launder (ghost base)))
  (define tg2 (launder (ghost base)))
  (slide-pict
   (fade-pict n
              tg1
              (evolved tg2))
   base
   tg1
   tg2
   n))

;; Concatenate a sequence of animations
(define (sequence-animations . l)
  (let ([len (length l)])
    (lambda (n)
      (cond
       [(zero? n)
        ((car l) 0.0)]
       [(= n 1.0)
        ((list-ref l (sub1 len)) n)]
       [else
        (let ([pos (inexact->exact (floor (* n len)))])
          ((list-ref l pos) (* len (- n (* pos (/ len))))))]))))

;; Reverse a sequence of animations
(define (reverse-animations . l)
  (let ([s (apply sequence-animations l)])
    (lambda (n)
      (s (- 1 n)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [0,1] -> [0,1] functions

(define (fast-start n)
  (- 1 (* (- 1 n) (- 1 n))))

(define (fast-end n)
  (* n n))

(define (fast-edges n)
  (+ 0.5 (* (sin (- (* n pi) (/ pi 2))) 0.5)))

(define (fast-middle n)
  (- 0.5 (/ (cos (* n pi)) 2)))

(define (split-phase opt-n)
  (values (* 2 (min opt-n 0.5))
          (* 2 (- (max opt-n 0.5) 0.5))))

