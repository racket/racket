#lang scheme/base
(require slideshow/base
         slideshow/pict
         scheme/list)

(provide play play-n
         fade-pict
         slide-pict
         sequence-animations
         reverse-animations)

(define (fail-gracefully t)
  (with-handlers ([exn:fail? (lambda (x) (values 0 0))])
    (t)))

(define single-pict (lambda (p) (if (list? p) (last p) p)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation player

;; Create a slide sequence where `mid' takes a number from 0.0 to 1.0.
;; The 0.0 slide will wit until you advance, but the remaining ones will
;; time out automatically to create the animation.
(define (play #:title [title #f] #:layout [layout 'auto] mid)
  (slide #:title title #:layout layout (mid 0))
  (if condense?
      (skip-slides 10)
      (map (lambda (n)
             (slide #:title title #:layout layout #:timeout 0.05 (mid n)))
           (let ([cnt 10])
             (let loop ([n cnt])
               (if (zero? n)
                   null
                   (cons (/ (- cnt -1 n) 1.0 cnt)
                         (loop (sub1 n)))))))))

;; Create a sequences of N `play' sequences, where `mid' takes
;; N arguments, each a number between 0.0 and 1.0. Initially, all
;; arguments will be 0.0. The first argument goes from 0.0 to 1.0
;; for the first `play' sequence, and then it stays at 1.0 while
;; the second goes from 0.0 to 1.0 for the second sequence, etc.
(define (play-n #:title [title #f] #:layout [layout 'auto]
                mid
                #:skip-last? [skip-last? #f])
  (let ([n (procedure-arity mid)])
    (let loop ([post (vector->list (make-vector n))]
               [pre null])
      (if (null? post)
          (unless skip-last?
            (slide #:title title #:layout layout (apply mid pre)))
          (begin
            (play #:title title
                  #:layout layout 
                  (lambda (n)
                    (apply mid (append pre (list n) (cdr post)))))
            (loop (cdr post) (cons 1.0 pre)))))))


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
                              (list (make-child orig 0 0 1 1))
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
