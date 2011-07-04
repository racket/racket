#lang scheme/base
(require slideshow/base
         slideshow/pict
         scheme/list
         scheme/math)

(provide play play-n
         fade-pict
         slide-pict
         fade-around-pict
         sequence-animations
         reverse-animations
         animate-slide
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
;; Animation player

;; Create a slide sequence where `mid' takes a number from 0.0 to 1.0.
;; The 0.0 slide will wit until you advance, but the remaining ones will
;; time out automatically to create the animation.
(define (play #:title [title #f]
              #:name [name title]
              #:layout [layout 'auto] 
              #:steps [N 10]
              #:delay [secs 0.05]
              #:skip-first? [skip-first? #f]
              mid)
  (unless skip-first?
    (slide #:title (if (procedure? title) (title 0) title) 
           #:name (if (procedure? name) (name 0) name)
           #:layout layout 
           (mid 0)))
  (if condense?
      (skip-slides N)
      (map (lambda (n)
             (slide #:title (if (procedure? title) (title n) title)
                    #:name (if (procedure? name) (name n) name)
                    #:layout layout 
                    #:timeout secs
                    (mid n)))
           (let ([cnt N])
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
(define (play-n #:title [title #f] 
                #:name [name title] 
                #:layout [layout 'auto]
                #:steps [N 10]
                #:delay [secs 0.05]
                #:skip-last? [skip-last? #f]
                #:skip-first? [skip-first? #f]
                mid)
  (let ([n (procedure-arity mid)])
    (let loop ([post (vector->list (make-vector n))]
               [pre null]
               [skip? skip-first?]
               [Ns N])
      (if (null? post)
          (unless skip-last?
            (slide #:title (if (procedure? title) (apply title pre) title)
                   #:name (if (procedure? name) (apply name pre) name)
                   #:layout layout
                   (apply mid pre)))
          (begin
            (play #:title (if (procedure? title)
                              (lambda (n)
                                (apply title (append pre (list n) (cdr post))))
                              title)
                  #:name (if (procedure? name)
                             (lambda (n)
                               (apply name (append pre (list n) (cdr post))))
                             name)
                  #:layout layout 
                  #:steps (if (pair? Ns) (car Ns) Ns)
                  #:delay secs
                  #:skip-first? skip?
                  (lambda (n)
                    (apply mid (append pre (list n) (cdr post)))))
            (loop (cdr post) (cons 1.0 pre) #f (if (pair? Ns) (cdr Ns) Ns)))))))


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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like `slide', supports 'next and 'alts, but produces as a
;; function of N number arguments (for N stages)
(define (animate-slide . content)
  (let ([n (let loop ([content content])
             (cond
               [(null? content) 1]
               [(eq? (car content) 'next)
                (add1 (loop (cdr content)))]
               [(eq? (car content) 'alts)
                (+ (apply + (map (lambda (alt)
                                   (loop alt))
                                 (cadr content)))
                   (sub1 (loop (cddr content))))]
               [else (loop (cdr content))]))])
    (procedure-reduce-arity
     (lambda ns
       (let loop ([content content]
                  [ns (cons 1.0 ns)]
                  [k (lambda (p ns) (or p (blank)))])
         (cond
           [(null? content) (k #f ns)]
           [(eq? 'next (car content))
            (loop (cdr content)
                  (cdr ns)
                  k)]
           [(eq? 'alts (car content))
            (let aloop ([l (cadr content)]
                        [p (blank)]
                        [ns ns])
              (if (null? l)
                  (loop (cddr content)
                        ns 
                        (lambda (p2 ns)
                          (k (if p2 (vc-append gap-size p p2) p) ns)))
                  (loop (car l) 
                        ns
                        (lambda (p2 ns2)
                          (aloop (cdr l)
                                 (cellophane
                                  (if p2
                                      (ct-superimpose
                                       p
                                       p2)
                                      p)
                                  (if (null? (cdr l))
                                      1.0
                                      (- 1.0 (min 1.0 (* 2 (cadr ns2))))))
                                 (if (null? (cdr l))
                                     ns2
                                     (let ([ns (cdr ns2)])
                                       (cons (max 0.0 (* 2 (- (car ns) 0.5)))
                                             (cdr ns)))))))))]
           [else (vc-append
                  gap-size
                  (let ([p (cellophane (car content) (car ns))])
                    (loop (cdr content) ns
                          (lambda (p2 ns)
                            (k (if p2 (vc-append gap-size p p2) p) ns)))))])))
     (sub1 n))))

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

