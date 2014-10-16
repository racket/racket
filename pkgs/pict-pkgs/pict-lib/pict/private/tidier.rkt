#lang racket/base
(require "../main.rkt"
         racket/match
         "layout.rkt")

(provide binary-tidier)

#|

Tidier Drawing of Trees
Edward M. Reingold and John S. Tilford
IEEE Transactions on Software Engineering, 
Vol 7, #2, March 1981

|#

(define (binary-tidier t #:x-spacing [given-x-spacing #f] #:y-spacing [given-y-spacing #f])
  (cond
    [t
     (define-values (x-spacing y-spacing) (compute-spacing t given-x-spacing given-y-spacing))
     (unless given-y-spacing (set! y-spacing (* y-spacing 1.5)))
     (define minsep 2)
     (define xc (tidier-x-coordinates t minsep))
     (define x-max (let loop ([xc xc])
                     (match xc
                       [#f 0]
                       [(x-node x left-xc right-xc)
                        (max x (loop left-xc) (loop right-xc))])))
     (define y-max (let loop ([xc xc])
                     (match xc 
                       [#f 0]
                       [(x-node x left-xc right-xc)
                        (+ 1 (max (loop left-xc) (loop right-xc)))])))
     
     (define main (blank (* x-spacing (+ x-max 1))
                         (* y-spacing y-max)))
     (let loop ([t t]
                [xc xc]
                [y 0])
       (match* (t xc)
         [(#f #f) (void)]
         [((tree-layout pict (list left-t right-t))
           (x-node x left-xc right-xc))
          (define node-pict (launder pict))
          (set! main (pin-over main 
                               (* x x-spacing)
                               (* y y-spacing)
                               node-pict))
          (define (add-edge to color width)
            (define colored
              (colorize (launder (pin-line (ghost main)
                                           node-pict cc-find
                                           to cc-find))
                        color))
            (define with-linewidth
              (if (eq? width 'unspecified)
                  colored
                  (linewidth width colored)))
            (set! main (cc-superimpose with-linewidth main)))
          (match left-t
            [#f (void)]
            [(tree-edge left-t left-color left-width)
             (define left-pict (loop left-t left-xc (+ y 1)))
             (add-edge left-pict left-color left-width)])
          (match right-t
            [#f (void)]
            [(tree-edge right-t right-color right-width)
             (define right-pict (loop right-t right-xc (+ y 1)))
             (add-edge right-pict right-color right-width)])
          node-pict]))
     
     main]
    [else (blank)]))

;; x-coordinate-tree : (or/c #f x-node?)

;; x : exact-positive-integer?
;; l : x-coordinate-tree?
;; r : x-coordinate-tree?
(struct x-node (x l r) #:transparent)

(define (tidier-x-coordinates t minsep)
  (cond
    [(not t) #f]
    [else
     (define t-link
       (let loop ([t t])
         (match t
           [(tree-layout pict (list left right))
            (link (and left (loop (tree-edge-child left)))
                  (and right (loop (tree-edge-child right)))
                  #f #f #f #f)])))
     (setup t-link 0 (extreme #f #f #f) (extreme #f #f #f) minsep)
     (petrify t-link 0)
     
     (define smallest
       (let loop ([t-link t-link])
         (match t-link
           [#f #f]
           [(link llink rlink xcoord _ _ _)
            (min2/f xcoord (min2/f (loop llink) (loop rlink)))])))
     
     (let loop ([t-link t-link])
       (match t-link
         [#f #f]
         [(link llink rlink xcoord ycoord offset thread)
          (x-node (- xcoord smallest) (loop llink) (loop rlink))]))]))

(define (min2/f a b) 
  (cond
    [(not a) b]
    [(not b) a]
    [else (min a b)]))

(struct extreme (addr off lev) #:mutable)
(struct link (llink rlink xcoord ycoord offset thread) #:mutable)

(define (setup t level rmost lmost minsep)
  (cond
    [(not t) 
     (set-extreme-lev! lmost -1)
     (set-extreme-lev! rmost -1)]
    [else
     (define lr (extreme #f #f #f))
     (define ll (extreme #f #f #f))
     (define rr (extreme #f #f #f))
     (define rl (extreme #f #f #f))
     (set-link-ycoord! t level)
     (define l (link-llink t))
     (define r (link-rlink t))
     (setup l (+ level 1) lr ll minsep)
     (setup r (+ level 1) rr rl minsep)
     (cond 
       [(and (not l) (not r))
        (set-extreme-addr! rmost t)
        (set-extreme-addr! lmost t)
        (set-extreme-lev! rmost level)
        (set-extreme-lev! lmost level)
        (set-extreme-off! rmost 0)
        (set-extreme-off! lmost 0)
        (set-link-offset! t 0)]
       [else
        (define cursep minsep)
        (define rootsep minsep)
        (define loffsum 0)
        (define roffsum 0)
        
        (let loop ()
          (when (and l r)
            (when (< cursep minsep)
              (set! rootsep (+ rootsep (- minsep cursep)))
              (set! cursep minsep))
            (cond
              [(link-rlink l)
               (set! loffsum (+ loffsum (link-offset l)))
               (set! cursep (- cursep (link-offset l)))
               (set! l (link-rlink l))]
              [else 
               (set! loffsum (- loffsum (link-offset l)))
               (set! cursep (+ cursep (link-offset l)))
               (set! l (link-llink l))])
            (cond
              [(link-llink r)
               (set! roffsum (- roffsum (link-offset r)))
               (set! cursep (- cursep (link-offset r)))
               (set! r (link-llink r))]
              [else
               (set! roffsum (+ roffsum (link-offset r)))
               (set! cursep (+ cursep (link-offset r)))
               (set! r (link-rlink r))])
            (loop)))
        
        (set-link-offset! t (quotient (+ rootsep 1) 2))
        (set! loffsum (- loffsum (link-offset t)))
        (set! roffsum (+ roffsum (link-offset t)))
        
        (cond
          [(or (> (extreme-lev rl) (extreme-lev ll)) (not (link-llink t)))
           (extreme-copy! lmost rl)
           (set-extreme-off! lmost (+ (extreme-off lmost) (link-offset t)))]
          [else
           (extreme-copy! lmost ll)
           (set-extreme-off! lmost (- (extreme-off lmost) (link-offset t)))])
        (cond
          [(or (> (extreme-lev lr) (extreme-lev rr)) (not (link-rlink t)))
           (extreme-copy! rmost lr)
           (set-extreme-off! rmost (- (extreme-off rmost) (link-offset t)))]
          [else
           (extreme-copy! rmost rr)
           (set-extreme-off! rmost (+ (extreme-off rmost) (link-offset t)))])
        
        (cond
          [(and l (not (eq? l (link-llink t))))
           (set-link-thread! (extreme-addr rr) #t)
           (set-link-offset! (extreme-addr rr) 
                             (abs (- (+ (extreme-off rr) (link-offset t)) loffsum)))
           (cond
             [(<= (- loffsum (link-offset t)) (extreme-off rr))
              (set-link-llink! (extreme-addr rr) l)]
             [else
              (set-link-rlink! (extreme-addr rr) l)])]
          [(and r (not (eq? r (link-rlink t))))
           (set-link-thread! (extreme-addr ll) #t)
           (set-link-offset! (extreme-addr ll) 
                             (abs (- (- (extreme-off ll) (link-offset t)) roffsum)))
           (cond
             [(>= (+ roffsum (link-offset t)) (extreme-off ll))
              (set-link-rlink! (extreme-addr ll) r)]
             [else 
              (set-link-llink! (extreme-addr ll) r)])])])]))

(define (extreme-copy! dest src)
  (set-extreme-addr! dest (extreme-addr src))
  (set-extreme-off! dest (extreme-off src))
  (set-extreme-lev! dest (extreme-lev src)))

(define (petrify t xpos)
  (when t
    (set-link-xcoord! t xpos)
    (when (link-thread t)
      (set-link-thread! t #f)
      (set-link-rlink! t #f)
      (set-link-llink! t #f))
    (petrify (link-llink t) (- xpos (link-offset t)))
    (petrify (link-rlink t) (+ xpos (link-offset t)))))  

(module+ test
  (require rackunit)
  (check-equal? (tidier-x-coordinates #f 2)
                #f)
  (check-equal? (tidier-x-coordinates (_tree-layout #f #f) 2)
                (x-node 0 #f #f))
  (check-equal? (tidier-x-coordinates (_tree-layout
                                       (_tree-layout
                                        #f #f)
                                       (_tree-layout
                                        #f #f))
                                      2)
                (x-node 1 (x-node 0 #f #f) (x-node 2 #f #f)))
  
  (check-equal? (tidier-x-coordinates (_tree-layout
                                       #f
                                       (_tree-layout 
                                        (_tree-layout #f #f)
                                        (_tree-layout #f #f)))
                                      2)
                (x-node 0 #f (x-node 1 (x-node 0 #f #f) (x-node 2 #f #f))))
  (check-equal? (tidier-x-coordinates (_tree-layout
                                       (_tree-layout 
                                        (_tree-layout #f #f)
                                        (_tree-layout #f #f))
                                       #f)
                                      2)
                (x-node 2 (x-node 1 (x-node 0 #f #f) (x-node 2 #f #f)) #f))
  
  
  ;; this is building up an example from 
  ;; http://rp-www.cs.usyd.edu.au/~comp5048/Lect2-trees.pdf and from
  ;; http://sydney.edu.au/engineering/it/~shhong/comp5048-lec2.pdf
  ;; for the tidier algorithm
  (define triangle
    (_tree-layout 
     (_tree-layout #f #f)
     (_tree-layout #f #f)))
  
  (define left-subtree
    (_tree-layout (_tree-layout #f triangle) 
                  #f))

  (define right-subtree
    (_tree-layout 
     triangle
     (_tree-layout #f #f)))
  
  (check-equal? (tidier-x-coordinates left-subtree 2)
                (x-node 1 (x-node 0 #f (x-node 1 (x-node 0 #f #f) (x-node 2 #f #f))) #f))
  
  (check-equal? (tidier-x-coordinates (_tree-layout left-subtree right-subtree) 2)
                (x-node 3
                        (x-node 1
                                (x-node 0 
                                        #f
                                        (x-node 1
                                                (x-node 0 #f #f)
                                                (x-node 2 #f #f)))
                                #f)
                        (x-node 5
                                (x-node 4
                                        (x-node 3 #f #f)
                                        (x-node 5 #f #f))
                                (x-node 6 #f #f))))
  
  
  ;; this is a simplification of the tree in figure 2 from the tidier paper
  (define (build-left t) (_tree-layout (_tree-layout #f #f) t))
  (define (build-right t) (_tree-layout t (_tree-layout #f #f)))
  (check-equal? (tidier-x-coordinates
                 (_tree-layout
                  #f
                  (build-left
                   (build-left
                    (build-right
                     (build-right
                      triangle)))))
                 2)
                (x-node
                 0
                 #f 
                 (x-node
                  1 
                  (x-node 0 #f #f)
                  (x-node
                   2 
                   (x-node 1 #f #f) 
                   (x-node
                    3
                    (x-node
                     2 
                     (x-node
                      1 
                      (x-node 0 #f #f) 
                      (x-node 2 #f #f))
                     (x-node 3 #f #f))
                    (x-node 4 #f #f))))))
  
  
  (check-pred pict? (binary-tidier #f))
  (check-pred pict? (binary-tidier (_tree-layout #f #f))))


(module+ main
  (define (full d)
    (cond
      [(zero? d) #f]
      [else (define s (full (- d 1)))
            (_tree-layout s s)]))
  (define triangle (full 1))
  (define (build-left t) (_tree-layout (_tree-layout #f #f) t))
  (define (build-right t) (_tree-layout t (_tree-layout #f #f)))
  (define (n-of n f t) (if (zero? n) t (n-of (- n 1) f (f t))))
  ;; this is the example from the paper
  (binary-tidier
   (_tree-layout
    (n-of 3 build-right (n-of 3 build-left triangle))
    (n-of 3 build-left (n-of 3 build-right triangle)))) 
  (binary-tidier (full 3)))
