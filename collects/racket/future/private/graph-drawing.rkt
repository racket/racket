#lang racket
(require rackunit)
(provide (struct-out point) 
         (struct-out node) 
         (struct-out drawable-node) 
         (struct-out graph-layout)
         draw-tree
         drawable-node-center)

(define DEFAULT-WIDTH 10)
(define PADDING 5)
(define-struct/contract point ([x integer?] [y integer?]) #:transparent)
(struct node (data children))
(struct graph-layout (width height nodes) #:transparent)
(struct drawable-node (node x y width depth children children-xextent children-yextent) #:transparent)

(define (int x) 
  (floor (exact->inexact x)))

;;Gets the center point of a node circle.
;;drawable-node-center : node -> point
(define (drawable-node-center dnode) 
  (point (int (+ (drawable-node-x dnode) (/ (drawable-node-width dnode) 2))) 
         (int (+ (drawable-node-y dnode) (/ (drawable-node-width dnode) 2)))))

;                                                                                  
;                                                ;;                            ;;  
;     ;;; ;     ;                                 ;                             ;  
;    ;   ;;     ;                                 ;                             ;  
;    ;    ;   ;;;;;;;     ;;;;    ;; ;;;      ;;; ;     ;;;;     ;;  ;;     ;;; ;  
;    ;          ;        ;    ;    ;;   ;    ;   ;;    ;    ;     ;;;      ;   ;;  
;     ;;;;      ;             ;    ;    ;   ;     ;         ;     ;       ;     ;  
;         ;     ;        ;;;;;;    ;    ;   ;     ;    ;;;;;;     ;       ;     ;  
;         ;     ;       ;     ;    ;    ;   ;     ;   ;     ;     ;       ;     ;  
;    ;    ;     ;       ;     ;    ;    ;   ;     ;   ;     ;     ;       ;     ;  
;    ;;   ;     ;       ;    ;;    ;    ;    ;   ;;   ;    ;;     ;        ;   ;;  
;    ; ;;;       ;;;;    ;;;; ;;  ;;;  ;;;    ;;; ;;   ;;;; ;;   ;;;;;;     ;;; ;; 
;                                                                                  

;;draw-tree/standard : node uint uint uint uint uint -> drawable-node
(define (draw-tree/standard parent x y depth node-width padding)
  (if (empty? (node-children parent)) 
      (drawable-node parent 
                     (+ padding x) 
                     (+ padding y) 
                     node-width 
                     depth
                     '() 
                     (+ padding x node-width) 
                     (+ padding y node-width)) 
      (let ([child-y (+ y node-width)] 
            [children (node-children parent)] 
            [parenty (+ y padding)])
        (if (= 1 (length children)) ;Align parent and child vertically
            (let ([child (draw-tree/standard (first children)
                                            x 
                                            (+ parenty node-width) 
                                            (add1 depth) 
                                            node-width 
                                            padding)]) 
              (drawable-node parent 
                             (drawable-node-x child) 
                             parenty 
                             node-width 
                             depth
                             (list child) 
                             (drawable-node-children-xextent child) 
                             (drawable-node-children-yextent child)))
            (let-values ([(x-extent 
                           y-extent
                           children) 
                          (for/fold ([xacc x] [yacc y] [chn '()]) 
                            ([child (in-list children)])
                            (let ([dchild (draw-tree/standard child 
                                                             xacc 
                                                             (+ parenty node-width) 
                                                             (add1 depth) 
                                                             node-width 
                                                             padding)]) 
                              (values (drawable-node-children-xextent dchild) 
                                      (drawable-node-children-yextent dchild)
                                      (cons dchild chn))))]) 
              (let* ([chn (reverse children)] 
                     [xmin (drawable-node-x (first chn))] 
                     [xmax (drawable-node-x (last chn))]) 
                (drawable-node parent 
                               (+ xmin (/ (- xmax xmin) 2))
                               parenty
                               node-width 
                               depth
                               chn 
                               x-extent 
                               (+ y-extent node-width))))))))

                          ;;       ;                 ;;;    
;   ;;;;;;                    ;                           ;    
;    ;    ;;                  ;                           ;    
;    ;     ;    ;;;;      ;;; ;     ;;;       ;;;;        ;    
;    ;     ;   ;    ;    ;   ;;       ;      ;    ;       ;    
;    ;    ;         ;   ;     ;       ;           ;       ;    
;    ;;;;;     ;;;;;;   ;     ;       ;      ;;;;;;       ;    
;    ;   ;    ;     ;   ;     ;       ;     ;     ;       ;    
;    ;    ;   ;     ;   ;     ;       ;     ;     ;       ;    
;    ;     ;  ;    ;;    ;   ;;       ;     ;    ;;       ;    
;   ;;;    ;;  ;;;; ;;    ;;; ;;   ;;;;;;    ;;;; ;;   ;;;;;;  
;                                                

;(r * cos(deg), r * sin(deg)) = point on circle given angle and radius r.

(struct attributed-node (node type num-leaves depth children))
(define (leaf? anode) 
  (equal? (attributed-node-type anode) 'leaf))

;;build-attr-tree : node uint -> attributed-node
(define (build-attr-tree parent depth) 
  (if (empty? (node-children parent)) 
      (attributed-node parent 'leaf 0 depth '()) 
      (let-values ([(leaves achn) 
                   (for/fold ([l 0] [achildren '()]) ([child (in-list (node-children parent))]) 
                     (let ([anode (build-attr-tree child (add1 depth))]) 
                       (if (leaf? anode) 
                           (values (add1 l) (cons anode achildren)) 
                           (values (+ l (attributed-node-num-leaves anode)) (cons anode achildren)))))]) 
        (attributed-node parent 
                         'interior 
                         leaves 
                         depth 
                         achn))))


;(struct drawable-node (node x y width depth children children-xextent children-yextent) #:transparent)
;;draw-tree/radial : node uint (uint -> uint) uint -> drawable-node
(define (draw-tree/radial root node-width Bv p depth)  
  (let* ([atree (build-attr-tree root 0)] 
         #;[angle-incr (/ Bv (length (attributed-node-children root)))])
    (for/fold ([angle 0] [chn '()]) ([achild (in-list (attributed-node-children atree))]) 
      (let* ([Bu (/ (* (attributed-node-num-leaves achild) Bv) 
                    (attributed-node-num-leaves atree))] 
             [pa (+ angle 
                    (/ Bu 2))] 
             [x (* (p depth) (cos pa))] 
             [y (* (p depth) (sin pa))]) 
        (values (+ angle Bu)
                (cons (drawable-node (attributed-node-node achild) 
                               x 
                               y 
                               node-width 
                               depth 
                               '() 
                               0 
                               0) chn))))))                   
      
;;tree-layout/private : drawable-node uint uint (listof drawable-node) -> (values uint uint (listof drawable-node))
(define (tree-layout/private parent xextent yextent nodes) 
  (if (empty? (drawable-node-children parent)) 
      (values (max (+ (drawable-node-x parent) (drawable-node-width parent)) xextent) 
              (max (+ (drawable-node-y parent) (drawable-node-width parent)) yextent) 
              (cons parent nodes))
      (for/fold ([x xextent] [y yextent] [ns (cons parent nodes)]) ([child (in-list (drawable-node-children parent))]) 
        (tree-layout/private child x y (cons child ns)))))

;;calc-tree-layout : drawable-node uint uint -> graph-layout
(define (calc-tree-layout root node-width padding) 
  (define-values (w h nodes) (tree-layout/private root 0 0 '())) 
  (graph-layout w
                h 
                nodes))

;;draw-tree : node [symbol] [uint] [uint] [uint] -> tree-layout 
(define (draw-tree root 
                   #:style [style 'standard]
                   #:node-width [node-width DEFAULT-WIDTH] 
                   #:padding [padding PADDING] 
                   #:zoom [zoom-level 1])
  (let* ([scaled-node-w (* node-width zoom-level)] 
         [scaled-padding (* padding zoom-level)] 
         [layout 
          (case style 
            [(standard) (calc-tree-layout (draw-tree/standard root 
                                                              0 
                                                              0 
                                                              0
                                                              scaled-node-w 
                                                              scaled-padding) 
                                          scaled-node-w 
                                          scaled-padding)]
            [(radial) (calc-tree-layout (draw-tree/radial root 
                                                          (λ (i) (* i 50))) 
                                        scaled-node-w 
                                        scaled-padding)]
            [(hv) 0] 
            [else 
             (error 'draw-tree "Invalid tree drawing style.")])]) 
    (graph-layout (+ (graph-layout-width layout) scaled-padding) 
                  (+ (graph-layout-height layout) scaled-padding) 
                  (graph-layout-nodes layout))))


;Tests
(let* ([nodea (drawable-node (node 'a '()) 5 5 10 0 0 '() 10)]
       [center (drawable-node-center nodea)]) 
  (check-equal? (point-x center) 10.0) 
  (check-equal? (point-y center) 10.0))


(define test-padding 5)
(define test-width 10)

(define (tree root-data . children) 
  (node root-data children))

(define (get-node data layout) 
  (first (filter (λ (dn) (equal? (node-data (drawable-node-node dn)) data)) (graph-layout-nodes layout))))

#|
   a
   |
   b
|#
(define tree0 (tree 'a (tree 'b)))
(let* ([layout (draw-tree tree0 #:node-width test-width #:padding test-padding)]
       [dnode-a (get-node 'a layout)]
       [dnode-b (get-node 'b layout)])
  (check-equal? (graph-layout-width layout) (+ (* test-padding 2) test-width))
  (check-equal? (graph-layout-height layout) (+ (* test-padding 3) (* test-width 2)))
  (check-equal? (drawable-node-x dnode-a) test-padding) 
  (check-equal? (drawable-node-y dnode-a) test-padding) 
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ test-padding test-width test-padding)))
(let ([atree (build-attr-tree tree0 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 1))

#|
      a
     / \
    b   c
|# 
(define tree1 (tree 'a 
                    (tree 'b) 
                    (tree 'c)))
(define layout (draw-tree tree1 #:node-width test-width #:padding test-padding)) 
(for ([dnode (in-list (graph-layout-nodes layout))]) 
  (check-equal? (drawable-node-width dnode) test-width))
(define dnode-a (get-node 'a layout)) 
(define dnode-b (get-node 'b layout)) 
(define dnode-c (get-node 'c layout))

(define slot-one-pos (+ test-padding test-width test-padding))
(define square-sz (+ (* test-padding 3) (* test-width 2)))
(check-equal? (graph-layout-width layout) square-sz) 
(check-equal? (graph-layout-height layout) square-sz)
(check-equal? (drawable-node-x dnode-b) test-padding) 
(check-equal? (drawable-node-y dnode-b) slot-one-pos)
(check-equal? (drawable-node-x dnode-c) slot-one-pos)
(check-equal? (drawable-node-y dnode-c) slot-one-pos) 
(check-equal? (drawable-node-x dnode-a) (/ 25 2)) 
(check-equal? (drawable-node-y dnode-a) test-padding) 
(check-equal? (length (drawable-node-children dnode-a)) 2)
(let ([atree (build-attr-tree tree1 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 2))

#|
         a
       /  \
      b    d
      |   / \
      c   e f
            |
            g
|#
(define tree2 (tree 'a 
                    (tree 'b 
                          (tree 'c)) 
                    (tree 'd 
                          (tree 'e) 
                          (tree 'f 
                                (tree 'g)))))
(let* ([layout (draw-tree tree2 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)])
  (check-equal? (node-data (drawable-node-node dnode-a)) 'a) 
  (check-equal? (node-data (drawable-node-node dnode-b)) 'b) 
  (check-equal? (node-data (drawable-node-node dnode-c)) 'c) 
  (check-equal? (node-data (drawable-node-node dnode-d)) 'd) 
  (check-equal? (node-data (drawable-node-node dnode-e)) 'e) 
  (check-equal? (node-data (drawable-node-node dnode-f)) 'f) 
  (check-equal? (node-data (drawable-node-node dnode-g)) 'g) 
  (check-equal? (graph-layout-width layout) 50) 
  (check-equal? (graph-layout-height layout) 65) 
  (check-equal? (drawable-node-x dnode-a) (/ 65 4))
  (check-equal? (drawable-node-y dnode-a) test-padding)
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-x dnode-c) test-padding) 
  (check-equal? (drawable-node-y dnode-c) (+ (drawable-node-y dnode-b) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-e) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-y dnode-e) (+ (drawable-node-y dnode-d) test-width test-padding))
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding))
  (check-equal? (drawable-node-y dnode-f) (drawable-node-y dnode-e))
  (check-equal? (drawable-node-x dnode-g) (drawable-node-x dnode-f))
  (check-equal? (drawable-node-y dnode-g) (+ (drawable-node-y dnode-f) test-width test-padding)))
(let ([atree (build-attr-tree tree2 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 3))

#|
        a
       /|\
      b c e
        |
        d
|#
(define tree3 (tree 'a 
                    (tree 'b) 
                    (tree 'c 
                          (tree 'd)) 
                    (tree 'e)))
(let* ([layout (draw-tree tree3 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)]) 
  (check-equal? (graph-layout-width layout) 50) 
  (check-equal? (graph-layout-height layout) 50) 
  (check-equal? (drawable-node-x dnode-a) 20) 
  (check-equal? (drawable-node-y dnode-a) 5)
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-x dnode-c) (+ (* 2 test-padding) test-width)) 
  (check-equal? (drawable-node-y dnode-c) (drawable-node-y dnode-b)) 
  (check-equal? (drawable-node-x dnode-e) (+ (* 3 test-padding) (* 2 test-width))) 
  (check-equal? (drawable-node-y dnode-e) (drawable-node-y dnode-c)) 
  (check-equal? (drawable-node-x dnode-d) (drawable-node-x dnode-c)) 
  (check-equal? (drawable-node-y dnode-d) (+ (drawable-node-y dnode-c) test-padding test-width)))
(let ([atree (build-attr-tree tree3 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 3))

#| 
        a
     / | | \
    b  c f  g 
      / \
     d   e
|#
(define tree4 (tree 'a 
                    (tree 'b) 
                    (tree 'c 
                          (tree 'd) 
                          (tree 'e)) 
                    (tree 'f) 
                    (tree 'g)))
(let* ([layout (draw-tree tree4 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)]) 
  (check-equal? (graph-layout-width layout) 80) 
  (check-equal? (graph-layout-height layout) 50) 
  (check-equal? (drawable-node-x dnode-b) test-padding) 
  (check-equal? (drawable-node-y dnode-b) (+ (drawable-node-y dnode-a) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-c) (drawable-node-y dnode-b)) 
  (check-equal? (drawable-node-x dnode-d) (+ (drawable-node-x dnode-b) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-d) (+ (drawable-node-y dnode-c) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-e) (+ (drawable-node-x dnode-d) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-e) (drawable-node-y dnode-d)) 
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding)) 
  (check-equal? (drawable-node-y dnode-f) (drawable-node-y dnode-c)) 
  (check-equal? (drawable-node-x dnode-g) (+ (drawable-node-x dnode-f) test-width test-padding)))
(let ([atree (build-attr-tree tree4 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 5))

#| 
Layered-tree-draw example from Di Battista 
         a
        /   \
       b      g
       |     / \
       c    h   k
       |   / \
       d  i   j
      / \
     e   f 
|#
(define tree5 (tree 'a 
                    (tree 'b 
                          (tree 'c 
                                (tree 'd 
                                      (tree 'e) 
                                      (tree 'f)))) 
                    (tree 'g 
                          (tree 'h 
                                (tree 'i) 
                                (tree 'j)) 
                          (tree 'k))))
(let* ([layout (draw-tree tree5 #:node-width test-width #:padding test-padding)] 
       [nodes (graph-layout-nodes layout)] 
       [dnode-a (get-node 'a layout)] 
       [dnode-b (get-node 'b layout)] 
       [dnode-c (get-node 'c layout)] 
       [dnode-d (get-node 'd layout)] 
       [dnode-e (get-node 'e layout)] 
       [dnode-f (get-node 'f layout)] 
       [dnode-g (get-node 'g layout)] 
       [dnode-h (get-node 'h layout)] 
       [dnode-i (get-node 'i layout)] 
       [dnode-j (get-node 'j layout)] 
       [dnode-k (get-node 'k layout)]) 
  (check-equal? (graph-layout-width layout) 80) 
  (check-equal? (graph-layout-height layout) 80) 
  (check-equal? (drawable-node-x dnode-e) test-padding) 
  (check-equal? (drawable-node-y dnode-e) 65) 
  (check-equal? (drawable-node-x dnode-f) (+ (drawable-node-x dnode-e) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-i) (+ (drawable-node-x dnode-f) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-j) (+ (drawable-node-x dnode-i) test-width test-padding)) 
  (check-equal? (drawable-node-x dnode-k) (+ (drawable-node-x dnode-j) test-width test-padding)))
(let ([atree (build-attr-tree tree5 0)]) 
  (check-equal? (attributed-node-num-leaves atree) 5))
       

                                
  

      
   
  
  
  
  
  