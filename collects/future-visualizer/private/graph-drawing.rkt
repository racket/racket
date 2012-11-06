#lang racket/base

(require racket/list
         racket/contract
         ;; rackunit
         "constants.rkt")

(provide (struct-out point)
         (struct-out node)
         (struct-out drawable-node)
         (struct-out graph-layout)
         (struct-out attributed-node)
         draw-tree
         drawable-node-center
         build-attr-tree)

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
                   #:node-width [node-width CREATE-GRAPH-NODE-DIAMETER] 
                   #:padding [padding CREATE-GRAPH-PADDING] 
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
