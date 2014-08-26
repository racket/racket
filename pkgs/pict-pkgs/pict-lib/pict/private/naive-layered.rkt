#lang racket/base
(require racket/match
         "../main.rkt"
         "layout.rkt")
(provide naive-layered)
(define (naive-layered t #:x-spacing [given-x-spacing #f] #:y-spacing [given-y-spacing #f])
  (define-values (x-space y-space) (compute-spacing t given-x-spacing given-y-spacing))

  (define root+tree-pair
    (let loop ([t t])
      (match t
        [#f (cons #f (blank))]
        [(tree-layout pict children)
         (cond
           [(andmap not children)
            (define this-root (ghost (launder pict)))
            (cons this-root (cc-superimpose this-root pict))]
           [else
            (define children-pairs
              (for/list ([child (in-list children)])
                (match child
                  [#f 
                   (define b (blank))
                   (cons b b)]
                  [(tree-edge child color _)
                   (loop child)])))
            (define this-root (launder (ghost pict)))
            (define children-roots (map car children-pairs))
            (define children-trees (map cdr children-pairs))
            (let loop ([main (place-parent-over-children
                              (cc-superimpose this-root pict)
                              children-roots
                              (vc-append
                               y-space
                               (ghost (launder pict))
                               (apply ht-append x-space children-trees)))]
                       [children-roots children-roots]
                       [tree-edges children])
              (cond
                [(null? children-roots) (cons this-root main)]
                [else 
                 (define child-root (car children-roots))
                 (define this-tree-edge (car tree-edges))
                 (match this-tree-edge
                   [#f (loop main (cdr children-roots) (cdr tree-edges))]
                   [(tree-edge child edge-color edge-width)
                    (define *w/line
                      (colorize
                       (launder
                        (pin-line (ghost main)
                                  this-root cc-find
                                  child-root cc-find))
                       edge-color))
                    (define w/line
                      (if (eq? edge-width 'unspecified)
                          *w/line
                          (linewidth edge-width *w/line)))
                    (loop (cc-superimpose w/line main)
                          (cdr children-roots)
                          (cdr tree-edges))])]))])])))
  
  (cdr root+tree-pair))

(define (place-parent-over-children parent-root children-roots main)
  (define x-min (pict-width main))
  (define x-max 0)
  (for ([child-root (in-list children-roots)])
    (when child-root
      (define-values (c-min _1) (lc-find main child-root))
      (define-values (c-max _2) (rc-find main child-root))
      (set! x-min (min c-min x-min))
      (set! x-max (max c-max x-max))))
  (pin-over main
            (- (/ (+ x-min x-max) 2) (/ (pict-width parent-root) 2))
            0
            parent-root))
                             

(module+ test 
  (require rackunit)
  (check-pred pict? (naive-layered #f))
  (check-pred pict? (naive-layered (_tree-layout)))
  (check-pred pict? (naive-layered (_tree-layout
                                    (_tree-layout)
                                    (_tree-layout))))
  (check-pred pict? (naive-layered (_tree-layout
                                    (_tree-layout)
                                    (_tree-layout)
                                    (_tree-layout
                                     (_tree-layout)
                                     (_tree-layout)
                                     (_tree-layout
                                      (_tree-layout)
                                      (_tree-layout)))))))

(module+ main
  (define (complete n)
    (cond
      [(= n 0) #f]
      [else
       (define t (complete (- n 1)))
       (apply _tree-layout (build-list n (λ (_) t)))]))
  
  (naive-layered (complete 4))
  (define right-subtree-with-long-left-chain
    (_tree-layout
     (_tree-layout
      (_tree-layout #f #f)
      (_tree-layout
       (_tree-layout #f #f)
       #f))
     (_tree-layout
      (_tree-layout
       (_tree-layout
        (_tree-layout 
         (_tree-layout #f #f)
         #f)
        #f)
       #f)
      #f)))
  (naive-layered right-subtree-with-long-left-chain))
