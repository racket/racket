#lang racket/base
(require "../main.rkt" racket/match)

(provide (struct-out tree-layout)
         (struct-out tree-edge)
         binary-tree-layout?
         _tree-layout?
         _tree-layout
         _tree-edge
         default-node-pict
         compute-spacing)

;; values of this struct leak outside, so it cannot be transparent
(struct tree-layout (pict children))
(struct tree-edge (child edge-color edge-width))

(define _tree-layout
  (let ([constructor tree-layout])
    (define (tree-layout #:pict [node-pict #f]
                         . children)
      (constructor (or node-pict default-node-pict)
                   (for/list ([child (in-list children)])
                     (cond
                       [(tree-edge? child) child]
                       [(not child) child]
                       [else (_tree-edge child)]))))
    tree-layout))

(define _tree-layout?
  (let ([predicate tree-layout?])
    (define (tree-layout? v)
      (or (not v) (predicate v)))
    tree-layout?))

(define default-node-pict
  (cc-superimpose
   (disk 16 #:draw-border? #f)
   (colorize (disk 12 #:draw-border? #f) "white")))

(define _tree-edge
  (let ([constructor tree-edge])
    (define (tree-edge child
                       #:edge-color [edge-color "gray"]
                       #:edge-width [edge-width 'unspecified])
      (constructor child edge-color edge-width))
    tree-edge))

(define (binary-tree-layout? t)
  (match t
    [#f #t]
    [(tree-layout pict (list left right))
     (and (binary-tree-edge? left)
          (binary-tree-edge? right))]
    [else #f]))

(define (binary-tree-edge? e)
  (match e
    [(tree-edge t _ _) (binary-tree-layout? t)]
    [#f #t]))

(define (compute-spacing t given-x-spacing given-y-spacing)
  (cond
    [(and given-x-spacing given-y-spacing)
     (values given-x-spacing given-y-spacing)]
    [else
     (define x-spacing 0)
     (define y-spacing 0)
     
     (let loop ([t t])
       (match t
         [#f (void)]
         [(tree-layout pict (list children ...))
          (set! x-spacing (max (pict-width pict) x-spacing))
          (set! y-spacing (max (pict-height pict) y-spacing))
          (for ([edge (in-list children)])
            (match edge
              [#f (void)]
              [(tree-edge child edge-color _)
               (loop child)]))]))
     
     (values (or given-x-spacing x-spacing)
             (or given-y-spacing y-spacing))]))
