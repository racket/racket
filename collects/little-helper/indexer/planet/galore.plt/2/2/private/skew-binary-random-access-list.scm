;;; skew-binary-random-access-list.scm  --  Jens Axel Søgaard

;;; SKEW BINARY RANDOM-ACCESS LISTS

; Reference: [Oka, p.132-134]
; Hint:      These lists are a good choice if you need
;            both list-like and array-like operations.

(module skew-binary-random-access-list "mzscheme-without-lists.scm"
  (require (only (lib "list.ss") foldl)
           "../signatures/list-signature.scm")
  (provide-random-access-list)
  
  (require (rename mzscheme mz:cons cons )
           (rename mzscheme mz:car  car)
           (rename mzscheme mz:cdr  cdr))
  
  (define empty '())               ; a tree is either leaf or node
  (define-struct skew-binary-tree-list (roots))
  (define-struct leaf (e))
  (define-struct node (e l r))     ; e element, l and r trees
  (define-struct root (w t))       ; w integer, t tree
  
  (define (roots l)
    (if (or (null? l) #;(pair? l))
        l
        (skew-binary-tree-list-roots l)))
  
  (define (insert x rs)
    (let ([rs (roots rs)])
      (make-skew-binary-tree-list
       (if (and (not (null? rs))
                (not (null? (mz:cdr rs))))
           (let* ([r1  (mz:car rs)]
                  [w1  (root-w r1)]
                  [t1  (root-t r1)]
                  
                  [r2  (mz:car (mz:cdr rs))]
                  [w2  (root-w r2)]
                  [t2  (root-t r2)])      
             (if (= w1 w2)
                 (mz:cons (make-root (+ 1 w1 w2) 
                                     (make-node x t1 t2))
                          (cddr rs))
                 (mz:cons (make-root 1 (make-leaf x))
                          rs)))
           (mz:cons (make-root 1 (make-leaf x)) rs)))))
  
  (define cons insert)
  (define insert-first insert)
  
  (define (insert* xs l)
    (foldl insert l xs))
  
  (define (first rs)
    (let ([rs (roots rs)])
      (if (and (= (root-w (mz:car rs)) 1)
               (leaf? (root-t (mz:car rs))))
          (leaf-e (root-t (mz:car rs)))
          (node-e (root-t (mz:car rs))))))
  
  (define car first)
  
  (define (rest rs)
    (let ([rs (roots rs)])
      (if (null? rs) (error 'rest "rest called on empty list"))
      (make-skew-binary-tree-list
       (if (and (= (root-w (mz:car rs)) 1) 
                (leaf? (root-t (mz:car rs))))
           (mz:cdr rs)
           (mz:cons (make-root (quotient (root-w (mz:car rs)) 2)
                               (node-l (root-t (mz:car rs))))
                    (mz:cons (make-root (quotient (root-w (mz:car rs)) 2)
                                        (node-r (root-t (mz:car rs))))
                             (mz:cdr rs)))))))
  (define cdr rest)
  
  (define remove rest)
  (define remove-first rest)
  
  (define (ref rs i)
    (let ([rs (roots rs)])
      (if (null? rs)
          (error "ref: index " i "too large."))
      (let ([w (root-w (mz:car rs))])
        (if (< i w)
            (ref-tree w i (root-t (mz:car rs)))
            (ref (mz:cdr rs) (- i w))))))
  
  (define (ref-tree w i t)
    (cond
      [(and (= w 1) (= i 0) (leaf? t))  (leaf-e t)]
      [(and         (= i 0) (node? t))  (node-e t)]
      [else (if (<= i (quotient w 2))
                (ref-tree (quotient w 2) (sub1 i) (node-l t))
                (ref-tree (quotient w 2) (- (sub1 i) (quotient w 2)) (node-r t)))]))
  
  (define (set rs i y)
    (let ([rs (roots rs)])
      (let ([w (root-w (mz:car rs))])
        (if (< i w)
            (mz:cons (make-root w (set-tree w i (root-t (mz:car rs)) y))
                     (mz:cdr rs))
            (mz:cons (mz:car rs)
                     (set (mz:cdr rs) (- i w) y))))))
  
  (define (set-tree w i t y)
    (cond
      [(and (= w 1) (= i 0) (leaf? t))  (make-leaf y)]
      [(and         (= i 0) (node? t))  (make-node y (node-l t) (node-r t))]
      [else (if (<= i (quotient w 2))
                (make-node (node-e t)
                           (set-tree (quotient w 2) (sub1 i) (node-l t) y)
                           (node-r t))
                (make-node (node-e t)
                           (node-l t)
                           (set-tree (quotient w 2) (- (sub1 i) (quotient w 2)) (node-r t) y)))]))
  
  (define (fold f b l)
    (if (empty? l)
        b
        (fold f (f (first l) b) (rest l))))
  
  ;    (define empty '())
  (define empty? null?)
  
  (define (elements l)
    (fold mz:cons '() l)))
