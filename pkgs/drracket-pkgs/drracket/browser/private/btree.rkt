#lang racket/unit

(require "sig.rkt")

;; Implements a red-black tree with relative indexing along right
;; splines. This allows the usual O(log(n)) operations, plus a
;; O(log(n)) shift operation.

;; (This is the same data structure as used for lines by GRacket's text%
;; class, but that one is implemented in C++.)
(import)
(export (rename relative-btree^
                (create-btree make-btree)))

(struct btree (root) #:mutable)

(struct node (pos data parent left right color) #:mutable)

(define (adjust-offsets n new-child)
  (when new-child
    (set-node-pos! new-child (- (node-pos new-child)
                                (node-pos n)))))

(define (deadjust-offsets n old-child)
  (when old-child
    (set-node-pos! old-child (+ (node-pos old-child)
                                (node-pos n)))))

(define (rotate-left n btree)
  (let ([old-right (node-right n)])
    (deadjust-offsets n old-right)
    
    (let ([r (node-left old-right)])
      (set-node-right! n r)
      (when r
        (set-node-parent! r n)))
    
    (let ([p (node-parent n)])
      (set-node-parent! old-right p)
      (cond
       [(not p) (set-btree-root! btree old-right)]
       [(eq? n (node-left p)) (set-node-left! p old-right)]
       [else                 (set-node-right! p old-right)]))
    
    (set-node-left! old-right n)
    (set-node-parent! n old-right)))

(define (rotate-right n btree)
  (let ([old-left (node-left n)])
    (adjust-offsets old-left n)
    
    (let ([l (node-right old-left)])
      (set-node-left! n l)
      (when l
        (set-node-parent! l n)))
    
    (let ([p (node-parent n)])
      (set-node-parent! old-left p)
      (cond
       [(not p) (set-btree-root! btree old-left)]
       [(eq? n (node-left p)) (set-node-left! p old-left)]
       [else                 (set-node-right! p old-left)]))
    
    (set-node-right! old-left n)
    (set-node-parent! n old-left)))


(define (insert before? n btree pos data)
  (let ([new (node pos data #f #f #f 'black)])
    (if (not (btree-root btree))
        (set-btree-root! btree new)
        
        (begin
          
          (set-node-color! new 'red)
          
                                        ; Insert into tree
          (if before?
              
              (if (not (node-left n))
                  (begin
                    (set-node-left! n new)
                    (set-node-parent! new n))
                  
                  (let loop ([node (node-left n)])
                    (if (node-right node)
                        (loop (node-right node))
                        (begin
                          (set-node-right! node new)
                          (set-node-parent! new node)))))
              
              (if (not (node-right n))
                  (begin
                    (set-node-right! n new)
                    (set-node-parent! new n))
                  
                  (let loop ([node (node-right n)])
                    (if (node-left node)
                        (loop (node-left node))
                        (begin
                          (set-node-left! node new)
                          (set-node-parent! new node))))))
          
                                        ; Make value in new node relative to right-hand parents
          (let loop ([node new])
            (let ([p (node-parent node)])
              (when p
                (when (eq? node (node-right p))
                  (adjust-offsets p new))
                (loop p))))
          
                                        ; Balance tree
          (let loop ([node new])
            (let ([p (node-parent node)])
              (when (and (not (eq? node (btree-root btree)))
                         (eq? 'red (node-color p)))
                (let* ([recolor-k
                        (lambda (y)
                          (set-node-color! p 'black)
                          (set-node-color! y 'black)
                          (let ([pp (node-parent p)])
                            (set-node-color! pp 'red)
                            (loop pp)))]
                       [rotate-k
                        (lambda (rotate node)
                          (let ([p (node-parent node)])
                            (set-node-color! p 'black)
                            (let ([pp (node-parent p)])
                              (set-node-color! pp 'red)
                              (rotate pp btree)
                              (loop pp))))]
                       [k
                        (lambda (node-y long-rotate always-rotate)
                          (let ([y (node-y (node-parent p))])
                            (if (and y (eq? 'red (node-color y)))
                                (recolor-k y)
                                (let ([k (lambda (node)
                                           (rotate-k always-rotate node))])
                                  (if (eq? node (node-y p))
                                      (begin
                                        (long-rotate p btree)
                                        (k p))
                                      (k node))))))])
                  (if (eq? p (node-left (node-parent p)))
                      (k node-right rotate-left rotate-right)
                      (k node-left rotate-right rotate-left))))))
          
          (set-node-color! (btree-root btree) 'black)))))

(define (find-following-node btree pos)
  (let ([root (btree-root btree)])
    (let loop ([n root]
               [so-far root]
               [so-far-pos (and root (node-pos root))]
               [v 0])
      (if (not n) 
          (values so-far so-far-pos)
          (let ([npos (+ (node-pos n) v)])
            (cond
             [(<= pos npos)
              (loop (node-left n) n npos v)]
             [(or (not so-far-pos)
                  (> npos so-far-pos))
              (loop (node-right n) n npos npos)]
             [else
              (loop (node-right n) so-far so-far-pos npos)]))))))

(define (create-btree)
  (btree #f))

(define (btree-get btree pos)
  (let-values ([(n npos) (find-following-node btree pos)])
    (and n
         (= npos pos)
         (node-data n))))

(define (btree-put! btree pos data)
  (let-values ([(n npos) (find-following-node btree pos)])
    (if (and n (= npos pos))
        (set-node-data! n data)
        (insert (and n (< pos npos))
                n btree pos data))))

(define (btree-shift! btree start delta)
  (let loop ([n (btree-root btree)]
             [v 0])
    (when n
      (let ([npos (node-pos n)])
        (cond
         [(< start (+ v npos))
          (set-node-pos! n (+ npos delta))
          (loop (node-left n) v)]
         [else
          (loop (node-right n) (+ v npos))])))))

(define (btree-for-each btree f)
  (when (btree-root btree)
    (let loop ([n (btree-root btree)]
               [v 0])
      (when (node-left n)
        (loop (node-left n) v))
      (f (+ v (node-pos n)) (node-data n))
      (when (node-right n)
        (loop (node-right n)
              (+ v (node-pos n)))))))

(define (btree-map btree f)
  (reverse
   (let loop ([n (btree-root btree)]
              [v 0]
              [a null])
     (if (not n)
         a
         (let* ([pre (loop (node-left n) v a)]
                [here (cons (f (+ v (node-pos n))
                               (node-data n))
                            pre)])
           (loop (node-right n)
                 (+ v (node-pos n))
                 here))))))
