#lang racket/base

;; Balanced binary tree ---- immutable and parameterized
;; over the less-than function

(provide empty-tree
         (rename-out [is-empty? tree-empty?]
                     [lookup tree-ref]
                     [insert tree-set]
                     [delete tree-remove]
                     [min-key+value tree-min]
                     [max-key+value tree-max]))

(struct node (key val height left right)
        #:transparent)

(define empty-tree #f)

(define (is-empty? t) (not t))

;; ----------------------------------------

(define (tree-height t)
  (cond
    [(not t) 0]
    [else (node-height t)]))

(define (tree-balance t)
  (- (tree-height (node-left t))
     (tree-height (node-right t))))

(define (combine key val left right)
  (node key
        val
        (add1 (max (tree-height left) (tree-height right)))
        left 
        right))

(define (reverse-combine key val right left)
  (combine key val left right))

;; ----------------------------------------

(define (lookup t key <?)
  (cond
   [(not t) #f]
   [(<? key (node-key t))
    (lookup (node-left t) key <?)]
   [(<? (node-key t) key)
    (lookup (node-right t) key <?)]
   [else (node-val t)]))

;; ----------------------------------------

;; insert : tree X Y (X X -> bool) -> tree
(define (insert t key val <?)
  (cond
    [(not t) (combine key val #f #f)]
    [(<? key (node-key t))
     (insert-to t key val <?
                node-left
                node-right
                combine)]
    [(<? (node-key t) key)
     (insert-to t key val <?
                node-right
                node-left
                reverse-combine)]
    [else t]))

;; insert-to : tree X Y (X X -> bool) 
;;             (tree-of-X -> tree-of-X) 
;;             (tree-of-X -> tree-of-X) 
;;             (X tree-of-X tree-of-X -> tree-of-X) 
;;             -> tree
;; Like insert, but inserts to a child, where `node-to'
;; determines the side where the child is added,`node-other'
;; is the other side, and `comb' builds the new tree gven the
;; two new children.
(define (insert-to t new-key new-val <? node-to node-other comb)
  ;; Insert into the `node-to' child:
  (define new-to (insert (node-to t) new-key new-val <?))
  (define new-other (node-other t))
  
  (define new-t (comb (node-key t) (node-val t) new-to new-other))
  (check-rotate new-t node-to node-other comb))

(define (check-rotate new-t node-to node-other comb)
  (define new-to (node-to new-t))
  (define new-other (node-other new-t))
  (define to-height (tree-height new-to))
  (define other-height (tree-height new-other))
  (if ((- to-height other-height) . = . 2)
      (rotate new-t node-to node-other comb)
      new-t))

;; Helper rotate function:
(define (rotate t node-to node-other comb)
  (define to (node-to t))
  (define to-balance (- (tree-height (node-to to))
                        (tree-height (node-other to))))
  (cond
   [(negative? to-balance)
    (double-rotate t node-to node-other comb)]
   [else
    (single-rotate t node-to node-other comb)]))

;; Helper double-rotate function:
(define (double-rotate t node-to node-other comb)
  (define orange (node-to t))
  (define yellow (node-other orange))
  (define A (node-to orange))
  (define B (node-to yellow))
  (define C (node-other yellow))
  (define D (node-other t))
  (single-rotate (comb (node-key t)
                       (node-val t)
                       (comb (node-key yellow)
                             (node-val yellow)
                             (comb (node-key orange)
                                   (node-val orange)
                                   A
                                   B)
                             C)                   
                       D)
                 node-to node-other comb))

;; Helper single-rotate function:
(define (single-rotate t node-to node-other comb)
  (define yellow (node-to t))
  (comb (node-key yellow)
        (node-val yellow)
        (node-to yellow)
        (comb (node-key t)
              (node-val t)
              (node-other yellow)
              (node-other t))))
  
;; ----------------------------------------

(define (delete t key <?)
  (cond
    [(not t) #f]
    [(<? key (node-key t))
     (delete-at t key <? node-left node-right combine reverse-combine)]
    [(<? (node-key t) key)
     (delete-at t key <? node-right node-left reverse-combine combine)]
    [(not (node-left t))
     (node-right t)]
    [(not (node-right t))
     (node-left t)]
    [else
     (define-values (move-key move-val) (max-key+value (node-left t)))
     (define new-left (delete (node-left t) move-key <?))
     (define new-t (combine move-key
                            move-val
                            new-left
                            (node-right t)))
     (define balance (tree-balance new-t))
     (if (balance . = . -2)
         (check-rotate new-t
                       node-right
                       node-left
                       reverse-combine)
         (check-rotate new-t
                       node-left
                       node-right
                       combine))]))

(define (delete-at t key <? node-to node-from combine reverse-combine)
  (define new-to (delete (node-to t) key <?))
  (cond
   [(eq? new-to (node-to t))
    t]
   [else
    (check-rotate (combine (node-key t)
                           (node-val t)
                           new-to
                           (node-from t))
                  node-from node-to reverse-combine)]))

;; ----------------------------------------

(define (min-key+value t)
  (cond
   [(not (node-left t))
    (values (node-key t) (node-val t))]
   [else
    (min-key+value (node-left t))]))

(define (max-key+value t)
  (cond
   [(not (node-right t))
    (values (node-key t) (node-val t))]
   [else
    (max-key+value (node-right t))]))

;; ----------------------------------------

#;
(begin
  (require racket/match
           racket/list
           rackunit)
  
  (define (inorder t accum)
    (match t
      [#f accum]
      [(node k v h l r)
       (inorder l (cons k (inorder r accum)))]))
  
  (define (insert-all l <?)
    (for/fold ([t #f]) ([i (in-list l)])
      (insert t i (number->string i) <?)))
  
  (define (check-ok? l <?)
    (define t (insert-all l <))
    (check-equal? (inorder t null)
                  (sort l <?))
    (check-balance? t))
  
  (define (check-balance? t)
    (cond
     [(not t) #t]
     [else
      (check-balance? (node-left t))
      (check-balance? (node-right t))
      (unless (<= -1 (tree-balance t) 1)
        (error "misbalanced"))]))
  
  (check-ok? '(1 2 3 4 5 6 7 8) <)
  (check-ok? '(-1 -2 -3 -4 -5 -6 -7 -8) <)
  (for ([i 10])
    (check-ok? (shuffle '(1 2 3 4 5 6 7 8 9 10 11 12 13)) <))
  
  (for ([j 10])
    (let ([l (for/list ([i 50]) i)])
      (let loop ([t (insert-all (shuffle l) <)]
                 [l (shuffle l)])
        (cond
         [(null? l) (check-equal? t #f)]
         [else
          (check-equal? (number->string (car l)) (lookup t (car l) <))
          (define new-t (delete t (car l) <))
          (check-equal? #f (lookup new-t (car l) <))
          (check-balance? new-t)
          (loop new-t (cdr l))]))))
  
  "tests passed")
