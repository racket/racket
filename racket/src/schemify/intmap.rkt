#lang racket/base
(require racket/fixnum
         (for-syntax racket/base))

(provide empty-intmap
         intmap-count ; not constant-time
         intmap-ref
         intmap-set
         intmap-remove)

;; AVL tree where keys are always fixnums

;; ----------------------------------------

(struct node (key val height left right)
  #:transparent
  #:authentic)

;; ----------------------------------------

(define (tree-height t)
  (cond
    [(not t) 0]
    [else (node-height t)]))

(define (combine key val left right)
  (node key
        val
        (fx+ 1 (fxmax (tree-height left) (tree-height right)))
        left 
        right))

(define (reverse-combine key val right left)
  (combine key val left right))

;; ----------------------------------------

(define (insert t key val)
  (cond
    [(not t) (combine key val #f #f)]
    [(fx< key (node-key t))
     (insert-to t key val
                node-left
                node-right
                combine
                rotate-right)]
    [(fx< (node-key t) key)
     (insert-to t key val
                node-right
                node-left
                reverse-combine
                rotate-left)]
    [else
     (node key val
           (node-height t)
           (node-left t)
           (node-right t))]))

;; Like insert, but inserts to a child, where `node-to'
;; determines the side where the child is added,`node-other'
;; is the other side, and `comb' builds the new tree gven the
;; two new children.
(define-syntax-rule (insert-to t new-key new-val node-to node-other comb rotate)
  (begin
    ;; Insert into the `node-to' child:
    (define new-to (insert (node-to t) new-key new-val))
    (define new-other (node-other t))
    
    (define new-t (comb (node-key t) (node-val t) new-to new-other))
    
    ;; Check for rotation:
    (define to-height (tree-height new-to))
    (define other-height (tree-height new-other))
    (if ((fx- to-height other-height) . fx= . 2)
        (rotate new-t)
        new-t)))

(define (delete t key)
  (cond
    [(not t) #f]
    [(fx< key (node-key t))
     (delete-from t key
                  node-left
                  node-right
                  combine
                  rotate-left)]
    [(fx< (node-key t) key)
     (delete-from t key
                  node-right
                  node-left
                  reverse-combine
                  rotate-right)]
    [else
     (define l (node-left t))
     (define r (node-right t))
     (cond
       [(not l) r]
       [(not r) l]
       [else
        (delete-here t node-left node-right combine rotate-left)])]))

(define-syntax-rule (delete-from t key node-to node-other comb rotate)
  (begin
    ;; Delete from the `node-to' child:
    (define new-to (delete (node-to t) key))
    (define new-other (node-other t))
    
    (define new-t (comb (node-key t) (node-val t) new-to new-other))
    
    ;; Check for rotation:
    (define to-height (tree-height new-to))
    (define other-height (tree-height new-other))
    (if ((fx- to-height other-height) . fx= . -2)
        (rotate new-t)
        new-t)))

(define-syntax-rule (delete-here t node-from node-other comb rotate)
  (begin
    ;; Delete by moving from `from` to `other`
    (define from (node-from t))
    (define new-t
      (let loop ([end from])
        (cond
          [(node-other end)
           => (lambda (e) (loop e))]
          [else
           (define key (node-key end))
           (define new-from (delete from key))
           (comb key (node-val end) new-from (node-other t))])))

    ;; Check for rotation:
    (define from-height (tree-height (node-from new-t)))
    (define other-height (tree-height (node-other new-t)))
    (if ((fx- from-height other-height) . fx= . -2)
        (rotate new-t)
        new-t)))

(define-syntax-rule (define-rotate rotate node-to node-other comb)
  (begin
    ;; Helper rotate function:
    (define (rotate t)
      (define to (node-to t))
      (define to-balance (fx- (tree-height (node-to to))
                              (tree-height (node-other to))))
      (cond
        [(to-balance . fx< . 0)
         (double-rotate t)]
        [else
         (single-rotate t)]))

    ;; Helper double-rotate function:
    (define (double-rotate t)
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
                           D)))
    
    ;; Helper single-rotate function:
    (define (single-rotate t)
      (define yellow (node-to t))
      (comb (node-key yellow)
            (node-val yellow)
            (node-to yellow)
            (comb (node-key t)
                  (node-val t)
                  (node-other yellow)
                  (node-other t))))))

(define-rotate rotate-right node-left node-right combine)
(define-rotate rotate-left node-right node-left reverse-combine)

;; ----------------------------------------

(define empty-intmap #f)

(define (intmap-count im)
  (cond
    [(not im) 0]
    [else (fx+ 1
               (intmap-count (node-left im))
               (intmap-count (node-right im)))]))

(define (intmap-ref im key)
  (cond
    [(not im)
     (error 'intmap-ref "not found: ~e" key)]
    [(fx< key (node-key im))
     (intmap-ref (node-left im) key)]
    [(fx< (node-key im) key)
     (intmap-ref (node-right im) key)]
    [else
     (node-val im)]))
    
(define (intmap-set im key val)
  (insert im key val))

(define (intmap-remove im key)
  (delete im key))

;; ----------------------------------------

#;
(module+ main
  (require racket/match
           racket/list
           rackunit)
  
  (define (inorder t accum)
    (match t
      [#f accum]
      [(node k v h l r)
       (inorder l (cons v (inorder r accum)))]))
  
  (define (insert-all l)
    (for/fold ([t #f]) ([i (in-list l)])
      (insert t i (number->string i))))
  
  (define (delete-all t l)
    (let loop ([t t] [l l])
      (cond
        [(null? l) t]
        [else
         (define new-t (delete t (car l)))
         (check-equal? (map string->number (inorder new-t null))
                       (sort (cdr l) <))
         (loop new-t (cdr l))])))
  
  (define (check-ok? l)
    (define t (insert-all l))
    (check-equal? (map string->number (inorder t null))
                  (sort l <))
    (check-equal? #f
                  (delete-all t l)))
  
  
  (check-ok? '(1 2 3 4 5 6 7 8))
  (check-ok? '(-1 -2 -3 -4 -5 -6 -7 -8))
  (check-ok? (for/list ([i (in-range 128)]) i))
  (check-ok? (reverse (for/list ([i (in-range 128)]) i)))
  (for ([i 10])
    (check-ok? (shuffle '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
  "tests passed")
