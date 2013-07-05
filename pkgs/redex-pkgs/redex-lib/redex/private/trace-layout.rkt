#lang racket/base

(require racket/match
         racket/list)

(provide trace->coords
         trace->widths/coords)
         

(define-struct tree (loc children) #:transparent)
;; children : (listof tree)

(define-struct u-tree (loc subtrees) #:transparent)
(define (u-tree-children ut)
  (car (u-tree-subtrees ut)))
(define (add-child ut child)
  (match ut
    [(u-tree loc (list st sts ...))
     (u-tree loc (cons (cons child st) sts))]))
;; subtrees : (listof (listof u-tree))
;; first subtree is the "active" subtree

(define (insert-loc loc input-tree)
  (let loop ([lc loc]
             [tr input-tree])
    (match* (lc tr)
      [(`(,l) (tree loc children))
       (when (for/or ([c children]) (= (tree-loc c) l))
         (error 'insert-loc "tried to replace a node: ~s" l))
       (tree loc (cons (tree l '()) children))]
      [(`(,l ,ls ...) (tree loc children))
       (tree loc
             (for/list ([t (tree-children tr)])
               (if (= l (tree-loc t))
                   (loop ls t)
                   t)))])))

(define (build-tree full-trace)
  (let loop ([trace full-trace]
             [t #f])
    (match trace
      ['() t]
      [`(() ,locs ...)
       (loop locs (tree '() '()))]
      [`(,loc ,locs ...)
       (loop locs (insert-loc loc t))])))

(define (append/replace-child a-u-tree l)
  (match a-u-tree
    [(u-tree ut-loc ut-sts)
     (define-values (this-c other-cs)
       (partition (λ (c) (equal? (u-tree-loc c) l))
                  (car ut-sts)))
     (match this-c
       ['()
        (add-child a-u-tree (u-tree l '(())))]
       [(list (u-tree c-l c-sts))
        (u-tree ut-loc (cons
                        (cons (u-tree c-l (cons '() c-sts)) other-cs)
                        (cdr ut-sts)))]
       [else
        (error 'append/replace-child "identical twins not allowed at: ~s" a-u-tree)])]))


(define (insert-loc/u loc input-tree)
  (let loop ([lc loc]
             [tr input-tree])
    (match* (lc tr)
      [(`(,l) a-tree)
       (append/replace-child a-tree l)]
      [(`(,l ,ls ...) (u-tree loc (list (list children ...) sts ...)))
       (u-tree loc
               (cons
                (for/list ([t children])
                  (if (= l (u-tree-loc t))
                      (loop ls t)
                      t))
                sts))])))

(define (build-tree/u full-trace)
  (let loop ([trace full-trace]
             [t (u-tree '() '())])
    (match trace
      ['() t]
      [`(() ,locs ...)
       (loop locs (u-tree '() (cons '() (u-tree-subtrees t))))]
      [`(,loc ,locs ...)
       (loop locs (insert-loc/u loc t))])))

(define NODE-WIDTH 200)

(define (trace->max-depth trace)
  (apply max (map (λ (loc) (add1 (length loc))) trace)))

(define (collect-width-info a-u-tree max-depth)
  (define locs->max-width (make-hash))
  (define locs->most-children (make-hash))
  (let loop ([t a-u-tree]
             [d 0]
             [full-loc '()])
    (match t
      [(u-tree loc sts)
       (define my-loc (if (equal? loc '()) '() (cons loc full-loc)))
       (define mc (map (λ (c) (cons (u-tree-loc c) my-loc))
                       (car (sort sts > #:key length))))
       (when (>= (length mc)
                 (length (hash-ref locs->most-children my-loc '())))
         (hash-set! locs->most-children
                    my-loc
                    mc))
       (define width (apply max 
                            (map (λ (st)
                                   (if (empty? st) 
                                       (* NODE-WIDTH (add1 (- max-depth d)))
                                       (apply + (for/list ([c st])
                                                  (loop c (add1 d) my-loc)))))
                                 sts)))
       (when (>= width (hash-ref locs->max-width my-loc 0))
         (hash-set! locs->max-width my-loc width))
       width]))
  (values locs->max-width
          locs->most-children))

(define (make-x-coords ls->mws ls->mcs)
  (define locs->coords (make-hash))
  (hash-set! locs->coords '() 0)
  (let loop1 ([cur-loc '()]
              [cur-x 0])
    (hash-set! locs->coords (reverse cur-loc) cur-x)
    (define cs (sort (hash-ref ls->mcs cur-loc)
                     <
                     #:key car))
    (define this-width (apply + (map (λ (c)
                                       (hash-ref ls->mws c))
                                     cs)))
    (define left-x (- cur-x (/ this-width 2)))
    (for ([c-loc (in-list cs)])
      (define c-w (hash-ref ls->mws c-loc))
      (loop1 c-loc (+ left-x (/ c-w 2)))
      (set! left-x (+ left-x c-w))))
  locs->coords)

(define (trace->widths/coords tr)
  (define tree (build-tree/u tr))
  (define-values (ls->mws ls->mcs)
    (collect-width-info tree (trace->max-depth tr)))
  (values ls->mws (make-x-coords ls->mws ls->mcs)))

(define (trace->coords tr)
  (define-values (_ cs)
    (trace->widths/coords tr))
  cs)


(module+ 
 test
 
 (require rackunit)
 
 (check-equal? (insert-loc '(1) (tree '() '()))
               (tree '() (list (tree 1 '()))))
 
 (check-equal? (insert-loc '(0) (insert-loc '(1) (tree '() '())))
               (tree '() (list (tree 0 '()) (tree 1 '()))))
 
 (check-equal? (insert-loc '(0 0) (insert-loc '(0) (insert-loc '(1) (tree '() '()))))
               (tree '() (list (tree 0 (list (tree 0 '()))) (tree 1 '()))))
 
 (check-equal? (build-tree '(() (0) (1)))
               (tree '() (list (tree 1 '()) (tree 0 '()))))
 
 (check-equal? (build-tree '(() (0) (1) (0 0) (0 1)))
               (tree '() (list (tree 1 '()) (tree 0 (list (tree 1 '()) (tree 0 '()))))))
 (check-equal? (build-tree '(() (3) (2) (1) (0)
                                (3 0) (3 1) (3 2)))
               (tree
                '()
                (list
                 (tree 0 '())
                 (tree 1 '())
                 (tree 2 '())
                 (tree 3 (list (tree 2 '()) (tree 1 '()) (tree 0 '()))))))
 (check-equal? (build-tree/u '(() (0)))
               (u-tree '() 
                       (list 
                        (list 
                         (u-tree 0 '(()))))))
 (check-equal? (build-tree/u '(() (0) (1)))
               (u-tree '() 
                       (list 
                        (list 
                         (u-tree 1 '(())) 
                         (u-tree 0 '(()))))))
 (check-equal? (build-tree/u '(() (0) (0)))
               (u-tree '() 
                       (list
                        (list 
                         (u-tree 0 '(() ()))))))
 (check-equal? (build-tree/u '(() ()))
               (u-tree '() '(() ())))
 (check-equal? (build-tree/u '(() (0) (0) (0) () (0) (0 0) (1) (1 0) (1 0)))
               (u-tree
                '()
                (list
                 (list
                  (u-tree 1 (list (list (u-tree 0 '(() ())))))
                  (u-tree 0 (list (list (u-tree 0 '(()))))))
                 (list (u-tree 0 '(() () ()))))))
 (check-equal? (trace->coords '(() (0) (1)))
               (make-hash (list (cons '() 0)
                                (cons '(0) (- NODE-WIDTH))
                                (cons '(1) NODE-WIDTH))))
 (check-equal? (trace->coords '(() (0) (1) (0 0) (0 1) (0 2)))
               (make-hash (list (cons '() 0)
                                (cons '(0) (- (* NODE-WIDTH (/ 3 2))))
                                (cons '(0 0) (- (* NODE-WIDTH (/ 7 2))))
                                (cons '(0 1) (- (* NODE-WIDTH (/ 3 2))))
                                (cons '(0 2) (/ NODE-WIDTH 2))
                                (cons '(1) (* NODE-WIDTH 3)))))
 (check-equal? (trace->coords '(() (0) (1) (0 0) (0 1) (0) (0 0) (0 1) (0 2)))
               (make-hash (list (cons '() 0)
                                (cons '(0) (- (* NODE-WIDTH (/ 3 2))))
                                (cons '(0 0) (- (* NODE-WIDTH (/ 7 2))))
                                (cons '(0 1) (- (* NODE-WIDTH (/ 3 2))))
                                (cons '(0 2) (/ NODE-WIDTH 2))
                                (cons '(1) (* NODE-WIDTH 3))))))
