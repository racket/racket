#lang racket/base

;; Two variants of red-black trees.

;; In the expunge variant, each node stores the count of nodes in its
;; left branch. A nodes' value corresponds to a pre-delete index, and
;; subtracting the right-branch count produces a post-delete index
;; (i.e., as if all deleteions for earlier pre-delete indices happened
;; first).  Inserting into the tree provides a post-delete index, so
;; the insert must use the left-branch count to compare the node's
;; pre-delete index to the given post-delete index. Rotations to
;; maintain the red-black property can locally update the left-branch
;; count stored in the rotated nodes. It's not possible to insert the
;; same pre-delete index twice, since insertion uses the post-delete
;; index.

;; In the fetch variant, each node's value is a list, where the first
;; number in the list is the key. But for a right branch, all keys are
;; decremented by the node's key (recursively). This allows a ln-time
;; shift operation when a message is expunged.

(provide new-tree tree-empty?
         expunge-insert! expunge-tree->list
         fetch-insert! fetch-find fetch-delete! fetch-shift! fetch-tree->list)

(define-struct tree (v red? left-count left right parent)
  #:mutable #:transparent)

(define (new-tree)
  (make-tree 'pre-root #f 0 #f #f #f))

(define (tree-empty? t)
  (not (tree-left t)))

(define (k+ a b)
  (cons (+ (car a) (if (number? b) b (car b)))
        (cdr a)))
(define (k- a b)
  (cons (- (car a) (if (number? b) b (car b)))
        (cdr a)))
(define kv car)

(define (mk-insert sort-to-left? sort=? right+
                   left-insert-adjust!
                   left-rotate-adjust! right-rotate-adjust!)
  (define-values (rotate-left! rotate-right!)
    (let ([mk
           (lambda (tree-west tree-east set-tree-west! set-tree-east! adj-count!)
             (lambda (t)
               (let ([old-east (tree-east t)])
                 (let ([r (tree-west old-east)])
                   (set-tree-east! t r)
                   (when r (set-tree-parent! r t)))
                 (let ([p (tree-parent t)])
                   (set-tree-parent! old-east p)
                   (if (eq? t (tree-left p))
                     (set-tree-left! p old-east)
                     (set-tree-right! p old-east)))
                 (set-tree-west! old-east t)
                 (set-tree-parent! t old-east)
                 (adj-count! t old-east))))])
      (values (mk tree-left tree-right set-tree-left! set-tree-right!
                  left-rotate-adjust!)
              (mk tree-right tree-left set-tree-right! set-tree-left!
                  right-rotate-adjust!))))

  (values
   ;; insert
   (lambda (pre-root n)
     (let ([new
            ;; Insert:
            (let loop ([t (tree-left pre-root)]
                       [n n]
                       [parent pre-root]
                       [set-child! (lambda (t v)
                                     (set-tree-left! pre-root v))])
              (cond
                [(not t) (let ([new (make-tree n #t 0 #f #f parent)])
                           (set-child! parent new)
                           new)]
                [(sort=? n t)
                 (set-tree-v! t n)
                 pre-root]
                [(sort-to-left? n t)
                 (left-insert-adjust! t)
                 (loop (tree-left t) n t set-tree-left!)]
                [else
                 (loop (tree-right t) (right+ n t) t set-tree-right!)]))])
       ;; Restore red-black property:
       (let loop ([v new])
         (let ([p (tree-parent v)])
           (when (and p (tree-red? p))
             (let ([gp (tree-parent p)])
               (let-values ([(tree-west tree-east rotate-west! rotate-east!)
                             (if (eq? p (tree-left gp))
                               (values tree-left tree-right rotate-left! rotate-right!)
                               (values tree-right tree-left rotate-right! rotate-left!))])
                 (let ([uncle (tree-east (tree-parent p))])
                   (if (and uncle (tree-red? uncle))
                     (begin
                       (set-tree-red?! p #f)
                       (set-tree-red?! uncle #f)
                       (set-tree-red?! gp #t)
                       (loop gp))
                     (let ([finish (lambda (v)
                                     (let* ([p (tree-parent v)]
                                            [gp (tree-parent p)])
                                       (set-tree-red?! p #f)
                                       (set-tree-red?! gp #t)
                                       (rotate-east! gp)
                                       (loop gp)))])
                       (if (eq? v (tree-east p))
                         (begin (rotate-west! p) (finish p))
                         (finish v))))))))))
       (set-tree-red?! (tree-left pre-root) #f)))

   ;; delete (fetch only)
   (lambda (pre-root n)
     (let ([orig-t (fetch-find-node pre-root n)])
       (when orig-t
         ;; Delete note t if it has at most one child.
         ;; Otherwise, move a leaf's data to here, and
         ;; delete the leaf.
         (let ([t (if (and (tree-left orig-t)
                           (tree-right orig-t))
                    (let loop ([t (tree-right orig-t)])
                      (if (tree-left t)
                        (loop (tree-left t))
                        t))
                    orig-t)])
           (unless (eq? t orig-t)
             ;; Swap out:
             (let ([delta (kv (tree-v t))])
               (set-tree-v! orig-t (k+ (tree-v t) (tree-v orig-t)))
               (let loop ([c (tree-right orig-t)])
                 (when c
                   (set-tree-v! c (k- (tree-v c) delta))
                   (loop (tree-left c))))))
           ;; Now we can delete t:
           (let ([child-t (or (tree-left t) (tree-right t))]
                 [p (tree-parent t)])
             (when child-t
               (set-tree-parent! child-t p)
               ;; Adjust relative index of left spine of the
               ;;  right branch (in the case that there was only
               ;;  a right branch)
               (let loop ([c (tree-right t)])
                 (when c
                   (set-tree-v! c (k+ (tree-v c) (tree-v t)))
                   (loop (tree-left c)))))
             (if (eq? (tree-left p) t)
               (set-tree-left! p child-t)
               (set-tree-right! p child-t))
             ;; Restore red-black property:
             (when (not (tree-red? t))
               (let loop ([c child-t] [p p])
                 (cond
                   [(and c (tree-red? c)) (set-tree-red?! c #f)]
                   [(tree-parent p)
                    (let-values ([(tree-west tree-east rotate-west! rotate-east!)
                                  (if (eq? c (tree-left p))
                                    (values tree-left tree-right rotate-left! rotate-right!)
                                    (values tree-right tree-left rotate-right! rotate-left!))])
                      (let ([sibling (tree-east p)])
                        (let ([z (if (tree-red? sibling)
                                   (begin
                                     (set-tree-red?! sibling #f)
                                     (set-tree-red?! p #t)
                                     (rotate-west! p)
                                     (tree-east p))
                                   sibling)])
                          (if (not (or (and (tree-west z)
                                            (tree-red? (tree-west z)))
                                       (and (tree-east z)
                                            (tree-red? (tree-east z)))))
                            (begin
                              (set-tree-red?! z #t)
                              (loop p (tree-parent p)))
                            (let ([w (if (not (and (tree-east z)
                                                   (tree-red? (tree-east z))))
                                       (begin
                                         (set-tree-red?! (tree-west z) #f)
                                         (set-tree-red?! z #t)
                                         (rotate-east! z)
                                         (tree-east p))
                                       z)])
                              (set-tree-red?! w (tree-red? p))
                              (set-tree-red?! p #f)
                              (set-tree-red?! (tree-east w) #f)
                              (rotate-west! p))))))]))))))))))

(define-values (expunge-insert! ---)
  (mk-insert
   ;; sort-to-left?
   (lambda (n t)
     ((+ n (tree-left-count t)) . < . (tree-v t)))
   ;; sort=?
   (lambda (n t) #f)
   ;; right+
   (lambda (n t)
     (+ n 1 (tree-left-count t)))
   ;; left-insert-adjust!
   (lambda (t)
     (set-tree-left-count! t (add1 (tree-left-count t))))
   ;; left-rotate-adjust!
   (lambda (t old-right)
     (set-tree-left-count! old-right (+ 1
                                        (tree-left-count old-right)
                                        (tree-left-count t))))
   ;; right-rotate-adjust!
   (lambda (t old-left)
     (set-tree-left-count! t (- (tree-left-count t)
                                (tree-left-count old-left)
                                1)))))

(define-values (fetch-insert! fetch-delete!)
  (mk-insert
   ;; sort-to-left?
   (lambda (n t)
     ((kv n) . < . (kv (tree-v t))))
   ;; sort=?
   (lambda (n t)
     (= (kv n) (kv (tree-v t))))
   ;; right+
   (lambda (n t)
     (k- n (tree-v t)))
   ;; left-insert-adjust!
   void
   ;; left-rotate-adjust!
   (lambda (t old-right)
     (set-tree-v! old-right (k+ (tree-v old-right)
                                (tree-v t))))
   ;; right-rotate-adjust!
   (lambda (t old-left)
     (set-tree-v! t (k- (tree-v t)
                        (tree-v old-left))))))

(define (expunge-tree->list pre-root)
  (let loop ([t (tree-left pre-root)])
    (if t
      (append (loop (tree-left t))
              (list (tree-v t))
              (loop (tree-right t)))
      null)))

(define (fetch-find-node pre-root n)
  (let loop ([t (tree-left pre-root)] [n n])
    (and t
         (cond
           [(= n (kv (tree-v t))) t]
           [(< n (kv (tree-v t))) (loop (tree-left t) n)]
           [else (loop (tree-right t) (- n (kv (tree-v t))))]))))

(define (fetch-find pre-root n)
  (let ([t (fetch-find-node pre-root n)])
    (and t (tree-v t))))

(define (fetch-shift! pre-root n)
  (fetch-delete! pre-root n)
  (let loop ([t (tree-left pre-root)] [n n])
    (when t
      (if (n . < . (kv (tree-v t)))
        (begin (set-tree-v! t (k- (tree-v t) 1))
               (loop (tree-left t) n))
        (loop (tree-right t)
              (- n (kv (tree-v t))))))))

(define (fetch-tree->list pre-root)
  (let loop ([t (tree-left pre-root)][d 0])
    (if t
      (append (loop (tree-left t) d)
              (list (k+ (tree-v t) d))
              (loop (tree-right t) (+ d (kv (tree-v t)))))
      null)))


(module+ test
  (require racket/pretty)
  (print-struct #t)

  (define t (new-tree))

  (define (test n v)
    (expunge-insert! t n)
    (unless (equal? (expunge-tree->list t) v)
      (error 'bad "~s != ~s" (expunge-tree->list t) v)))

  (test 12 '(12))
  (test 8 '(8 12))
  (test 1 '(1 8 12))

  (set! t (new-tree))

  (test 10 '(10))
  (test 8 '(8 10))
  (test 10 '(8 10 12))
  (test 8 '(8 9 10 12))
  (test 8 '(8 9 10 11 12))
  (test 100 '(8 9 10 11 12 105))
  (test 1 '(1 8 9 10 11 12 105))
  (test 105 '(1 8 9 10 11 12 105 112))
  (test 99 '(1 8 9 10 11 12 105 106 112))
  (test 2 '(1 3 8 9 10 11 12 105 106 112))
  (test 6 '(1 3 8 9 10 11 12 13 105 106 112))
  (test 5 '(1 3 7 8 9 10 11 12 13 105 106 112))
  (test 15 '(1 3 7 8 9 10 11 12 13 24 105 106 112))
  (test 15 '(1 3 7 8 9 10 11 12 13 24 25 105 106 112))
  (test 15 '(1 3 7 8 9 10 11 12 13 24 25 26 105 106 112))

  (set! t (new-tree))

  (define (test2 n v)
    (cond
     [(< n 0) (fetch-delete! t (- n))]
     [(inexact? n) (fetch-shift! t (inexact->exact n))]
     [else (fetch-insert! t (list n))])
    ;; (printf "Check ~a\n" v)
    (let ([v (map list v)])
      (unless (equal? (fetch-tree->list t) v)
        (error 'bad "~s != ~s" (fetch-tree->list t) v))))

  (test2 10 '(10))
  (test2 12 '(10 12))
  (test2 8 '(8 10 12))
  (test2 10 '(8 10 12))
  (test2 -10 '(8 12))
  (test2 -10 '(8 12))
  (test2 10.0 '(8 11))
  (test2 100.0 '(8 11))
  (test2 5.0 '(7 10))
  (test2 1 '(1 7 10))
  (test2 2 '(1 2 7 10))
  (test2 3 '(1 2 3 7 10))
  (test2 4 '(1 2 3 4 7 10))
  (test2 5 '(1 2 3 4 5 7 10))
  (test2 6 '(1 2 3 4 5 6 7 10))
  (test2 -6 '(1 2 3 4 5 7 10))
  (test2 -5 '(1 2 3 4 7 10))
  (test2 -4 '(1 2 3 7 10))
  (test2 -3 '(1 2 7 10))
  (test2 -2 '(1 7 10))
  (test2 -7 '(1 10))
  (test2 -1 '(10))
  (test2 -10 '())

  (define (in-all-positions n l)
    (if (null? l)
        (list (list n))
        (cons
         (cons n l)
         (map (lambda (r) (cons (car l) r))
              (in-all-positions n (cdr l))))))

  (define (permutations l)
    (if (or (null? l)
            (null? (cdr l)))
        (list l)
        (apply
         append
         (map (lambda (lol)
                (in-all-positions (car l) lol))
              (permutations (cdr l))))))

  (define perms (permutations '(1 2 3 4 5 6 7 8)))

  (for-each (lambda (l)
              (let ([t (new-tree)])
                (for-each (lambda (i)
                            (fetch-insert! t (list i)))
                          l)
                (unless (equal? (fetch-tree->list t) '((1) (2) (3) (4) (5) (6) (7) (8)))
                  (error 'perms "bad: ~a" l))
                (for-each (lambda (i)
                            (fetch-delete! t i))
                          l)
                (unless (equal? (fetch-tree->list t) '())
                  (error 'perms "remove bad: ~a" l))))
            perms))
