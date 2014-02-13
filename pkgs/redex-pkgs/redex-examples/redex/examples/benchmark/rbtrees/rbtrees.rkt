#lang racket/base

(define the-error "no-error")

(require redex/reduction-semantics
         racket/match)

(define-language rbtrees
  (t E
     (c t n t))
  (c R B)
  (n O (s n)))

(define-relation rbtrees
  [(< O (s n))]
  [(< (s n_1) (s n_2))
   (< n_1 n_2)])

(define-judgment-form rbtrees
  #:mode (ordered? I O O)
  [(ordered? (c E n E) n n)]
  [(ordered? (c E n t_2) n n_2max)
   (ordered? t_2 n_2min n_2max)
   (< n n_2min)]
  [(ordered? (c t_1 n E) n_1min n)
   (ordered? t_1 n_1min n_1max)
   (< n_1max n)]
  [(ordered? (c t_1 n t_2) n_1min n_2max)
   (ordered? t_1 n_1min n_1max)
   (ordered? t_2 n_2min n_2max)
   (< n_1max n)
   (< n n_2min)])

(define-judgment-form rbtrees
  #:mode (rbtree I O)
  [(rbtree E O)]
  [(rbtree (R (B t_l1 n_l t_l2) 
              n 
              (B t_r1 n_r t_r2)) 
           n_bd)
   (rbtree (B t_l1 n_l t_l2) n_bd)
   (rbtree (B t_r1 n_r t_r2) n_bd)]
  [(rbtree (R E n E) O)]
  ;; note the black-depth invariant precludes
  ;; a mixture of B and E's as R children
  [(rbtree (B t_1 n t_2) (s n_bd))
   (rbtree t_1 n_bd)
   (rbtree t_2 n_bd)])

(define-judgment-form rbtrees
  #:mode (rb-tree I)
  [(rb-tree t)
   (rbt t n_1 n_2 n_3)])

(define-judgment-form rbtrees
  #:mode (rbt I O O O)
  [(rbt (R E n E) n n O)]
  [(rbt (B E n E) n n (s O))]
  [(rbt (R (B t_l1 n_l t_l2) 
           n 
           (B t_r1 n_r t_r2))
        n_1min n_2max n_bd)
   (rbt (B t_l1 n_l t_l2) n_1min n_1max n_bd)
   (rbt (B t_r1 n_r t_r2) n_2min n_2max n_bd)
   (< n_1max n)
   (< n n_2min)]
  [(rbt (B E n (c_2 t_21 n_2 t_22)) n n_2max (s O))
   (rbt (c_2 t_21 n_2 t_22) n_2min n_2max O)
   (< n n_2min)]
  [(rbt (B (c_1 t_11 n_1 t_12) n E) n_1min n (s O))
   (rbt (c_1 t_11 n_1 t_12) n_1min n_1max O)
   (< n_1max n)]
  [(rbt (B (c_1 t_11 n_1 t_12) n (c_2 t_21 n_2 t_22)) n_1min n_2max (s n_bd))
   (rbt (c_1 t_11 n_1 t_12) n_1min n_1max n_bd)
   (rbt (c_2 t_21 n_2 t_22) n_2min n_2max n_bd)
   (< n_1max n)
   (< n n_2min)])

(define-metafunction rbtrees
  [(insert n t)
   (B t_1 n_1 t_2)
   (where (c t_1 n_1 t_2) (ins n t))])

(define-metafunction rbtrees
  [(ins n E)
   (R E n E)]
  [(ins n_1 (c t_1 n_2 t_2))
   (balance (c (ins n_1 t_1) n_2 t_2))
   (where #t (< n_1 n_2))]
  [(ins n_1 (c t_1 n_2 t_2))
   (balance (c t_1 n_2 (ins n_1 t_2)))
   (where #t (< n_2 n_1))]
  [(ins n t)
   t])

(define-metafunction rbtrees
  [(balance (B (R (R t_1 n_1 t_2) n_2 t_3) n_3 t_4))
   (R (B t_1 n_1 t_2) n_2 (B t_3 n_3 t_4))]
  [(balance (B (R t_1 n_1 (R t_2 n_2 t_3)) n_3 t_4))
   (R (B t_1 n_1 t_2) n_2 (B t_3 n_3 t_4))]
  [(balance (B t_1 n_1 (R (R t_2 n_2 t_3) n_3 t_4)))
   (R (B t_1 n_1 t_2) n_2 (B t_3 n_3 t_4))]
  [(balance (B t_1 n_1 (R t_2 n_2 (R t_3 n_3 t_4))))
   (R (B t_1 n_1 t_2) n_2 (B t_3 n_3 t_4))]
  [(balance t)
   t])


(define-metafunction rbtrees
  [(n->num O)
   0]
  [(n->num (s n))
   ,(+ 1 (term (n->num n)))])

(define-metafunction rbtrees
  [(t->num E)
   E]
  [(t->num (c t_1 n t_2))
   (c (t->num t_1) (n->num n) (t->num t_2))])

(define-metafunction rbtrees
  [(num->n 0)
   O]
  [(num->n number)
   (s (num->n ,(sub1 (term number))))])

(define-metafunction rbtrees
  [(numt->t E) 
   E]
  [(numt->t (c any_1 number any_2))
   (c (numt->t any_1) (num->n number) (numt->t any_2))])

(define (rand-ordt depth)
  (match (generate-term
          rbtrees
          #:satisfying
          (ordered? t n_1 n_2)
          depth)
    [#f #f]
    [`(ordered? ,t ,min ,max)
     (term (t->num ,t))]))

(define (rand-ordts depth num)
  (for/list ([_ (in-range num)])
    (rand-ordt depth)))

(define-judgment-form rbtrees
  #:mode (rbst I)
  [(rbst t)
   (rbtree t n_1)
   (ordered? t n_2 n_3)])

(define (rand-rbst depth)
  (match (generate-term rbtrees
                        #:satisfying
                        (rbst t)
                        depth)
    [#f #f]
    [`(rbst ,t)
     (term (t->num ,t))]))

(define (rand-rb-tree depth)
  (match (generate-term rbtrees
                        #:satisfying
                        (rb-tree t)
                        depth)
    [#f #f]
    [`(rb-tree ,t)
     (term (t->num ,t))]))

(define (rand-rbsts depth num)
  (for/list ([_ (in-range num)])
    (rand-rbst depth)))

(define (rand-rb-trees depth num)
  (for/list ([_ (in-range num)])
    (rand-rb-tree depth)))

(module+ 
 test
 (require rackunit)
 (check-true (judgment-holds
              (ordered?
               (B (R E (s O) E)
                  (s (s (s O)))
                  E)
               n_1 n_2)))
 (check-true (judgment-holds
              (rbtree (B (R E (s O) E)
                         (s (s (s O)))
                         E)
                      n_1)))
 (check-true (judgment-holds
              (rbst (B (R E (s O) E)
                       (s (s (s O)))
                       E))))
 (check-true (judgment-holds
              (rbst (R (B E (s O) E)
                       (s (s (s O)))
                       (B E
                          (s (s (s (s (s O)))))
                          E)))))
 (check-false (judgment-holds
               (rbst (R (B E (s (s O)) E)
                        (s O)
                        (R E O E)))))
 )

(define (ins-preserves-rb-tree t)
  (or (not (judgment-holds (rb-tree ,t)))
      (match (judgment-holds (ordered? ,t n_1 n_2) (n_1 n_2))
        [`((,min-n ,max-n))
         (define nmin (term (n->num ,min-n)))
         (define nmax (term (n->num ,max-n)))
         (for/and ([n (in-range (max 0 (sub1 nmin)) (+ 2 nmax))])
           (judgment-holds
            (rb-tree
             (insert (num->n ,n) ,t))))])))

(module+
 test
 (define (check-rbsts n)
   (for ([_ (in-range n)])
     (match (generate-term rbtrees
                           #:satisfying
                           (rbst t)
                           8)
       [#f (void)]
       [`(rbst ,t)
        (check-not-false (or (ins-preserves-rb-tree t)
                             (printf "~s\n" t)))])))
 
 (define (check-rbst/rb-tree tries)
   (for ([_ tries])
     (match (generate-term rbtrees
                           #:satisfying
                           (rbst t)
                           8)
       [#f (void)]
       [`(rbst ,t)
        (define res
          (judgment-holds
           (rb-tree ,t)))
        (unless res (displayln t))
        (check-not-false res)]))
   (for ([_ tries])
     (match (generate-term rbtrees
                           #:satisfying
                           (rb-tree t)
                           8)
       [#f (void)]
       [`(rb-tree ,t)
        (define res
          (judgment-holds
           (rb-tree ,t)))
        (unless res (displayln t))
        (check-not-false res)])))
 )

(define (generate-M-term)
  (generate-term rbtrees t 5))

(define (generate-typed-term)
  (match (generate-term rbtrees 
                        #:satisfying
                        (rb-tree t)
                        5)
    [`(rb-tree ,t)
     t]
    [#f #f]))

(define (type-check t)
  (judgment-holds (rb-tree ,t)))

(define (typed-generator)
  (let ([g (redex-generator rbtrees 
                            (rb-tree t)
                            5)])
    (λ () 
      (match (g)
        [`(rb-tree ,t)
         t]
        [#f #f]))))

(define (check t)
  (or (not t)
      (ins-preserves-rb-tree t)))