#lang racket
(require redex/reduction-semantics)

(provide list-machine red 
         var-lookup var-set
         program-lookup
         different)

(define-language list-machine
  (a nil
     (cons a a))
  (v variable-not-otherwise-mentioned)
  (r empty
     (r v ↦ a))
  (l variable-not-otherwise-mentioned)
  (ι (jump l)
     (branch-if-nil v l)
     (fetch-field v 0 v)
     (fetch-field v 1 v)
     (cons v v v)
     halt
     (begin ι ι))
  (p (l : ι p)
     end))

(define-judgment-form list-machine
  #:contract (var-lookup r v a)
  #:mode (var-lookup I I O)
  [-----
   (var-lookup (r v ↦ a) v a)]
  [(where #t (different v_1 v_2))
   (var-lookup r v_2 a_2)
   -----
   (var-lookup (r v_1 ↦ a_1) v_2 a_2)])

(define-judgment-form list-machine
  #:contract (var-set r v a r)
  #:mode (var-set I I I O)
  [-----
   (var-set (r v ↦ a) v a_2 (r v ↦ a_2))]
  [(where #t (different v v_2))
   (var-set r v_2 a_2 r_2)
   -----
   (var-set (r v ↦ a) v_2 a_2 (r_2 v ↦ a))]
  [-----
   (var-set empty v a (empty v ↦ a))])

(define-judgment-form list-machine
  #:contract (program-lookup p l ι)
  #:mode (program-lookup I I O)
  [-----
   (program-lookup (l : ι p) l ι)]
  [(where #t (different l l_2))
   (program-lookup p l_2 ι_2)
   -----
   (program-lookup (l : ι p) l_2 ι_2)])

(define red 
  (reduction-relation 
   list-machine
   #:domain (p r ι)
   (--> (p r (begin (begin ι_1 ι_2) ι_3))
        (p r (begin ι_1 (begin ι_2 ι_3)))
        "step-seq")
   (--> (p r (begin (fetch-field v 0 v_2) ι))
        (p r_2 ι)
        "step-fetch-field-0"
        (judgment-holds (var-lookup r v (cons a_0 a_1)))
        (judgment-holds (var-set r v_2 a_0 r_2)))
   (--> (p r (begin (fetch-field v 1 v_2) ι))
        (p r_2 ι)
        "step-fetch-field-1"
        (judgment-holds (var-lookup r v (cons a_0 a_1)))
        (judgment-holds (var-set r v_2 a_1 r_2)))
   (--> (p r (begin (cons v_0 v_1 v_2) ι))
        (p r_2 ι)
        "step-cons"
        (judgment-holds (var-lookup r v_0 a_0))
        (judgment-holds (var-lookup r v_1 a_1))
        (judgment-holds (var-set r v_2 (cons a_0 a_1) r_2)))
   (--> (p r (begin (branch-if-nil v l) ι))
        (p r ι)
        "step-branch-not-taken"
        (judgment-holds (var-lookup r v (cons a_0 a_1))))
   (--> (p r (begin (branch-if-nil v l) ι))
        (p r ι_2)
        "step-branch-taken"
        (judgment-holds (var-lookup r v nil))
        (judgment-holds (program-lookup p l ι_2)))
   (--> (p r (jump l))
        (p r ι_2)
        "step-jump"
        (judgment-holds (program-lookup p l ι_2)))))


(define-metafunction list-machine
  different : any any -> any
  [(different any_1 any_2) ,(not (equal? (term any_1) (term any_2)))])
