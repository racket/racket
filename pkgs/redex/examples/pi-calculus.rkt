#lang racket

;; this is a formulation of the pi-calculus in redex, following Milner's 1990 paper,
;; "Functions as Processes", available online (for now, anyhow) at:
;; ftp://ftp.inria.fr/INRIA/publication/publi-ps-gz/RR/RR-1154.ps.gz


;; the challenge here is handling the structural congruence rules; in particular,
;; these rules use congruence "as needed" to make the reductions work. An alternate
;; solution would be to determine a canonical representative for each of the
;; equivalence classes implied by structural congruence, and to formulate reduction
;; rules that are guaranteed to converge to this canonical element.  This would 
;; have the advantage that you wouldn't have to trust that my "as needed" rules
;; are adequate, but ... I couldn't see how to do it cleanly in the presence of
;; replicating terms. Also, this solution has the advantage that its steps match
;; those of milner; each of his steps is exactly one of our steps.

;; this model performs no "garbage collection" to remove dead terms.

(require redex/reduction-semantics)

(define-language π-calculus
  (π (π π)       ;; parallel composition
     zero         ;; dead computation
     (nu a π)     ;; fresh name
     (out a a π)  ;; sending 
     (in a a π)   ;; receiving
     (! π))       ;; replication
  (a variable)
  ;; evaluation contexts:
  (E hole
     (E π)
     (π E)
     (nu a E)
     (! E)))


(define red
  (reduction-relation 
   π-calculus
   
   ;; the only reduction rule: an "in" matches with an "out" on the same channel.
   (--> (in-hole E_1 ((in-hole E_in (in a_ch a_x π_in)) (in-hole E_out (out a_ch a_o π_out))))
        (in-hole E_1 (maybe-lift sent E_in (in a_ch a_x π_in) E_out (out a_ch a_o π_out)))
        
        (fresh sent)
        ;; the name of the channel must be free on both sides:
        (side-condition (and (not (member (term a_ch) (term (bound-by E_in))))
                             (not (member (term a_ch) (term (bound-by E_out)))))))
   
   
   ;; EXACTLY THE SAME except in & out are reversed in the input:
   (--> (in-hole E_1 ((in-hole E_out (out a_ch a_o π_out)) (in-hole E_in (in a_ch a_x π_in))))
        (in-hole E_1 (maybe-lift sent E_in (in a_ch a_x π_in) E_out (out a_ch a_o π_out)))
        
        (fresh sent)
        ;; the name of the channel must be free on both sides:
        (side-condition (and (not (member (term a_ch) (term (bound-by E_in))))
                             (not (member (term a_ch) (term (bound-by E_out)))))))))


;; maybe-lift : given a fresh identifier and two context/π pairs, perform replication
;; on each pair if necessary and then lift the nu-binding of the a_o name if necessary
;; (renaming it a_lifted)
(define-metafunction π-calculus
  maybe-lift : a E π E π -> π
  
  ;; the name being sent needs lifting, so that it includes the recipient:
  [(maybe-lift a_lifted E_in (in a_ch a_x π_in) E_out (out a_ch a_o π_out))
   (nu a_lifted ((in-hole E_i¬! (subst a_x a_lifted π_in))
                 (in-hole (rename-innermost a_o a_lifted E_o¬!)
                          (subst a_o a_lifted π_out))))
   
   (where E_i¬!  (replicate-as-needed E_in  (in a_ch a_x π_in)))
   (where E_o¬! (replicate-as-needed E_out (out a_ch a_o π_out)))
   
   (side-condition (member (term a_o) (term (bound-by E_out))))]
  
  ;; otherwise, no need to lift nu:
  [(maybe-lift a_lifted E_in (in a_ch a_x π_in) E_out (out a_ch a_o π_out))
   ((in-hole E_i¬! (subst a_x a_o π_in))
    (in-hole E_o¬! π_out))
   
   (where E_i¬!  (replicate-as-needed E_in  (in a_ch a_x π_in)))
   (where E_o¬! (replicate-as-needed E_out (out a_ch a_o π_out)))])



;; replicate-as-needed : performs replication to uncover the desired
;; expression
(define-metafunction π-calculus
  replicate-as-needed : E π -> E
  [(replicate-as-needed hole π_orig)     hole]  
  [(replicate-as-needed (E π) π_orig)    ((replicate-as-needed E π_orig) π)]
  [(replicate-as-needed (π E) π_orig)    (π (replicate-as-needed E π_orig))]
  [(replicate-as-needed (nu a E) π_orig) (nu a (replicate-as-needed E π_orig))]
  [(replicate-as-needed (! E) π_orig)    ((replicate-as-needed E π_orig)
                                          (in-hole (! E) π_orig))])

;; rename-innermost : rename the *innermost* binding of a given name along
;; an evaluation context, using another given name. At this point,
;; there should be no bangs along the context. Also, a_from should 
;; be fresh, so there's no need to worry about those collisions.
(define-metafunction π-calculus
  rename-innermost : a_from a_to E -> E
  ;; no hole case; the base case is no more bindings of a_from
  [(rename-innermost a_from a_to (E π))         ((rename-innermost a_from a_to E) π)]
  [(rename-innermost a_from a_to (π E))         (π (rename-innermost a_from a_to E))]
  
  ;; this is the last! substitute from here on out:
  [(rename-innermost a_from a_to (nu a_from E))  (subst/E a_from a_to E)
                                                 (side-condition (not (member (term a_from) (term (bound-by E)))))]
  
  [(rename-innermost a_from a_to (nu a_other E)) (nu a_other (rename-innermost a_from a_to E))]
  ;; no case for bang.  
  )

;; THE REST OF THIS IS BOILERPLATE... (subst, free-vars, etc.)


;; BOUND-BY : compute the variables bound along the spine of a context
(define-metafunction π-calculus
  bound-by : E -> (a ...)
  [(bound-by hole)     ()]
  [(bound-by (E π))   (bound-by E)]
  [(bound-by (π E))   (bound-by E)]
  [(bound-by (nu a E)) (a a_rest ...)
                       (where (a_rest ...) (bound-by E))]
  [(bound-by (! E)) (bound-by E)])




;; SUBST

;; substitute a_to for a_from in π.  Note that renaming may be necessary, 
;; when looking inside a binder for a_to.
(define-metafunction π-calculus
  subst : a a π -> π
  [(subst a_from a_to zero) zero]
  [(subst a_from a_to (π_1 π_2)) ((subst a_from a_to π_1)
                                  (subst a_from a_to π_2))]
  ;; shadowing of a_from: stop substitution
  [(subst a_from a_to (nu a_from π)) (nu a_from π)]
  ;; shadowing of a_to: rename if a_from occurs free.
  [(subst a_from a_to (nu a_to π))
   (nu a_new (subst a_from a_to (subst a_to a_new π)))
   (where a_new (fresh-in (nu a_to π) a_to))
   (side-condition (member (term a_from) (term (free-vars π))))]
  ;; otherwise, proceed:
  [(subst a_from a_to (nu a π))
   (nu a (subst a_from a_to π))]
  
  [(subst a_from a_to (out a_1 a_2 π))
   (out (subst-name a_from a_to a_1)
        (subst-name a_from a_to a_2) 
        (subst a_from a_to π))]
  ;; shadowing of a_from: stop substitution
  [(subst a_from a_to (in a_ch a_from π))
   (in (subst-name a_from a_to a_ch) a_from π)]
  ;; shadowing of a_to: rename if a_from occurs free
  [(subst a_from a_to (in a_ch a_to π))
   (in (subst-name a_from a_to a_ch) 
       a_new
       (subst a_from a_to (subst a_to a_new π)))
   (where a_new (fresh-in (in a_ch a_to π) a_to))
   (side-condition (member (term a_from) (term (free-vars π))))]
  ;; otherwise, proceed:
  [(subst a_from a_to (in a_ch a_x π))
   (in (subst-name a_from a_to a_ch) a_x (subst a_from a_to π))]
  
  [(subst a_from a_to (! π))
   (! (subst a_from a_to π))])

;; SUBST/E : substitute a_to for a_from in π; assumes 
;; there are no more bindings for a_from along the context sπne,
;; and that a_to is fresh (enough).
(define-metafunction π-calculus
  subst/E : a_from a_to E -> E
  
  [(subst/E a_from a_to hole)      hole]
  [(subst/E a_from a_to (π E))    ((subst a_from a_to π) (subst/E a_from a_to E))]
  [(subst/E a_from a_to (E π))    ((subst/E a_from a_to E) (subst a_from a_to π))]
  [(subst/E a_from a_to (nu a E))  (nu a (subst/E a_from a_to E))]
  [(subst/E a_from a_to (! E))  (! (subst/E a_from a_to))])


;; SUBST-NAME: replace a_from with a_to, leave other names alone.
(define-metafunction π-calculus
  subst-name : a a a -> a
  [(subst-name a_src a_tgt a_src)   a_tgt]
  [(subst-name a_src a_tgt a)       a])

;; FREE-VARS : return a list of the variables that occur free in the term
;; WARNING: MAY INCLUDE DUPLICATES...
(define-metafunction π-calculus
  free-vars : π -> (a ...)
  [(free-vars zero) ()]
  [(free-vars (π_1 π_2)) (a_1 ... a_2 ...)
                         (where (a_1 ...) (free-vars π_1))
                         (where (a_2 ...) (free-vars π_2))]
  [(free-vars (nu a π)) (set-minus (free-vars π) a)]
  [(free-vars (out a_ch a_o π)) (a_ch a_o a ...)
                                (where (a ...) (free-vars π))]
  [(free-vars (in a_ch a_x π)) (a_ch a ...)
                               (where (a ...) (set-minus (free-vars π) a_x))])

;; remove *all* instances of a_kilt from (a ...)
(define-metafunction π-calculus
  set-minus : (a ...) a -> (a ...)
  [(set-minus (a_1 ... a_kilt a_2 ...) a_kilt) (set-minus (a_1 ... a_2 ...) a_kilt)]
  [(set-minus (a ...) a_kilt) (a ...)])


;; provide variable-not-in as a metafunction:
(define-metafunction π-calculus
  fresh-in : π a -> a
  [(fresh-in π a) ,(variable-not-in (term π) (term a))])


;; TEST CASES

(test--> red (term zero))
;; simplest communication:
(test--> red
         (term ((in a b zero) (out a c zero)))
         (term (zero zero)))
;; check simplest substitution
(test--> red
         (term ((out a c zero) (in a b (out b b zero))))
         (term ((out c c zero) zero)))
;; check simple nesting
(test--> red
         (term (((out z y zero) (out a c zero)) ((in a b (out b b zero)) (out x w zero))))
         (term (((out c c zero) (out x w zero)) ((out z y zero) zero))))
;; outer context:
(test--> red
         (term (zero (nu a (((out z y zero) (out a c zero)) ((in a b (out b b zero)) (out x w zero))))))
         (term (zero (nu a (((out c c zero) (out x w zero)) ((out z y zero) zero))))))
;; with bang replication:
(test--> red
         (term (zero (nu a (((out z y zero) (out a c zero)) ((! (in a b (out b b zero))) (out x w zero))))))
         (term  (zero (nu a ((((out c c zero) (! (in a b (out b b zero)))) (out x w zero)) 
                             ((out z y zero) zero))))))
;; with lifting (depends on fresh name):
(test--> red
         (term (zero (nu a (((out z y zero) (nu c (out a c (in c c zero)))) ((! (in a b (out b b zero))) (out x w zero))))))
         (term (zero (nu a (nu sent
                               ((((out sent sent zero)
                                  (! (in a b (out b b zero))))
                                 (out x w zero))
                                ((out z y zero) (in sent c zero))))))))

;; need longer examples...

;; this on taken from wikipedia:
(test--> red
         (term ((nu x ((out x z zero)
                       (in x y (out y x (in x y zero)))))
                (in z v (out v v zero))))
         (term ((nu x ((out z x (in x y zero))
                       zero))
                (in z v (out v v zero)))))
(test--> red
         (term ((nu x ((out z x (in x y zero))
                       zero))
                (in z v (out v v zero))))
         (term (nu sent ((out sent sent zero)
                         ((in sent y zero) zero)))))

(test--> red
         (term (nu sent ((out sent sent zero)
                         ((in sent y zero) zero))))
         (term (nu sent ((zero zero)
                         zero))))


(test-equal (term (rename-innermost a1 a2 
                                    ((out a1 a1 zero)
                                     (nu a1 ((nu a1 (hole (out a1 a1 zero)))
                                             (out a1 a1 zero))))))
            (term ((out a1 a1 zero)
                   (nu a1 ((hole (out a2 a2 zero))
                           (out a1 a1 zero))))))

(test-equal (term (replicate-as-needed ((out a b zero)
                                        (! ((nu e (! hole))
                                               (out c d zero))))
                                       (out e f zero)))
            (term ((out a b zero)
                   (((nu e (hole
                            (! (out e f zero))))
                     (out c d zero))
                    (! ((nu e (! (out e f zero)))
                           (out c d zero)))))))

(begin (test-equal (term (bound-by hole))
                   '())
       (test-equal (term (bound-by (nu x hole)))
                   '(x))
       (test-equal (term (bound-by (nu x (nu y hole))))
                   '(x y))
       (test-equal (term (bound-by (zero (nu x hole))))
                   '(x))
       (test-equal (term (bound-by (! (nu x hole))))
                   '(x)))


(test-equal (term (set-minus (w x y x z) x))
              `(w y z))

(test-equal (term (free-vars zero)) `())
(test-equal (term (free-vars (out a b zero))) `(a b))
(test-equal (term (free-vars (out a b (out c d zero)))) `(a b c d))
(test-equal (term (free-vars ((out a b zero) (out c d zero)))) `(a b c d))
(test-equal (term (free-vars (nu a (out a a (out b a zero)))))  '(b))
(test-equal (term (free-vars (in a b (out b b zero)))) `(a))

(test-equal (term (subst a1 a2 (nu a3 (out a1 a1 zero))))
            (term (nu a3 (out a2 a2 zero))))

(test-equal (term (subst a1 a2 (in a1 a1 (out a1 a1 zero))))
            (term (in a2 a1 (out a1 a1 zero))))
(test-equal (term (subst a1 a2 (in a1 a2 (out a1 a1 (out a2 a2 zero)))))
            (term (in a2 a3 (out a2 a2 (out a3 a3 zero)))))
(test-equal (term (subst a1 a2 (in a1 a3 (out a3 a1 zero))))
            (term (in a2 a3 (out a3 a2 zero))))

(test-equal (term (subst a1 a2 (! (out a1 a1 zero))))
            (term (! (out a2 a2 zero))))


;; MILNER'S (1990) TRANSLATION FROM CBN-LAMBDA CALCULUS TO PI-CALCULUS:

(define-language lambda-calculus
  (e (lam x e) x (e e))
  (x (variable-except lam))
  (E hole (E e))
  
  (π (π π)
      zero
      (nu a π)
      (out a a π)
      (in a a π)
      (! π))
  (a variable))


;; not bothering to define reduction...

;; Encode-as-π : turn an lc term into a π-calculus term.
(define-metafunction lambda-calculus
  encode-as-π : e a -> π
  [(encode-as-π (lam x e) a)  (in a x (in a v (encode-as-π e v)))]
  [(encode-as-π x a)          (out x a zero)]
  [(encode-as-π (e_1 e_2) a)  (nu v ((encode-as-π e_1 v)
                                     (nu a_x (out v a_x (out v a (binding-encode a_x e_2))))))
                              (where a_x ,(variable-not-in (term e_2) (term x)))])

;; binding-encode : represent a binding.  This is the key idea: represent a binding
;; as a replicating agent that listens on a channel and delivers a channel corresponding
;; to the argument to anyone that wants it.
(define-metafunction lambda-calculus
  binding-encode : a e -> π
  [(binding-encode a e) (! (in a w (encode-as-π e w)))])

;; test sequence adapted from paper for the term ((lam x x) (lam y y))

(test--> red
         (term (encode-as-π ((lam x x) (lam y y)) chan))
         (term (nu v (nu sent ((in v v (out sent v zero)) (out v chan (! (in sent w (in w y (in w v (out y v zero)))))))))))

(test--> red
         (term (nu v (nu sent ((in v v (out sent v zero)) (out v chan (! (in sent w (in w y (in w v (out y v zero))))))))))
         (term (nu v (nu sent ((out sent chan zero) (! (in sent w (in w y (in w v (out y v zero))))))))))

(test--> red
         (term (nu v (nu sent ((out sent chan zero) (! (in sent w (in w y (in w v (out y v zero)))))))))
         (term (nu v (nu sent (((in chan y (in chan v (out y v zero))) (! (in sent w (in w y (in w v (out y v zero)))))) zero)))))

;; observe that the bang clause can be collected: it's listening on a channel that can't escape. So (observes Milner) this
;; is equivalent to (encode-as-π (lam y y) chan).

(test-results)
