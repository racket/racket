#lang racket

;"one point basis"
;"formal aspects of computing"

(require redex)

(initial-font-size 12)
(reduction-steps-cutoff 100)
(initial-char-width 80)

(define-language lang
  (e (e e)
     comb
     abs1
     abs2
     abs3)
  (e-ctxt (e e-ctxt)
          (e-ctxt e)
          hole)
  (comb i
        j
        b
        c
        c*
        w))

(define ij-relation
  (reduction-relation
   lang
   (--> (in-hole e-ctxt_1 (i e_1)) 
        (in-hole e-ctxt_1 e_1))
   (--> (in-hole e-ctxt_1 ((((j e_a) e_b) e_c) e_d))
        (in-hole e-ctxt_1 ((e_a e_b) ((e_a e_d) e_c))))))

(define relation
  (union-reduction-relations
   ij-relation
   (reduction-relation
    lang
    (--> (in-hole e-ctxt_1 (((b e_m) e_n) e_l))
         (in-hole e-ctxt_1 (e_m (e_n e_l))))
    (--> (in-hole e-ctxt_1 (((c e_m) e_n) e_l))
         (in-hole e-ctxt_1 ((e_m e_l) e_n)))
    (--> (in-hole e-ctxt_1 ((c* e_a) e_b))
         (in-hole e-ctxt_1 (e_b e_a)))
    (--> (in-hole e-ctxt_1 ((w e_a) e_b))
         (in-hole e-ctxt_1 ((e_a e_b) e_b))))))


(define c* `((j i) i))
(define (make-c c*) `(((j ,c*) (j ,c*)) (j ,c*)))
(define (make-b c) `((,c ((j i) ,c)) (j i)))
(define (make-w b c c*) `(,c ((,c ((,b ,c) ((,c ((,b j) ,c*)) ,c*))) ,c*)))
(define (make-s b c w) `((,b ((,b (,b ,w)) ,c)) (,b ,b)))

(traces relation
        (list 
         `((,c* abs1) abs2)
         `(((,(make-c 'c*) abs1) abs2) abs3)
         `(((,(make-b 'c) abs1) abs2) abs3)
         `((,(make-w 'b 'c 'c*) abs1) abs2)
         `(((,(make-s 'b 'c 'w) abs1) abs2) abs3))
        #:multiple? #t)
