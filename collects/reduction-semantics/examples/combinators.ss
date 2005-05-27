;"one point basis"
;"formal aspects of computing"

(module combinators mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (initial-font-size 12)
  (reduction-steps-cutoff 100)
  (initial-char-width 80)
  
  (define lang
    (language 
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
           w)))
  
  (define ij-reductions
    (list
     (reduction/context lang
                        e-ctxt
                        (i e_1)
                        (term e_1))
     (reduction/context lang
                        e-ctxt
                        ((((j e_a) e_b) e_c) e_d)
                        (term ((e_a e_b) ((e_a e_d) e_c))))))
  
  (define reductions
    (append
     ij-reductions
     (list
      (reduction/context lang
                         e-ctxt
                         (((b e_m) e_n) e_l)
                         (term (e_m (e_n e_l))))
      (reduction/context lang
                         e-ctxt
                         (((c e_m) e_n) e_l)
                         (term ((e_m e_l) e_n)))
      (reduction/context lang
                         e-ctxt
                         ((c* e_a) e_b)
                         (term (e_b e_a)))
      (reduction/context lang
                         e-ctxt
                         ((w e_a) e_b)
                         (term ((e_a e_b) e_b))))))

  
  (define c* `((j i) i))
  (define (make-c c*) `(((j ,c*) (j ,c*)) (j ,c*)))
  (define (make-b c) `((,c ((j i) ,c)) (j i)))
  (define (make-w b c c*) `(,c ((,c ((,b ,c) ((,c ((,b j) ,c*)) ,c*))) ,c*)))
  (define (make-s b c w) `((,b ((,b (,b ,w)) ,c)) (,b ,b)))

  (traces/multiple lang
                reductions
                (list 
                 `((,c* abs1) abs2)
                 `(((,(make-c 'c*) abs1) abs2) abs3)
                 `(((,(make-b 'c) abs1) abs2) abs3)
                 `((,(make-w 'b 'c 'c*) abs1) abs2)
                 `(((,(make-s 'b 'c 'w) abs1) abs2) abs3)))
  
  ;; s in terms of i and j ( > 18,000 reductions and probably still long way to go)
  '(traces lang ij-reductions
       (make-s (make-b (make-c c*))
               (make-c c*)
               (make-w (make-b (make-c c*))
                       (make-c c*)
                       c*))))
