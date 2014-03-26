#lang racket
(require "list-machine.rkt"
         "list-machine-typing.rkt"
         redex)

(test-equal (judgment-holds (var-lookup (empty x ↦ nil) x a) a)
            (list (term nil)))
(test-equal (judgment-holds (var-lookup (empty x ↦ nil) y a) a)
            (list))
(test-equal (judgment-holds (var-lookup ((empty y ↦ (cons nil nil)) x ↦ nil) y a) a)
            (list (term (cons nil nil))))

(test-equal (judgment-holds (var-set empty x nil r) r)
            (list (term (empty x ↦ nil))))
(test-equal (judgment-holds (var-set (empty x ↦ nil) x (cons nil nil) r) r)
            (list (term (empty x ↦ (cons nil nil)))))
(test-equal (judgment-holds (var-set (empty x ↦ nil) y (cons nil nil) r) r)
            (list (term ((empty y ↦ (cons nil nil)) x ↦ nil))))

(test-equal (judgment-holds (program-lookup (l1 : halt end) l1 ι) ι)
            (list (term halt)))
(test-equal (judgment-holds (program-lookup (l1 : halt (l2 : (begin halt halt) end)) l2 ι) ι)
            (list (term (begin halt halt))))

(test--> red
         (term (end empty (begin (begin (jump l1) (jump l2)) (jump l3))))
         (term (end empty (begin (jump l1) (begin (jump l2) (jump l3))))))
(test--> red
         (term (end 
                ((empty x ↦ (cons (cons nil nil) nil)) y ↦ nil)
                (begin (fetch-field x 0 y) halt)))
         (term (end 
                ((empty x ↦ (cons (cons nil nil) nil)) y ↦ (cons nil nil))
                halt)))
(test--> red
         (term (end 
                ((empty x ↦ (cons nil (cons nil nil))) y ↦ nil)
                (begin (fetch-field x 1 y) halt)))
         (term (end 
                ((empty x ↦ (cons nil (cons nil nil))) y ↦ (cons nil nil))
                halt)))
(test--> red
         (term (end ((empty x ↦ nil) y ↦ nil) (begin (cons x y z) halt)))
         (term (end (((empty z ↦ (cons nil nil)) x ↦ nil) y ↦ nil) halt)))
(test--> red
         (term (end (empty x ↦ (cons nil nil)) (begin (branch-if-nil x l) halt)))
         (term (end (empty x ↦ (cons nil nil)) halt)))
(test--> red
         (term ((l : (begin halt halt) end) (empty x ↦ nil) (begin (branch-if-nil x l) halt)))
         (term ((l : (begin halt halt) end) (empty x ↦ nil) (begin halt halt))))
(test--> red
         (term ((l : (begin halt halt) end) (empty x ↦ nil) (jump l)))
         (term ((l : (begin halt halt) end) (empty x ↦ nil) (begin halt halt))))

(test-equal (judgment-holds (:lookup (x : nil empty) x τ) τ)
            (list (term nil)))
(test-equal (judgment-holds (:lookup (x : nil empty) y τ) τ)
            (list))
(test-equal (judgment-holds (:lookup (x : nil (y : (list nil) empty)) y τ) τ)
            (list (term (list nil))))

(test-equal (judgment-holds (:set empty x nil Γ) Γ)
            (list (term (x : nil empty))))
(test-equal (judgment-holds (:set (x : nil empty) x (list nil) Γ) Γ)
            (list (term (x : (list nil) empty))))
(test-equal (judgment-holds (:set (x : nil empty) y (list nil) Γ) Γ)
            (list (term (x : nil (y : (list nil) empty)))))

(test-equal (judgment-holds (⊂ nil nil)) #t)
(test-equal (judgment-holds (⊂ nil (list nil))) #t)
(test-equal (judgment-holds (⊂ (list nil) (list (list nil)))) #t)
(test-equal (judgment-holds (⊂ (listcons nil) (list (list nil)))) #t)
(test-equal (judgment-holds (⊂ (listcons nil) (listcons (list nil)))) #t)

(redex-check
 list-machine-typing
 τ
 (let ()
   (define subtypes (judgment-holds (⊂ τ_s τ) τ_s))
   (or (null? subtypes)
       (let ([τ_1 (list-ref subtypes (random (length subtypes)))]
             [τ_2 (list-ref subtypes (random (length subtypes)))])
         (for/and ([τ_3 (judgment-holds (⊔ ,τ_1 ,τ_2 τ_3) τ_3)])
           (judgment-holds (⊂ τ_3 τ))))))
 #:attempts 100)

(test-equal (judgment-holds (Γ-⊂ (x : nil empty) (x : nil empty)))
            #t)
(test-equal (judgment-holds (Γ-⊂ (x : nil empty) (x : (list nil) empty)))
            #t)
(test-equal (judgment-holds (Γ-⊂ (x : nil (y : (list nil) empty)) (x : (list nil) empty)))
            #t)
(test-equal (judgment-holds (Γ-⊂ (y : (list nil) (x : nil empty)) (x : (list nil) empty)))
            #t)

(test-equal (judgment-holds (check-instr (l0 : (x : (list nil) empty) empty)
                                         (x : (list nil) empty)
                                         (branch-if-nil x l0)
                                         Γ)
                            Γ)
            (list (term (x : (listcons nil) empty))))
(test-equal (judgment-holds (check-instr (l0 : (x : nil empty) empty)
                                         (x : (listcons nil) empty)
                                         (branch-if-nil x l0)
                                         Γ)
                            Γ)
            (list (term (x : (listcons nil) empty))))
(test-equal (judgment-holds (check-instr (l0 : (x : nil empty) empty)
                                         (x : nil empty)
                                         (branch-if-nil x l0)
                                         Γ)
                            Γ)
            (list (term (x : nil empty))))

(test-equal (judgment-holds (check-instr empty
                                         (x : (listcons nil) empty)
                                         (fetch-field x 0 y)
                                         Γ)
                            Γ)
            (list (term (x : (listcons nil) (y : nil empty)))))
(test-equal (judgment-holds (check-instr empty
                                         (x : (listcons nil) empty)
                                         (fetch-field x 1 y)
                                         Γ)
                            Γ)
            (list (term (x : (listcons nil) (y : (list nil) empty)))))
(test-equal (judgment-holds (check-instr empty
                                         (x : (listcons nil) 
                                            (y : (list (list nil)) 
                                               (z : nil empty)))
                                         (cons x y z)
                                         Γ)
                            Γ)
            (list (term (x : (listcons nil) 
                           (y : (list (list nil)) 
                              (z : (listcons (list nil))
                                 empty))))))
(test-equal (judgment-holds (check-instr
                             (l0 : (v0 : nil empty) empty)
                             (v0 : nil empty)
                             (cons v0 x f)
                             Γ))
            #f)

(test-equal (judgment-holds (check-block empty empty halt)) #t)
(test-equal (judgment-holds (check-block empty
                                         (x : (listcons nil) empty)
                                         (begin (fetch-field x 0 y) halt)))
            #t)
(test-equal (judgment-holds (check-block (l0 : empty empty)
                                         (x : nil empty)
                                         (jump l0)))
            #t)
(test-equal (judgment-holds (check-blocks empty end)) #t)
(test-equal (judgment-holds (check-blocks (l0 : empty empty) (l0 : halt end))) #t)
(test-equal (judgment-holds (check-program (l0 : halt end) (l0 : (v0 : nil empty) empty)))
            #t)

(test-equal (term (dom (l0 : halt (l1 : halt end))))
            (term (l0 l1)))
(test-equal (term (dom (l0 : empty (l2 : empty end))))
            (term (l0 l2)))
(test-equal (term (l-⊂ (l0 l1 l2) (l2 l3 l0 l1 l11)))
            #t)
(test-equal (term (l-⊂ (l0 l1 l2 l33) (l2 l3 l0 l1 l11)))
            #f)
(test-results)
