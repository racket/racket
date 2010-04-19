#lang scheme

(require redex/reduction-semantics)
(require "grammar.ss" "verification.ss")

;; localrefs
(test-predicate
 (negate bytecode-ok?)
 '(loc 0))

(test-predicate
 bytecode-ok?
 '(let-one 7 (loc 0)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (loc 1)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (loc-box 0)))

(test-predicate
 bytecode-ok?
 '(let-one 7 (boxenv 0 (loc-box 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (loc 0))))

(test-predicate
 bytecode-ok?
 '(let-one (let-one 7 (loc 0)) (loc 0)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one (let-one 7 (loc 0)) (loc 1)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 2 (seq (loc-box-clr 0) (loc-box-clr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (seq (loc-noclr 0) (loc-clr 0))))

(test-predicate
 bytecode-ok?
 '(let-one 'x (seq (loc-noclr 0) (loc-noclr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (seq (loc-noclr 0) (loc-noclr 0) (loc-clr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 1 (seq (loc-box-noclr 0) (loc-box-clr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 1 (seq (loc-box-noclr 0) (loc-noclr 0) (loc-box-clr 0))))

(test-predicate
 bytecode-ok?
 '(let-void-box 1 (seq (loc-clr 0) 'x)))

(test-predicate
 bytecode-ok?
 '(let-void-box 1 (branch 'q (seq (loc-box-noclr 0) (loc-box-noclr 0)) 'q)))

;; let-one
(test-predicate
 bytecode-ok?
 '(let-one 'x (loc-noclr 0)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one (loc 0) 'z))

;; application
(test-predicate
 bytecode-ok?
 '(application 'w 'x 'y 'z))

(test-predicate
 bytecode-ok?
 '(let-one 7 (application (loc 3) 'x (loc 3) 'z)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (application (loc 0) 'x 'y 'z)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (application (let-one 'x 'y) (loc-noclr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(application (lam (ref) () 'x) 'y))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (application (lam (ref) () 'y) (loc-noclr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(application (lam (val val) () 'a) (let-void 2 'b) (install-value 2 'c 'd)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (boxenv 0 (application (lam (ref) () 'y) (loc-box-noclr 1)))))

(test-predicate
 bytecode-ok?
 '(application (lam (ref) () 'x)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (boxenv 0 (application (lam () () 'body) (loc-noclr 0)))))

(test-predicate
 (negate bytecode-ok?)
 '(application
   (let-one 'x (boxenv 0 (proc-const (val) 'y)))
   (loc-box-noclr 0)))

(test-predicate
 (negate bytecode-ok?)
 '(application
   (proc-const (val val) (branch (loc-noclr 0) 'a 'b))
   'x
   (install-value 0 'y (boxenv 0 'z))))

; self-app
(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0)))) 'x)))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (seq 'x (application (loc-noclr 0))))) 'y)))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (let-one 'x (boxenv 0 (application (loc-noclr 1)))))) 'y)))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (let-void 1 (install-value 0 'x (application (loc-noclr 0)))))) 'y)))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (branch 'x (application (loc-noclr 0)) (application (loc-noclr 0))))) 'y)))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (let-rec () (application (loc-noclr 0))))) 'y)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (let-rec ((lam () (0) (boxenv 0 (application (loc-noclr 0))))) 'x)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (let-void 1 (let-rec ((lam () (0 1) (seq (loc-clr 1) (application (loc-noclr 0))))) 'y))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (let-rec ((lam () (0) (application (loc-noclr 0)))) 'x)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 
   1
   (let-rec ((lam () (0)
                  (install-value
                   0
                   (proc-const () 'x)
                   (application (loc-noclr 0)))))
            (application (loc-noclr 0)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x
           (let-void 1
                     (let-rec ((lam (val) (1 0)
                                    (seq (loc-clr 0)
                                         (application (loc-noclr 2) 'y))))
                              (application (loc-noclr 1) 'z)))))

(test-predicate
 bytecode-ok?
 '(let-one 'x
           (let-void 1
                     (let-rec ((lam (val) (1 0)
                                    (application (loc-noclr 2) 'y)))
                              (application (loc-noclr 1) 'z)))))

(test-predicate
 bytecode-ok?
 '(let-one 'x
          (let-void 2
                    (let-rec ((lam () (1 2) (loc-clr 1))
                              (lam () (0 2) (loc-clr 1)))
                             'x))))

(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0 0) 'x)) 'y)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one (proc-const () void)
           (let-void 1
                     (let-rec ((lam () (0 1) 
                                    (seq (application (loc 1))
                                         (boxenv 1
                                                 (application (loc-noclr 0))))))
                              (application (loc 0))))))

(let ([lr '(let-rec ((lam () (0 1 2) 
                          (seq (application (loc-box-noclr 1))
                               (application (loc-noclr 2))
                               (application (loc-noclr 0)))))
                    (application (loc 0)))])
  (test-predicate
   bytecode-ok?
   `(let-one 'x (let-one 'y (boxenv 0 (let-void 1 ,lr))))))

; seq
(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (seq 'x (loc 0)))))

(test-predicate
 bytecode-ok?
 '(let-one 7 (boxenv 0 (seq (loc 0) 'x))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (seq (let-one 'x 'y) (loc-noclr 0))))

;; install-value
(test-predicate
 bytecode-ok?
 '(let-void 1 (install-value 0 'x (loc 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (install-value 0 'x (loc 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (install-value-box 0 'x 'y)))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (install-value 1 'x 'y)))

(test-predicate
 bytecode-ok?
 '(let-one 7 (boxenv 0 (install-value-box 0 'x (loc-box 0)))))

(test-predicate
 bytecode-ok?
 '(let-one
   'x
   (install-value-box 0 (boxenv 0 'y) (loc-box 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (install-value 0 'x 'y))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 1 (install-value-box 0 (loc-box-clr 0) 'x)))

(test-predicate
 (negate bytecode-ok?)
 '(application (loc-box 0) (install-value-box 0 'x 'y)))

;; let-void
(test-predicate
 (negate bytecode-ok?)
 '(let-void 2 (application (loc 0) (loc 2))))

(test-predicate
 bytecode-ok?
 '(let-void-box 2 (application (loc-box 1) (loc-box 2))))

;; box-env
(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (boxenv 1 'y)))

;; lam
(test-predicate
 bytecode-ok?
 '(let-one 
   'x 
   (let-one 
    'y
    (let-one
     'z
     (boxenv 
      2
      (lam
       (val val) (0 2)
       (application 
        (loc 3)
        (loc-box 4)
        (loc 5)
        (loc 6))))))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (lam () (1) 'n)))

(test-predicate
 (negate bytecode-ok?)
 '(lam () (0) 'n))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (application (lam (val) (0) 'x) 'y)))

;; proc-const
(test-predicate
 bytecode-ok?
 '(proc-const (val val) (application (loc-noclr 1) (loc-noclr 2))))

;; branch
(test-predicate
 bytecode-ok?
 '(let-one 'x (branch 'x (loc-noclr 0) (loc-clr 0))))

(test-predicate
 bytecode-ok?
 '(let-one 'x (branch 'y (loc-clr 0) (loc-clr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (seq (branch 'y 'z (loc-noclr 0)) (loc-clr 0))))

(test-predicate
 bytecode-ok?
 '(let-one 'x (seq (branch 'y (loc-noclr 0) 'z) (loc-clr 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (seq (branch 'y 'z (loc-clr 0)) (loc 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (seq (branch 'y (loc-clr 0) 'z) (loc 0))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (branch 'w (install-value-box 0 'x 'y) 'z)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (branch 'w 'z (install-value-box 0 'x 'y))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'w (branch 'x (boxenv 0 'y) (loc-clr 0))))

; let-rec
(test-predicate
 bytecode-ok?
 '(let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0)))) (application (loc-noclr 0)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 0 (let-rec ((lam () (0) (application (loc-noclr 0)))) (application (loc-noclr 0)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 
   1
   (let-rec ((lam (ref) () 'x))
            'y)))

(test-predicate
 (negate bytecode-ok?)
 '(let-void 1 (branch #f (let-rec ((lam () (0) 'x)) 'y) (loc-noclr 0))))

;; ignored? properly maintained
(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (seq (application (loc 0)) 'x))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (seq (application 'w (loc 0)) 'x))))

(test-predicate
 bytecode-ok?
 '(seq (let-void-box 1 (install-value-box 0 'x (loc 0))) 'y))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 7 (boxenv 0 (seq (install-value 0 (loc 0) 'x) 'y))))

(test-predicate
 bytecode-ok?
 '(seq (let-one 'x (boxenv 0 (loc 0))) 'y))

(test-predicate
 bytecode-ok?
 '(let-one 'x (boxenv 0 (seq (let-one 'y (loc 0)) 'z))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (boxenv 0 (seq (let-one (loc 0) 'y) 'z))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 1 (seq (branch (loc 0) 'x 'y) 'z)))

(test-predicate
 bytecode-ok?
 '(let-void-box 1 (seq (branch 'x (loc 0) (loc 0)) 'y)))

(test-predicate
 bytecode-ok?
 '(let-one 'x (boxenv 0 (seq (let-void 1 (let-rec ((lam () (0) 'y)) (loc-noclr 0))) 'z))))

;; ref args
(test-predicate
 bytecode-ok?
 '(let-one 'x (boxenv 0 (application (lam (ref) () (loc-box-noclr 0)) (loc-noclr 1)))))

(test-predicate
 bytecode-ok?
 '(let-one 'x (boxenv 0 (application (proc-const (ref) (loc-box-noclr 0)) (loc-noclr 1)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (boxenv 0 (application (lam (ref) () (loc-box-noclr 0)) (loc-noclr 0)))))

(test-predicate
 bytecode-ok?
 '(let-one 
   'x
   (boxenv
    0
    (application
     (lam (ref val) () 'y)
     (loc-noclr 2)
     (loc-box-noclr 2)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 
   'x
   (boxenv
    0
    (application
     (lam (ref val) () 'y)
     (loc-clr 2)
     (loc-box-noclr 2)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 
   'x
   (boxenv
    0
    (application
     (lam (ref ref) () 'y)
     (loc-clr 2)
     (loc-noclr 2)))))

(test-predicate 
 (negate bytecode-ok?)
 '(lam (val ref) () 'y))

; case-lam
(test-predicate bytecode-ok? '(case-lam))

(test-predicate
 bytecode-ok?
 '(let-one 'x (case-lam (lam (val) () 'y) (lam () (0) 'z))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (case-lam (lam (val) () 'y) (lam () (1) 'z))))

(test-predicate
 (negate bytecode-ok?)
 '(let-one 'x (case-lam (lam (val) () (loc-noclr 34)))))

(test-predicate
 (negate bytecode-ok?)
 '(let-void-box 1 (application (case-lam (lam (ref) () (loc-box-noclr 0))) (loc-noclr 1))))

; literals
(test-predicate bytecode-ok? #t)

(test-results)