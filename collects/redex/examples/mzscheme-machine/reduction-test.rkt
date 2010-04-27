#lang scheme

(require redex/reduction-semantics
         "reduction.ss")

;                                                                                             
;                                                                                             
;  ;;; ;;;                        ;;;                                ;                        
;   ;; ;;          ;             ;                           ;                                
;   ;; ;;   ;;;   ;;;;;   ;;;   ;;;;; ;;  ;; ;; ;;    ;;;;  ;;;;;  ;;;     ;;;  ;; ;;    ;;;; 
;   ; ; ;  ;   ;   ;     ;   ;   ;     ;   ;  ;;  ;  ;   ;   ;       ;    ;   ;  ;;  ;  ;   ; 
;   ; ; ;  ;;;;;   ;      ;;;;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;   ;;;  
;   ;   ;  ;       ;     ;   ;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;      ; 
;   ;   ;  ;       ;   ; ;   ;   ;     ;  ;;  ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ;  ;   ; 
;  ;;; ;;;  ;;;;    ;;;   ;;;;; ;;;;;   ;; ;;;;; ;;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;; ;;;;  
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             


;; heap-ref
(test-equal (term (heap-ref (box y) ((w 0) (x 1) (y 2) (z 3)))) 2)

;; heap-set
(test-equal (term (heap-set 7 (box y) ((w 0) (x 1) (y 2) (z 3))))
            (term ((w 0) (x 1) (y 7) (z 3))))

;; push
(test-equal (term (push (1 2 3) (4 5 ε)))
            (term (1 2 3 4 5 ε)))

;; push-uninit
(test-equal (term (push-uninit 2 (1 2 (3 uninit 4 ε))))
            (term (uninit uninit 1 2 (3 uninit 4 ε))))

;; stack-ref
(test-equal (term (stack-ref 1 (1 2 3 (ε)))) 2)
(test-equal (term (stack-ref 3 (1 2 (3 4 (ε))))) 4)

;; stack-set
(test-equal (term (stack-set 2 1 (1 uninit uninit 4 ε)))
            (term (1 2 uninit 4 ε)))
(test-equal (term (stack-set 4 3 (1 2 (3 uninit 5 ε))))
            (term (1 2 (3 4 5 ε))))

;; load

(define-syntax test-load
  (syntax-rules ()
    [(_ e (e* h t))
     (test-predicate
      (redex-match runtime (uninit (((ε))) h t (e*)))
      (term (load e ())))]))

(test-load
 (proc-const (val ref) 'body)
 ((clos x_1) ((x_1 ((clos 2 () x_2)))) ((x_2 'body))))

(test-load
 (application 
  (proc-const (val ref) 'body1)
  (proc-const (val) 'body2))
 ((application (clos x_1) (clos x_2)) 
  ((x_1 ((clos 2 () x_3)))
   (x_2 ((clos 1 () x_4))))
  ((x_3 'body1) (x_4 'body2))))

(test-load
 (seq 
  (proc-const (val ref) 'body1)
  (proc-const (val) 'body2))
 ((seq (clos x_1) (clos x_2)) 
  ((x_1 ((clos 2 () x_3)))
   (x_2 ((clos 1 () x_4))))
  ((x_3 'body1) (x_4 'body2))))

(test-load
 (let-rec ((lam () (1) 'body1)
           (lam (val) (0) 'body2))
          (lam (val val) (0) 'body3))
 ((let-rec ((lam 0 (1) x_1)
            (lam 1 (0) x_2))
           (lam 2 (0) x_3))
  ()
  ((x_3 'body3) (x_1 'body1) (x_2 'body2))))

(test-load
 (let-one 
  (proc-const (val ref) 'body1)
  (proc-const (val) 'body2))
 ((let-one (clos x_1) (clos x_2)) 
  ((x_1 ((clos 2 () x_3)))
   (x_2 ((clos 1 () x_4))))
  ((x_3 'body1) (x_4 'body2))))

(test-load
 (let-void 0 (proc-const (val ref) 'body))
 ((let-void 0 (clos x_1)) 
  ((x_1 ((clos 2 () x_2))))
  ((x_2 'body))))

(test-load
 (boxenv 0 (proc-const (val ref) 'body))
 ((boxenv 0 (clos x_1)) 
  ((x_1 ((clos 2 () x_2))))
  ((x_2 'body))))

(test-load
 (install-value 0 
                (proc-const (val ref) 'body1)
                (proc-const (val) 'body2))
 ((install-value 0 (clos x_1) (clos x_2)) 
  ((x_1 ((clos 2 () x_3)))
   (x_2 ((clos 1 () x_4))))
  ((x_3 'body1) (x_4 'body2))))

(test-load
 (branch
  (proc-const (val ref) 'body1)
  (proc-const (val) 'body2)
  (proc-const () 'body3))
 ((branch (clos x_1) (clos x_2) (clos x_3)) 
  ((x_1 ((clos 2 () x_4)))
   (x_2 ((clos 1 () x_5)))
   (x_3 ((clos 0 () x_6))))
  ((x_4 'body1) (x_5 'body2) (x_6 'body3))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0)))) 'x))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'x))
  ()
  ((x_1 (self-app x_1 (loc-noclr 0))))))

(test-load
 (let-void 1 (let-rec ((lam (val) (0) (application (loc-noclr 1) 'x))) 'y))
 ((let-void 1 (let-rec ((lam 1 (0) x_1)) 'y))
  ()
  ((x_1 (self-app x_1 (loc-noclr 1) 'x)))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0) 'x))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (application (loc-noclr 0) 'x)))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (boxenv 0 (application (loc-box-noclr 0))))) 'x))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'x))
  ()
  ((x_1 (boxenv 0 (application (loc-box-noclr 0)))))))

(test-load
 (let-void 1 (let-rec ((lam (val) (0) (application (loc-noclr 0)))) 'x))
 ((let-void 1 (let-rec ((lam 1 (0) x_1)) 'x))
  ()
  ((x_1 (application (loc-noclr 0))))))

(test-load
 (let-one 'x (let-void 1 (let-rec ((lam () (1 0) (application (loc-noclr 1)))) 42)))
 ((let-one 'x (let-void 1 (let-rec ((lam 0 (1 0) x_1)) 42)))
  ()
  ((x_1 (self-app x_1 (loc-noclr 1))))))

(test-load
 (let-one 'x (let-void 1 (let-rec ((lam () (1) (application (loc-noclr 0)))) 42)))
 ((let-one 'x (let-void 1 (let-rec ((lam 0 (1) x_1)) 42)))
  ()
  ((x_1 (application (loc-noclr 0))))))

(test-load
 (let-one 'x (let-void 1 (let-rec ((lam () (1) (application (loc-noclr 0)))) 42)))
 ((let-one 'x (let-void 1 (let-rec ((lam 0 (1) x_1)) 42)))
  ()
  ((x_1 (application (loc-noclr 0))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (lam () (0) (application (loc-noclr 0))))) 'x))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'x))
  ()
  ((x_1 (lam 0 (0) x_2))
   (x_2 (application (loc-noclr 0))))))

(test-load
 (let-void 1 (let-rec ((lam () (0 0) (application (loc-noclr 0)))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0 0) x_1)) 'y))
  ()
  ((x_1 (application (loc-noclr 0))))))

(test-load
 (let-void 1 (let-rec ((lam () (0 0) (application (loc-noclr 1)))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0 0) x_1)) 'y))
  ()
  ((x_1 (self-app x_1 (loc-noclr 1))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (let-one 'x (boxenv 0 (application (loc-noclr 1)))))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (let-one 'x (boxenv 0 (self-app x_1 (loc-noclr 1))))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (let-one (application (loc-noclr 1)) 'x))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (let-one (application (loc-noclr 1)) 'x)))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (application (application (loc-noclr 0))))) 'x))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'x))
  ()
  ((x_1 (application (application (loc-noclr 0)))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (let-rec () (application (loc-noclr 0))))) 'x))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'x))
  ()
  ((x_1 (let-rec () (self-app x_1 (loc-noclr 0)))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (let-void 1 (install-value 1 'x (application (loc-noclr 1)))))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (let-void 1 (install-value 1 'x (self-app x_1 (loc-noclr 1))))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (let-void 1 (install-value 1 (application (loc-noclr 1)) 'x)))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (let-void 1 (install-value 1 (application (loc-noclr 1)) 'x))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (seq 'x (application (loc-noclr 0))))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (seq 'x (self-app x_1 (loc-noclr 0)))))))

(test-load
 (let-void 1 (let-rec ((lam () (0) (seq (application (loc-noclr 0)) 'x))) 'y))
 ((let-void 1 (let-rec ((lam 0 (0) x_1)) 'y))
  ()
  ((x_1 (seq (application (loc-noclr 0)) 'x)))))

(test-load
 (case-lam (lam (val) () (lam (val) (0) 'x)) (lam (val val) () 'y))
 ((case-lam (lam 1 () x_1) (lam 2 () x_3))
  ()
  ((x_1 (lam 1 (0) x_2)) (x_2 'x) (x_3 'y))))

;                                                                 
;                                                                 
;  ;;;;;             ;;                         ;                 
;   ;   ;             ;                 ;                         
;   ;   ;   ;;;    ;; ; ;;  ;;   ;;;;  ;;;;;  ;;;     ;;;  ;; ;;  
;   ;   ;  ;   ;  ;  ;;  ;   ;  ;   ;   ;       ;    ;   ;  ;;  ; 
;   ;;;;   ;;;;;  ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ; 
;   ;  ;   ;      ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ; 
;   ;   ;  ;      ;   ;  ;  ;;  ;   ;   ;   ;   ;    ;   ;  ;   ; 
;  ;;;   ;  ;;;;   ;;;;;  ;; ;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;;
;                                                                 
;                                                                 
;                                                                 
;                                                                 

(define step (compose car (curry apply-reduction-relation ->)))

;; application
(test-->
 ->
 (term (uninit (((ε))) () () ((application 1 2 3))))
 (term (uninit (uninit uninit ((ε))) () () ((reorder (call 2) (1 ?) (2 0) (3 1))))))

;; self-app
(test-->
 ->
 (term
  ((clos x)
   ('a ((clos x) ('b ε)))
   ((x ((clos 0 ((clos x)) x1))))
   ((x1 (self-app x1 (loc-noclr 1) 'c)))
   ((self-app x1 (loc-noclr 1) 'c))))
 (term
  ((clos x)
   ('a ((clos x) ('b ε)))
   ((x ((clos 0 ((clos x)) x1))))
   ((x1 (self-app x1 (loc-noclr 1) 'c)))
   ((application (loc-noclr 1) 'c))))
 (term
  ((clos x)
   (uninit 'a ((clos x) ('b ε)))
   ((x ((clos 0 ((clos x)) x1))))
   ((x1 (self-app x1 (loc-noclr 1) 'c)))
   ((reorder (self-call x1) ('c 0))))))

;; reorder
(test-->
 ->
 (term (uninit (((ε))) () () ((reorder (call 1) ('x 0) ((loc-noclr 0) ?)))))
 (term (uninit (((ε))) () () (framepush 'x framepop (set 0)
                                        framepush (loc-noclr 0) framepop
                                        (call 1)))))
(test-->
 ->
 (term (uninit (((ε))) () () ((reorder (call 1) ((loc-noclr 0) ?) ('x 0)))))
 (term (uninit (((ε))) () () ((reorder (call 1) ('x 0) ((loc-noclr 0) ?)))))
 (term (uninit (((ε))) () () (framepush (loc-noclr 0) framepop (set 0)
                                        framepush 'x framepop
                                        (swap 0) (call 1)))))

(test-->
 ->
 (term (uninit (((ε))) () ((x 'q)) ((reorder (self-call x) ('x 0) ((loc-noclr 0) 1)))))
 (term (uninit (((ε))) () ((x 'q)) (framepush 'x framepop (set 0)
                                              framepush (loc-noclr 0) framepop (set 1)
                                              (self-call x)))))
(test-->
 ->
 (term (uninit (((ε))) () ((x 'q)) ((reorder (self-call x) ((loc-noclr 0) 1) ('x 0)))))
 (term (uninit (((ε))) () ((x 'q)) ((reorder (self-call x) ('x 0) ((loc-noclr 0) 1)))))
 (term (uninit (((ε))) () ((x 'q)) (framepush (loc-noclr 0) framepop (set 1)
                                              framepush 'x framepop (set 0)
                                              (self-call x)))))

(test-equal
 (sort
  (map
   car
   (apply-reduction-relation/tag-with-names
    ->
    (term 
     (uninit
      (uninit uninit 'a 'b 'c ε)
      ()
      ()
      ((reorder (call 2) ((loc-noclr 0) ?) ((loc-noclr 1) 0) ((loc-noclr 2) 1)))))))
  string<=?)
 '("finalize-app-not-last" "reorder" "reorder"))

;; swap
(test-->
 ->
 (term ('z ('a 'b 'c ε) () () ((swap 1))))
 (term ('b ('a 'z 'c ε) () () ())))

;; call
(test-->
 ->
 (term ((clos x1) ('x 'y 'z ('q ('r ε))) ((x1 ((clos 2 ('w) x2)))) ((x2 'z)) ((call 2))))
 (term ((clos x1) (('w ('x 'y ε))) ((x1 ((clos 2 ('w) x2)))) ((x2 'z)) ('z))))

(test-->
 ->
 (term ((clos x1) 
        ('x 'y 'z ('q ('r ε)))
        ((x1 ((clos 1 () x2) (clos 2 ('w) x3) (clos 2 ('w) x4))))
        ((x2 'a) (x3 'b) (x4 'c))
        ((call 2))))
 (term ((clos x1)
        (('w ('x 'y ε)))
        ((x1 ((clos 1 () x2) (clos 2 ('w) x3) (clos 2 ('w) x4))))
        ((x2 'a) (x3 'b) (x4 'c))
        ('b))))

(test-equal
 (caar
  (apply-reduction-relation*
   ->
   (term (load (application (lam (val) () (loc-noclr 0)) (application (lam (val) () (loc-noclr 0)) 'x)) ()))))
 ''x)

;; self-call
(test-->
 ->
 (term
  ('c
   ('d 'b ((clos x) ('c ε)))
   ((x ((clos 0 ((clos x)) x1))))
   ((x1 (self-app x1 (loc-noclr 1) 'c)))
   ((self-call x1))))
 (term
  ('c
   (((clos x) ('d ε)))
   ((x ((clos 0 ((clos x)) x1))))
   ((x1 (self-app x1 (loc-noclr 1) 'c)))
   ((self-app x1 (loc-noclr 1) 'c)))))

(test-equal
 (map 
  car
  (apply-reduction-relation* 
   ->
   (term (load (let-void 1 (let-rec ((lam (val val) (0) 
                                          (branch (loc-noclr 1)
                                                  (loc-noclr 2)
                                                  (application (loc-noclr 2) #t (loc-noclr 3)))))
                                    (application (loc-noclr 2) #f #t))) ()))))
 '(#f))

;; arity
(test-equal
 (step (term ((clos x1) ('y 'q ε) ((x1 ((clos 2 ('w) x2)))) ((x2 'z)) ((call 1)))))
 (term error))

(test-->>
 ->
 (term (uninit (((ε))) () ((x 'x)) ((let-one 7 (boxenv 0 (application (lam 0 (1) x) 'y))))))
 (term error))

(test-->
 ->
 (term
  ((clos x170017)
   (19 ((ε)))
   ((x170017
     ((clos 4 () x170018)))
    (x170015
     ((clos 9 () x170016))))
   ((x170018 (clos x170015))
    (x170016 void))
   ((call 1))))
 (term error))

;; non-closure
(test-equal
 (step (term ('f ('x 'y 'q ε) () () ((call 2)))))
 (term error))

;; localref
(test-->
 ->
 (term (uninit (1 ε) () () ((loc 0))))
 (term (1 (1 ε) () () ())))

;; loc-box
(test-->
 ->
 (term (uninit ((box x) ε) ((x 1)) () ((loc-box 0))))
 (term (1 ((box x) ε) ((x 1)) () ())))

;; loc-clr
(test-equal
 (step (term (uninit (1 ε) () () ((loc-clr 0)))))
 (term (1 (uninit ε) () () ())))

;; loc-box-clr
(test-equal
 (step (term (uninit ((box x) ε) ((x 1)) () ((loc-box-clr 0)))))
 (term (1 (uninit ε) ((x 1)) () ())))

;; value
(test-equal
 (step (term (uninit (((ε))) () () (3))))
 (term (3 (((ε))) () () ())))

;; close-lam
(test-equal
 (step (term (uninit ('x 'y ('z ε)) () ((x 'q)) ((lam 3 (0 2) x)))))
 (term ((clos x1) ('x 'y ('z ε)) ((x1 ((clos 3 ('x 'z) x)))) ((x 'q)) ())))

;; close-case-lam
(test-equal
 (step (term (uninit ('x 'y ('z ε)) ((x1 'a)) ((x2 'b) (x3 'c)) ((case-lam (lam 3 (0 2) x2) (lam 2 (1 0) x3))))))
 (term ((clos x4) ('x 'y ('z ε)) ((x4 ((clos 3 ('x 'z) x2) (clos 2 ('y 'x) x3))) (x1 'a)) ((x2 'b) (x3 'c)) ())))

;; let-one
(test-equal
 (step (term (uninit ('x ε) () () ((let-one 'y 'z)))))
 (term (uninit (uninit 'x ε) () () (framepush 'y framepop (set 0) 'z))))

;; framepop
(test-equal
 (step (term (uninit ('u ('w ('x 'y ('z ε)))) () () (framepop))))
 (term (uninit ('z ε) () () ())))

;; framepush
(test-equal
 (step (term (7 ('x uninit ('y ε)) () () (framepush))))
 (term (7 (((('x uninit ('y ε))))) () () ())))

;; set
(test-equal
 (step (term ('z ('x ('y ε)) () () ((set 1)))))
 (term ('z ('x ('z ε)) () () ())))

;; set-box
(test-equal
 (step (term ('x ((box y) ε) ((y 'z)) () ((set-box 0)))))
 (term ('x ((box y) ε) ((y 'x)) () ())))

;; boxenv
(test-equal
 (step (term (uninit (9 ε) ((y 8)) () ((boxenv 0 'z)))))
 (term (uninit ((box x) ε) ((x 9) (y 8)) () ('z))))

;; let-void
(test-equal
 (step (term (uninit ('x 'y ε) () () ((let-void 3 'z)))))
 (term (uninit (uninit uninit uninit 'x 'y ε) () () ('z))))

;; let-void-box
(test-predicate
 (redex-match
  runtime
  (uninit 
   ((box variable_1) (box variable_2) (box x) ε)
   ((variable_1 undefined) (variable_2 undefined) (x 'y))
   ()
   ('z)))
 (step (term (uninit ((box x) ε) ((x 'y)) () ((let-void-box 2 'z))))))

;; install-value
(test-->
 ->
 (term (uninit (uninit ε) () () ((install-value 0 'r 'b) 'q)))
 (term (uninit (uninit ε) () () (framepush 'r framepop (set 0) 'b 'q))))
(test-->
 ->
 (term (uninit (uninit ε) () () ((install-value-box 0 'r 'b) 'q)))
 (term (uninit (uninit ε) () () (framepush 'r framepop (set-box 0) 'b 'q))))

;; seq-many
(test-equal
 (step (term (uninit (((ε))) () () ((seq 'x 'y 'z) 'w))))
 (term (uninit (((ε))) () () (framepush 'x framepop (seq 'y 'z) 'w))))

;; seq-one
(test-equal
 (step (term (uninit ((((ε)))) () () ((seq 'x 'y) 'z))))
 (term (uninit ((((ε)))) () () (framepush 'x framepop 'y 'z))))

;; branch
(let ([test-branch (λ (cond res)
                     (test-->>
                      ->
                      `(uninit 
                        ('t 'f ε)
                        ()
                        ()
                        ((branch (let-one 'q ,cond)
                                 (loc 0)
                                 (loc 1))))
                      `(',res ('t 'f ε) () () ())))])
  (test-branch ''not-false 't)
  (test-branch #f 'f))

;; let-rec
(test-->
 ->
 (term
  ('x
   (uninit uninit (box x1) ε)
   ((x1 'x))
   ((x2 'f) (x3 'g))
   ((let-rec
     ((lam 1 (1 2) x2)
      (lam 1 (0 2) x3))
     (loc-noclr 0)))))
 (term
  ('x
   ((clos x4) (clos x5) (box x1) ε)
   ((x1 'x)
    (x4 ((clos 1 ((clos x5) (box x1)) x2)))
    (x5 ((clos 1 ((clos x4) (box x1)) x3))))
   ((x2 'f) (x3 'g))
   ((loc-noclr 0)))))

(test-->
 ->
 (term
  (uninit
   (uninit ((ε)))
   ((x1 ((clos 0 () x2))))
   ((x2 'x) (x3 'y))
   ((let-rec ((lam 0 (0) x3)) 'z))))
 (term
  (uninit
   ((clos x4) ((ε)))
   ((x1 ((clos 0 () x2))) (x4 ((clos 0 ((clos x4)) x3))))
   ((x2 'x) (x3 'y))
   ('z))))

(test-->> 
 ->
 #:cycles-ok
 (term
  (uninit
   (((ε)))
   ()
   ((x1 (application (loc-noclr 0)))
    (x2 (application (loc-noclr 0))))
   ((let-void
     2
     (let-rec ((lam 0 (1) x1)
               (lam 0 (0) x2))
              (application (loc-noclr 1))))))))

;; indirect
(test-->> 
 ->
 #:cycles-ok
 (term
  (uninit
   (((ε)))
   ((x1 ((clos 0 () x2))))
   ((x2 (application (indirect x3)))
    (x3 (clos x1)))
   ((application (indirect x3))))))

;; loops
(test-->>
 ->
 #:cycles-ok
 (term
  (uninit
   (((ε)))
   ()
   ((x1 (application
         (loc-noclr 1)
         (loc-noclr 1)))
    (x2 (let-one
         7
         (application
          (let-one 8 (loc-noclr 3))
          (let-one 9 (loc-noclr 3))))))
   ((application (lam 1 () x1) (lam 1 () x2))))))

(test-->>
 ->
 #:cycles-ok
 (term (load (let-one (indirect x57042) (application (loc-noclr 1) (loc-noclr 1)))
             ((x57042 (proc-const (val) (application (loc-noclr 1) (loc-noclr 1))))))))

(test-->>
 ->
 #:cycles-ok
 (term (load (let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0)))) (application (loc-noclr 0)))) ())))

; mutable variables
(test-->>
 ->
 `(uninit (((ε))) () 
          ()
          ((let-void
            1
            (install-value 0 777 (boxenv 0 (install-value-box 0 888 (loc-box-noclr 0)))))))
 (term (888 ((box x) ((ε))) ((x 888)) () ())))

(test-->>
 ->
 `(uninit (((ε))) () ()
          ((let-one
            (let-void
             1
             (install-value
              0
              'foo
              (boxenv
               0
               (let-one
                (install-value-box 1 7 void)
                (seq (loc-clr 0) (let-one (loc-box-noclr 2) (loc-noclr 0)))))))
            (loc-noclr 0))))
 (term (7 (7 ((ε))) ((x 7)) () ())))

;; locals pushed for seq sub-exprs should be popped
(test-->>
 ->
 (term (uninit (((ε))) () () ((seq (let-one 1 (loc 0)) (let-one 2 (loc 0)) 3))))
 (term (3 (((ε))) () () ())))

; closure-captured value is above explicit arguments
(test-equal
 (caar
  (apply-reduction-relation*
   ->
   (term (load (application
                (let-void
                 1
                 (install-value
                  0
                  777
                  (boxenv
                   0
                   (lam
                    (val val val)
                    (0)
                    (install-value-box 0 (loc-noclr 3) (loc-box-noclr 0))))))
                111
                222
                333)
               ()))))
 333)

;; ref arg
(test-->>
 ->
 #:cycles-ok
 (term (uninit (((ε))) () ((x1 (loc-box-noclr 0))) ((let-one 'x (boxenv 0 (application (lam 1 () x1) (loc-noclr 1)))))))
 (term ('x ((((box x2) ε))) ((x3 ((clos 1 () x1))) (x2 'x)) ((x1 (loc-box-noclr 0))) ())))

;; case-lam
(test-equal
 (map
  car
  (apply-reduction-relation* 
   ->
   (term 
    (load (application 
           (let-one 'a 
                    (let-one 'b
                             (let-one 'c
                                      (case-lam (lam () (2) (loc-noclr 0))
                                                (lam (val) (1) (loc-noclr 0))
                                                (lam (val) (0) (loc-noclr 0))))))
           'x)
          ()))))
 '('b))

(test-equal
 (apply-reduction-relation* 
  ->
  (term (load (application (case-lam (lam (val) () 1) (lam (val val) () 2))) ())))
 '(error))

(test-equal
 (apply-reduction-relation* 
  ->
  (term (load (application (case-lam)) ())))
 '(error))

(test-results)
