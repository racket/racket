#lang racket

(require redex/reduction-semantics
         (for-syntax syntax/parse)
         "delim-cont.rkt"
         (only-in rackunit 
                  check-equal?
                  check-false
                  check-true))

(judgment-holds
 (tc · · (+ 1 2) t)
 t)
(judgment-holds
 (tc · · ((λ (x : Num) (+ x 5)) 3) t)
 t)
(judgment-holds
 (tc · · (if (zero? 0) #t #f) t)
 t)
(judgment-holds
 (tc · ·
     ((λ (pt : (Prompt Num Num))
        (* (% (+ 1 (abort Num pt 5))
              pt
              (λ (x : Num) (+ x 5)))
           3))
      (make-prompt-tag Num Num))
     t)
 t)
(judgment-holds
 (tc · · (flat (λ (x : Num) (zero? x))) t)
 t)
(judgment-holds
 (tc ·
     (tag0 : (Prompt Num Num) ·)
     (monitor (prompt-tag/c
               (flat (λ (x : Num) (zero? x)))
               (flat (λ (x : Num) (zero? x)))
               Num Num)
              tag0
              "pos"
              "neg"
              "con")
     t)
 t)


(test-equal
 (term (no-match (wcm ((key_0 0) ·) hole) tag_0))
 #t)
(test-equal
 (term (no-match (wcm · hole) tag_0))
 #t)
(test-equal
 (term (no-match (abort Bool #t hole) tag_0))
 #t)
(test-equal (term (no-match hole tag_0)) #t)
(test-equal
 (term (no-match (% hole
                    tag_0 
                    (λ (x : Num) (+ x 2)))
                 tag_0))
 #f)
(test-equal
 (term (no-match (% hole
                    tag_0
                    (λ (x : Num) (+ x 2)))
                 tag_1))
 #t)

;; evaluation tests

(check-equal? (abort-eval (term (+ 1 2)))
              3)
(check-equal? (abort-eval (term (λ (x : Num) #t)))
              'procedure)
(check-equal? (abort-eval (term (make-prompt-tag Num Bool)))
              'prompt-tag)

;; helper for tests
(define-syntax (do-test stx)
  (syntax-parse stx
                [(_ ?input ?expected)
                 #'(do-test ?input ?expected
                            #:init-store (term ·)
                            #:store-type ·)]
                [(_ ?input ?expected #:init-store ?store #:store-type ?store-type)
                 #'(begin
                     (check-true (judgment-holds (tc · ?store-type ,?input t)))
                     (check-equal? (abort-eval ?input #:init-store ?store)
                                   ?expected))]))

;; eval and type checking tests
;; recursion
(do-test
 (term ((μ (f : (→ Num Num))
           (λ (n : Num)
             (if (zero? n)
                 1
                 (* n (f (- n 1))))))
        5))
 (term 120))

;; list recursion
(do-test
 (term ((μ (f : (→ (List Num) Num))
           (λ (lst : (List Num))
             (case lst
               (null = 0)
               ((cons x1 x2) =
                             (+ x1 (f x2))))))
        (cons 5 (cons 6 (null Num)))))
 (term 11))

;; list contract
(do-test
 (term (monitor (list/c (flat (λ (x : Num) (zero? x))))
                (cons 0 (cons 6 (null Num)))
                "pos" "neg" "con"))
 (term (ctc-error "pos" "con")))

;; no control effect
(do-test
 (term (% 5 (make-prompt-tag Num Num) (λ (x : Num) x)))
 (term 5))

;; test basic abort & handle
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (abort Num pt 5)
            pt
            (λ (x : Num) (+ x 1)))))
 (term 6))

;; abort past a prompt
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (% (abort Num pt 5)
               (make-prompt-tag Num Num)
               (λ (x : Num) (+ x 2)))
            pt
            (λ (x : Num) (+ x 1)))))
 (term 6))

;; abort to innermost prompt
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (% (abort Num pt 5)
               pt
               (λ (x : Num) (+ x 2)))
            pt
            (λ (x : Num) (+ x 1)))))
 (term 7))

;; composable continuations
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (+ 1 (call/comp
                  (λ (kont : (→ Num Num))
                    (+ (kont 3) (kont 5)))
                  pt))
            pt
            (λ (x : Num) x))))
 (term 11))

(do-test
 (term (let ([(pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/comp
                  (λ (kont : (→ Num Num))
                    (abort Num pt
                           (λ (x : Unit)
                             (+ (kont 3) (kont 5)))))
                  pt))
            pt
            (λ (kont : (→ Unit Num)) (kont unit)))))
 (term 10))

;; call/cc encoding
(do-test
 (term (let ([(pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/cc
                  (λ (kont : (→ Num Num))
                    (+ (kont 3) (kont 5)))
                  pt))
            pt
            (λ (kont : (→ Unit Num)) (kont unit)))))
 (term 4))

;; handler destroys call/cc semantics
(do-test
 (term (let ([(pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/cc
                  (λ (kont : (→ Num Num))
                    (+ (kont 3) (kont 5)))
                  pt))
            pt
            (λ (kont : (→ Unit Num)) 18))))
 (term 18))

;; continuation marks
(do-test
 (term (let ([(mk : (Mark Num))
              (make-cm-key Num)])
         (call/cm
          mk 5
          (ccm mk Num))))
 (term (cons 5 (null Num))))

(do-test
 (term (let ([(mk : (Mark Num))
              (make-cm-key Num)])
         (call/cm
          mk 5
          (call/cm
           mk 7
           (ccm mk Num)))))
 (term (cons 7 (null Num))))

;; make sure wcms merge in weird cases
(do-test
 (term ((λ (f : (→ Unit (List Num)))
          (wcm ((key0 5) ·) (f unit)))
        (λ (x : Unit)
          (wcm ((key0 3) ·) (ccm key0 Num)))))
 (term (cons 3 (null Num)))
 #:init-store (term (key0 : (Mark Num) ·))
 #:store-type (key0 : (Mark Num) ·))

;; continuation mark contracts
(do-test
 (term (let ([(mk : (Mark Num))
              (monitor (mark/c (flat (λ (x : Num) (zero? x))) Num)
                       (make-cm-key Num)
                       "pos"
                       "neg"
                       "con")])
         (call/cm
          mk 5
          (ccm mk Num))))
 (term (ctc-error "neg" "con")))

(do-test
 (term (let ([(mk : (Mark Num))
              (monitor (mark/c (flat (λ (x : Num) (zero? x))) Num)
                       (make-cm-key Num)
                       "pos"
                       "neg"
                       "con")])
         (call/cm
          mk 0
          (ccm mk Num))))
 (term (cons 0 (null Num))))

;; naive contract
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (monitor (flat (λ (x : Num) (number? x)))
                     (abort Num pt 5)
                     "pos"
                     "neg"
                     "con")
            pt
            (λ (x : Num) (+ x 1)))))
 (term 6))

;; first-order checks
(do-test
 (term (monitor (flat (λ (x : Num) (zero? x)))
                5
                "server"
                "client"
                "con"))
 (term (ctc-error "server" "con")))

;; prompt & abort in the same component, the tag elsewhere
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (monitor (prompt-tag/c (flat (λ (x : Num) (zero? x)))
                                     (flat (λ (x : Num) (zero? x)))
                                     Num Num)
                       (make-prompt-tag Num Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num pt 3)
            pt
            (λ (x : Num) (+ x 1)))))
 (term (ctc-error "client" "con")))

;; call/comp issue
(do-test
 (term (let ([(pt : (Prompt Num Num))
              (monitor (prompt-tag/c (flat (λ (x : Num) (zero? x)))
                                     (flat (λ (x : Num) (zero? x)))
                                     Num Num)
                       (make-prompt-tag Num Num)
                       "server"
                       "client"
                       "con")])
         (% (+ 1
               (call/comp
                (λ (k : (→ Num Num))
                  (k 0))
                pt))
            pt
            (λ (x : Num) (+ x 1)))))
 (term (ctc-error "client" "con")))

;; blame even on one side
(do-test
 (term (let ([(pt1 : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (let ([(pt2 : (Prompt Num Num))
                (monitor (prompt-tag/c (flat (λ (x : Num) (zero? x)))
                                       (flat (λ (x : Num) (zero? (- x 5))))
                                       Num Num)
                         pt1
                         "client"
                         "server"
                         "con")])
           (% (+ 1 ; doesn't add to 5
                 (call/comp
                  (λ (k : (→ Num Num))
                    (k 3))
                  pt1))
              pt2
              (λ (x : Num) (+ x 1))))))
 (term (ctc-error "server" "con")))

;; blame even on other side
(do-test
 (term (let ([(pt1 : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (let ([(pt2 : (Prompt Num Num))
                (monitor (prompt-tag/c (flat (λ (x : Num) (zero? x)))
                                       (flat (λ (x : Num) (zero? (- x 5))))
                                       Num Num)
                         pt1
                         "server"
                         "client"
                         "con")])
           (% (+ 1 ; doesn't add to 5
                 (call/comp
                  (λ (k : (→ Num Num))
                    (k 3))
                  pt2))
              pt1
              (λ (x : Num) (+ x 1))))))
 (term (ctc-error "server" "con")))

;; same with ho-contract
(do-test
 (term (let ([(pt : (Prompt (→ Num Num) Num))
              (monitor (prompt-tag/c (-> (flat (λ (x : Num) (zero? x)))
                                         (flat (λ (x : Num) (number? x))))
                                     (flat (λ (x : Num) (number? x)))
                                     (→ Num Num) Num)
                       (make-prompt-tag (→ Num Num) Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num pt (λ (x : Num) 5))
            pt
            (λ (x : (→ Num Num)) (x 8)))))
 (term (ctc-error "client" "con")))

;; again, but from other side
(do-test
 (term (let ([(pt : (Prompt (→ Num Num) Num))
              (monitor (prompt-tag/c (-> (flat (λ (x : Num) (zero? x)))
                                         (flat (λ (x : Num) (zero? x))))
                                     (flat (λ (x : Num) (zero? x)))
                                     (→ Num Num) Num)
                       (make-prompt-tag (→ Num Num) Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num pt (λ (x : Num) 3))
            pt
            (λ (f : (→ Num Num)) (f 0)))))
 (term (ctc-error "client" "con")))

;; abort across boundary w/ ho-value
(do-test
 (term (let ([(do-prompt : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
              (let ([(pt : (Prompt (→ Num Num) Num))
                     (make-prompt-tag (→ Num Num) Num)])
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (x : Num) (zero? x)))
                                                   (flat (λ (x : Num) (zero? x))))
                                               (flat (λ (x : Num) (zero? x)))
                                               (→ Num Num) Num)
                                 (flat (λ (x : Num) (number? x))))
                             (flat (λ (x : Num) (number? x))))
                         (λ (f : (→ (Prompt (→ Num Num) Num) Num))
                           (% (f pt)
                              pt
                              (λ (f : (→ Num Num)) (f 5))))
                         "server"
                         "client"
                         "con"))])
         (do-prompt
          (λ (pt : (Prompt (→ Num Num) Num))
            (abort Num pt (λ (v : Num) (+ v 1)))))))
 (term (ctc-error "server" "con"))) ;; MF: nice example but in a paper presentation you need to simplify

;; where the prompt flows across multiple boundaries
(do-test
 (term (let ([(do-prompt : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
              (let ([(pt : (Prompt (→ Num Num) Num))
                     (make-prompt-tag (→ Num Num) Num)])
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (x : Num) (number? x)))
                                                   (flat (λ (x : Num) (number? x))))
                                               (flat (λ (x : Num) (number? x)))
                                               (→ Num Num) Num)
                                 (flat (λ (x : Num) (number? x))))
                             (flat (λ (x : Num) (number? x))))
                         (λ (f : (→ (Prompt (→ Num Num) Num) Num))
                           (% (f pt)
                              pt
                              (λ (f : (→ Num Num)) (f 1))))
                         "A"
                         "B"
                         "con1"))])
         (let ([(do-prompt-2 : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (x : Num) (zero? x)))
                                                   (flat (λ (x : Num) (number? x))))
                                               (flat (λ (x : Num) (number? x)))
                                               (→ Num Num) Num)
                                 (flat (λ (x : Num) (number? x))))
                             (flat (λ (x : Num) (number? x))))
                         (λ (f : (→ (Prompt (→ Num Num) Num) Num))
                           (do-prompt f))
                         "B"
                         "C"
                         "con2")])
           (do-prompt-2
            (λ (pt : (Prompt (→ Num Num) Num))
              (abort Num pt (λ (v : Num) (+ v 1))))))))
 (term (ctc-error "B" "con2")))

#|
  ;; from random test generation
  (do-test
   (term (boolean?
          (abort (monitor
                  (prompt-tag/c (-> (flat (λ (H) (error)))
                                    (flat (λ (R) (error)))))
                  (prompt-tag v)
                  "pos"
                  "neg")
                 (make-prompt-tag))))
   (term (error)))
   |#


;; tests for bugs found by random generation
(test-equal
 (term (subst (make-cm-key Bool) x 5))
 (term (make-cm-key Bool)))

(test-equal
 (term (subst (null Bool) x 5))
 (term (null Bool)))

(test-equal
 (abort-eval (term (ccm (make-cm-key Bool) Bool)))
 'null)

(test-equal
 (abort-eval (term (μ (B : Num) B)))
 'non-terminating)

(test-equal
 (abort-eval (term unit))
 'unit)

(test-equal
 (abort-eval
  (term (call/comp
         (λ (Dp : (→ (Mark Bool) Num)) (make-cm-key Bool))
         (make-prompt-tag (Mark (Con Num)) Num))))
 'missing-prompt)

(test-equal
 (term (no-match (call/comp 
                  (λ (K : (→ (Prompt (Mark Num) Bool) 
                             (List (Prompt Bool Unit)))) 
                    (make-prompt-tag (Mark Num) Bool)) 
                  (wcm · hole)) tag))
 #t)

(test-equal
 (abort-eval
  (term (abort
         Num
         (make-prompt-tag Bool (List (Mark Bool)))
         (boolean? 2))))
 'missing-prompt)

(test-equal
 (term (marks (call/cm hole (+ 1 2) 1) key1 (null (→ (Mark Num) Num))))
 (term (null (→ (Mark Num) Num))))

(test-equal
 (term
  (marks (if (boolean? hole) 1 2) key (null (→ Num (Mark Unit)))))
 (term (null (→ Num (Mark Unit)))))

(test-equal
 (term 
  (no-match (call/cm key (wcm · hole) 
                     ((wcm · (λ (C : Bool) #f)) (unit? 3))) 
            tag))
 #t)

(test-equal
 (abort-eval
  (term
   (number?
    (μ
     (i : Num)
     (-
      (+ (case 
             (null (Con (Con Bool))) 
           (null = 1) 
           ((cons X u) = 2)) (if #t 0 0))
      i)))))
 'non-terminating)

(test-equal
 (judgment-holds
  (tc
   ·
   (tag2
    :
    (Prompt (Mark (Prompt Unit Num)) (Prompt (→ Num Unit) (Con Bool)))
    (tag1
     :
     (Prompt Num (→ (→ Unit Unit) (Con Unit)))
     (tag
      :
      (Prompt
       (→ (Prompt Num Unit) (→ Bool Unit))
       (Prompt Num (→ (→ Unit Unit) (Con Unit))))
      ·)))
   (abort
    (→ Num (→ Num Num))
    tag1
    (wcm
     ·
     (call/comp
      (λ (Sa : (→ Num (Prompt (→ Num Unit) (Con Bool)))) 0)
      tag2)))
   t)
  t)
 '((→ Num (→ Num Num))))

(test-equal
 (judgment-holds
  (tc 
   ·
   (tag1
    :
    (Prompt Bool (List Num))
    (key1
     :
     (Mark Bool)
     (tag
      :
      (Prompt Unit (Mark (List (Mark Unit))))
      (key : (Mark (Prompt Unit (Mark (List (Mark Unit))))) ·))))
   (wcm
    ((key
      (PG
       (flat (λ (G : Unit) #t))
       (mark/c
        (list/c (mark/c (flat (λ (b : Unit) #t)) Unit))
        (List (Mark Unit)))
       tag
       "B"
       "iF"
       "CgXohMerymUWF"))
     ·)
    (monitor
     (flat (λ (Mk : Unit) #t))
     (abort Unit tag1 #t)
     "fO"
     "clRmiOfXGo"
     "jxeinueLyNmLozqsKl"))
   t)
  t)
 '(Unit))

(test-equal
 (judgment-holds
  (tc · (key : (Mark (Mark (Mark Num))) ·)
      (MG
       (mark/c (mark/c (flat (λ (r : Num) #f)) Num) (Mark Num))
       key
       "EVYdYcpulOg"
       "G"
       "BjUOkycjoz")
      t)
  t)
 '((Mark (Mark (Mark Num)))))




(check-false (term (not-wcm (wcm · hole))))
(check-false (term (not-wcm (+ 5 (wcm · hole)))))
(check-true (term (not-wcm hole)))
(check-true (term (not-wcm (abort Num hole 5))))