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
 (tc · · ((λ (var:x : Num) (+ var:x 5)) 3) t)
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
                    (λ (var:x : Num) (+ var:x 2)))
                 tag_0))
 #f)
(test-equal
 (term (no-match (% hole
                    tag_0
                    (λ (var:x : Num) (+ var:x 2)))
                 tag_1))
 #t)

;; evaluation tests

(check-equal? (abort-eval (term (+ 1 2)))
              3)
(check-equal? (abort-eval (term (λ (var:x : Num) #t)))
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
 (term ((μ (var:f : (→ Num Num))
           (λ (var:n : Num)
             (if (zero? var:n)
                 1
                 (* var:n (var:f (- var:n 1))))))
        5))
 (term 120))

;; list recursion
(do-test
 (term ((μ (var:f : (→ (List Num) Num))
           (λ (var:lst : (List Num))
             (case var:lst
               (null = 0)
               ((cons var:x1 var:x2) =
                             (+ var:x1 (var:f var:x2))))))
        (cons 5 (cons 6 (null Num)))))
 (term 11))

;; list contract
(do-test
 (term (monitor (list/c (flat (λ (var:x : Num) (zero? var:x))))
                (cons 0 (cons 6 (null Num)))
                "pos" "neg" "con"))
 (term (ctc-error "pos" "con")))

;; no control effect
(do-test
 (term (% 5 (make-prompt-tag Num Num) (λ (var:x : Num) var:x)))
 (term 5))

;; test basic abort & handle
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (abort Num var:pt 5)
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term 6))

;; abort past a prompt
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (% (abort Num var:pt 5)
               (make-prompt-tag Num Num)
               (λ (var:x : Num) (+ var:x 2)))
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term 6))

;; abort to innermost prompt
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (% (abort Num var:pt 5)
               var:pt
               (λ (var:x : Num) (+ var:x 2)))
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term 7))

;; composable continuations
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (+ 1 (call/comp
                  (λ (var:kont : (→ Num Num))
                    (+ (var:kont 3) (var:kont 5)))
                  var:pt))
            var:pt
            (λ (var:x : Num) var:x))))
 (term 11))

(do-test
 (term (let ([(var:pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/comp
                  (λ (var:kont : (→ Num Num))
                    (abort Num var:pt
                           (λ (var:x : Unit)
                             (+ (var:kont 3) (var:kont 5)))))
                  var:pt))
            var:pt
            (λ (var:kont : (→ Unit Num)) (var:kont unit)))))
 (term 10))

;; call/cc encoding
(do-test
 (term (let ([(var:pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/cc
                  (λ (var:kont : (→ Num Num))
                    (+ (var:kont 3) 
                       (var:kont 5)))
                  var:pt))
            var:pt
            (λ (var:kont : (→ Unit Num)) (var:kont unit)))))
 (term 4))

;; handler destroys call/cc semantics
(do-test
 (term (let ([(var:pt : (Prompt (→ Unit Num) Num))
              (make-prompt-tag (→ Unit Num) Num)])
         (% (+ 1 (call/cc
                  (λ (var:kon : (→ Num Num))
                    (+ (var:kon 3) (var:kon 5)))
                  var:pt))
            var:pt
            (λ (var:kon : (→ Unit Num)) 18))))
 (term 18))

;; continuation marks
(do-test
 (term (let ([(var:mk : (Mark Num))
              (make-cm-key Num)])
         (call/cm
          var:mk 5
          (ccm var:mk Num))))
 (term (cons 5 (null Num))))

(do-test
 (term (let ([(var:mk : (Mark Num))
              (make-cm-key Num)])
         (call/cm
          var:mk 5
          (call/cm
           var:mk 7
           (ccm var:mk Num)))))
 (term (cons 7 (null Num))))

;; make sure wcms merge in weird cases
(do-test
 (term ((λ (var:f : (→ Unit (List Num)))
          (wcm ((key0 5) ·) (var:f unit)))
        (λ (var:x : Unit)
          (wcm ((key0 3) ·) (ccm key0 Num)))))
 (term (cons 3 (null Num)))
 #:init-store (term (key0 : (Mark Num) ·))
 #:store-type (key0 : (Mark Num) ·))

;; continuation mark contracts
(do-test
 (term (let ([(var:mk : (Mark Num))
              (monitor (mark/c (flat (λ (var:x : Num) (zero? var:x))) Num)
                       (make-cm-key Num)
                       "pos"
                       "neg"
                       "con")])
         (call/cm
          var:mk 5
          (ccm var:mk Num))))
 (term (ctc-error "neg" "con")))

(do-test
 (term (let ([(var:mk : (Mark Num))
              (monitor (mark/c (flat (λ (var:x : Num) (zero? var:x))) Num)
                       (make-cm-key Num)
                       "pos"
                       "neg"
                       "con")])
         (call/cm
          var:mk 0
          (ccm var:mk Num))))
 (term (cons 0 (null Num))))

;; naive contract
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (% (monitor (flat (λ (var:x : Num) (number? var:x)))
                     (abort Num var:pt 5)
                     "pos"
                     "neg"
                     "con")
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term 6))

;; first-order checks
(do-test
 (term (monitor (flat (λ (var:x : Num) (zero? var:x)))
                5
                "server"
                "client"
                "con"))
 (term (ctc-error "server" "con")))

;; prompt & abort in the same component, the tag elsewhere
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (monitor (prompt-tag/c (flat (λ (var:x : Num) (zero? var:x)))
                                     (flat (λ (var:x : Num) (zero? var:x)))
                                     Num Num)
                       (make-prompt-tag Num Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num var:pt 3)
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term (ctc-error "client" "con")))

;; call/comp issue
(do-test
 (term (let ([(var:pt : (Prompt Num Num))
              (monitor (prompt-tag/c (flat (λ (var:x : Num) (zero? var:x)))
                                     (flat (λ (var:x : Num) (zero? var:x)))
                                     Num Num)
                       (make-prompt-tag Num Num)
                       "server"
                       "client"
                       "con")])
         (% (+ 1
               (call/comp
                (λ (var:k : (→ Num Num))
                  (var:k 0))
                var:pt))
            var:pt
            (λ (var:x : Num) (+ var:x 1)))))
 (term (ctc-error "client" "con")))

;; blame even on one side
(do-test
 (term (let ([(var:pt1 : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (let ([(var:pt2 : (Prompt Num Num))
                (monitor (prompt-tag/c (flat (λ (var:x : Num) (zero? var:x)))
                                       (flat (λ (var:x : Num) (zero? (- var:x 5))))
                                       Num Num)
                         var:pt1
                         "client"
                         "server"
                         "con")])
           (% (+ 1 ; doesn't add to 5
                 (call/comp
                  (λ (var:k : (→ Num Num))
                    (var:k 3))
                  var:pt1))
              var:pt2
              (λ (var:x : Num) (+ var:x 1))))))
 (term (ctc-error "server" "con")))

;; blame even on other side
(do-test
 (term (let ([(var:pt1 : (Prompt Num Num))
              (make-prompt-tag Num Num)])
         (let ([(var:pt2 : (Prompt Num Num))
                (monitor (prompt-tag/c (flat (λ (var:x : Num) (zero? var:x)))
                                       (flat (λ (var:x : Num) (zero? (- var:x 5))))
                                       Num Num)
                         var:pt1
                         "server"
                         "client"
                         "con")])
           (% (+ 1 ; doesn't add to 5
                 (call/comp
                  (λ (var:k : (→ Num Num))
                    (var:k 3))
                  var:pt2))
              var:pt1
              (λ (var:x : Num) (+ var:x 1))))))
 (term (ctc-error "server" "con")))

;; same with ho-contract
(do-test
 (term (let ([(var:pt : (Prompt (→ Num Num) Num))
              (monitor (prompt-tag/c (-> (flat (λ (var:x : Num) (zero? var:x)))
                                         (flat (λ (var:x : Num) (number? var:x))))
                                     (flat (λ (var:x : Num) (number? var:x)))
                                     (→ Num Num) Num)
                       (make-prompt-tag (→ Num Num) Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num var:pt (λ (var:x : Num) 5))
            var:pt
            (λ (var:x : (→ Num Num)) (var:x 8)))))
 (term (ctc-error "client" "con")))

;; again, but from other side
(do-test
 (term (let ([(var:pt : (Prompt (→ Num Num) Num))
              (monitor (prompt-tag/c (-> (flat (λ (var:x : Num) (zero? var:x)))
                                         (flat (λ (var:x : Num) (zero? var:x))))
                                     (flat (λ (var:x : Num) (zero? var:x)))
                                     (→ Num Num) Num)
                       (make-prompt-tag (→ Num Num) Num)
                       "server"
                       "client"
                       "con")])
         (% (abort Num var:pt (λ (var:x : Num) 3))
            var:pt
            (λ (var:f : (→ Num Num)) (var:f 0)))))
 (term (ctc-error "client" "con")))

;; abort across boundary w/ ho-value
(do-test
 (term (let ([(var:do-prompt : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
              (let ([(var:pt : (Prompt (→ Num Num) Num))
                     (make-prompt-tag (→ Num Num) Num)])
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (var:x : Num) (zero? var:x)))
                                                   (flat (λ (var:x : Num) (zero? var:x))))
                                               (flat (λ (var:x : Num) (zero? var:x)))
                                               (→ Num Num) Num)
                                 (flat (λ (var:x : Num) (number? var:x))))
                             (flat (λ (var:x : Num) (number? var:x))))
                         (λ (var:f : (→ (Prompt (→ Num Num) Num) Num))
                           (% (var:f var:pt)
                              var:pt
                              (λ (var:f : (→ Num Num)) (var:f 5))))
                         "server"
                         "client"
                         "con"))])
         (var:do-prompt
          (λ (var:pt : (Prompt (→ Num Num) Num))
            (abort Num var:pt (λ (var:v : Num) (+ var:v 1)))))))
 (term (ctc-error "server" "con"))) ;; MF: nice example but in a paper presentation you need to simplify

;; where the prompt flows across multiple boundaries
(do-test
 (term (let ([(var:do-prompt : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
              (let ([(var:pt : (Prompt (→ Num Num) Num))
                     (make-prompt-tag (→ Num Num) Num)])
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (var:x : Num) (number? var:x)))
                                                   (flat (λ (var:x : Num) (number? var:x))))
                                               (flat (λ (var:x : Num) (number? var:x)))
                                               (→ Num Num) Num)
                                 (flat (λ (var:x : Num) (number? var:x))))
                             (flat (λ (var:x : Num) (number? var:x))))
                         (λ (var:f : (→ (Prompt (→ Num Num) Num) Num))
                           (% (var:f var:pt)
                              var:pt
                              (λ (var:f : (→ Num Num)) (var:f 1))))
                         "A"
                         "B"
                         "con1"))])
         (let ([(var:do-prompt-2 : (→ (→ (Prompt (→ Num Num) Num) Num) Num))
                (monitor (-> (-> (prompt-tag/c (-> (flat (λ (var:x : Num) (zero? var:x)))
                                                   (flat (λ (var:x : Num) (number? var:x))))
                                               (flat (λ (var:x : Num) (number? var:x)))
                                               (→ Num Num) Num)
                                 (flat (λ (var:x : Num) (number? var:x))))
                             (flat (λ (var:x : Num) (number? var:x))))
                         (λ (var:f : (→ (Prompt (→ Num Num) Num) Num))
                           (var:do-prompt var:f))
                         "B"
                         "C"
                         "con2")])
           (var:do-prompt-2
            (λ (var:pt : (Prompt (→ Num Num) Num))
              (abort Num var:pt (λ (var:v : Num) (+ var:v 1))))))))
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
 (term (subst (make-cm-key Bool) var:x 5))
 (term (make-cm-key Bool)))

(test-equal
 (term (subst (null Bool) var:x 5))
 (term (null Bool)))

(test-equal
 (abort-eval (term (ccm (make-cm-key Bool) Bool)))
 'null)

(test-equal
 (abort-eval (term (μ (var:B : Num) var:B)))
 'non-terminating)

(test-equal
 (abort-eval (term unit))
 'unit)

(test-equal
 (abort-eval
  (term (call/comp
         (λ (var:Dp : (→ (Mark Bool) Num)) (make-cm-key Bool))
         (make-prompt-tag (Mark (Con Num)) Num))))
 'missing-prompt)

(test-equal
 (term (no-match (call/comp 
                  (λ (var:K : (→ (Prompt (Mark Num) Bool) 
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
                     ((wcm · (λ (var:C : Bool) #f)) (unit? 3))) 
            tag))
 #t)

(test-equal
 (abort-eval
  (term
   (number?
    (μ
     (var:i : Num)
     (-
      (+ (case 
             (null (Con (Con Bool))) 
           (null = 1) 
           ((cons var:X var:u) = 2)) (if #t 0 0))
      var:i)))))
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
      (λ (var:Sa : (→ Num (Prompt (→ Num Unit) (Con Bool)))) 0)
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
       (flat (λ (var:G : Unit) #t))
       (mark/c
        (list/c (mark/c (flat (λ (var:b : Unit) #t)) Unit))
        (List (Mark Unit)))
       tag
       "B"
       "iF"
       "CgXohMerymUWF"))
     ·)
    (monitor
     (flat (λ (var:Mk : Unit) #t))
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
       (mark/c (mark/c (flat (λ (var:r : Num) #f)) Num) (Mark Num))
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