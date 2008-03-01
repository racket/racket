;;; test-finite-map.scm  --  Jens Axel Søgaard  --  dec 27th 2005

(require "../finite-map.scm"
         "srfi-check.scm"
         (lib "42.ss" "srfi")
         (only (lib "list.ss") mergesort)
         (lib "67.ss" "srfi"))

;;; HELPERS

(define (same? l1 l2)
  (equal? (mergesort l1 <)
          (mergesort l2 <)))

(define (symbol< s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))

(define (same-symbols? l1 l2)
  (equal? (mergesort l1 symbol<)
          (mergesort l2 symbol<)))

(define (compare-mod2 x1 x2)
  (number-compare (modulo x1 2) (modulo x2 2)))

;;; TESTS  

; finite-map?
(check (finite-map? (empty)) => #t)
(check (finite-map? (insert 'a 1 (empty))) => #t)
(check (finite-map? '()) => #f)
(check (finite-map? '(1)) => #f)

; count
(check (count 'a (insert 'a 1 (insert 'a 3 (empty)))) => 1)
(check (count 1 (insert 'a 1 (insert 'a 3 (empty)))) => 0)

; delete
(check (let ([f (delete 'b (insert 'a 1 (insert 'b 2 (empty))))])
         (and (member? 'a f) (not (member? 'b f)))) => #t)
(check (let ([f (delete 'c (insert 'a 1 (insert 'b 2 (empty))))])
         (and (member? 'a f) (member? 'b f) (not (member? 'c f)))) => #t)

; delete-all
(check (keys (delete-all 'b (insert 'a 1 (insert 'b 2 (empty)))))  (=> same-symbols?) (list 'a))
(check (keys (delete-all 'c (insert 'a 1 (insert 'b 2 (empty)))))  (=> same-symbols?) (list 'a 'b))

; delete*
(check (keys (delete* (list 'a 'b) (insert 'a 1 (insert 'b 2 (insert 'c 3 (insert 'd 4 (empty)))))))                              
       (=> same-symbols?) (list 'c 'd))
(check (keys (delete* (list 'e 'f) (insert 'a 1 (insert 'b 2 (insert 'c 3 (insert 'd 4 (empty)))))))                              
       (=> same-symbols?) (list 'a 'b 'c 'd))

; difference
(define fabc   (insert 'a 1 (insert 'b 2 (insert 'c 3 (empty)))))
(define fcd    (insert 'c 3 (insert 'c 4 (empty))))
(define fcde   (insert 'd 5 (insert 'c 3 (insert 'e 4 (empty)))))
(define fabcde (insert 'a 1 (insert 'b 2 (insert 'c 3 (insert 'd 4 (insert 'e 5 (empty)))))))

(check (keys (difference fabc                 fcd))           (=> same-symbols?)  (list 'a 'b))
(check (keys (difference fabc                 (empty)))       (=> same-symbols?)  (list 'a 'b 'c))
(check (keys (difference (empty)              fabc))          (=> same-symbols?)  '())
(check (keys (difference fabcde               fcde))          (=> same-symbols?)  (list 'a 'b))
(check (keys (difference fabc                 (empty)))       (=> same-symbols?)  (list 'a 'b 'c))
(check (keys (difference (empty)              fabc))          (=> same-symbols?)  '())

; empty
(check (elements (empty))  (=> same?)  '())
(check (size (empty))  =>  0)

; empty?
(check (empty? (empty)) => #t)
(check (empty? (insert 'a 1 (empty))) => #f)

; equal=?

(define fabcde (insert 'a 1 (insert 'b 2 (insert 'c 3 (insert 'd 4 (insert 'e 5 (empty)))))))
(define fedcba  (insert 'e 5 (insert 'd 4 (insert 'c 3 (insert 'b 2 (insert 'a 1 (empty)))))))
(define fedcba2 (insert 'e 5 (insert 'd 4 (insert 'c 3 (insert 'b 2 (insert 'a 2 (empty)))))))
(define fa (insert 'a 1 (empty)))
(define fab (insert 'b 2 (insert 'a 1 (empty))))

(check (equal=? fabcde fedcba)   => #t)
(check (equal=? fedcba fedcba2)  => #f)
(check (equal=? (empty) (empty)) => #t)
(check (equal=? (empty) fa)      => #f)
(check (equal=? fa (empty))      => #f)

; fold
(check (fold + 0 (insert* '((a . 1) (b . 2) (c . 3)) (empty))) => 6)
(check (fold cons '() (insert* '((a . 1) (b . 2) (c . 3)) (empty)))
       (=> same?)  (list 1 2 3))

; fold/key
(check (fold/key (lambda (key val acc) (cons key acc))
                 '() (insert* '((a . 1) (b . 2) (c . 3)) (empty)))
       (=> same-symbols?) '(a b c))
(check (fold/key (lambda (key val acc) (+ val acc))
                 0 (insert* '((a . 1) (b . 2) (c . 3)) (empty)))
       => 6)

; get
(check (get 'a (insert 'a 1 (empty))) =>  '(a . 1))
(check (get 'b (insert 'a 1 (empty))) => #f)

; insert, elements 
(check (elements (insert 'a 1 (empty)))                              =>         (list 1))
(check (elements (insert 'b 2 (empty)))                              =>         (list 2))
(check (elements (insert 'a 1 (insert 'b 2 (empty))))                (=> same?)  (list 1 2))
(check (elements (insert 'a 1 (insert 'b 2 (insert 'c 3 (empty)))))  (=> same?)  (list 1 2 3))

; insert*
(check (elements (insert* '() (empty)))                         =>         (list))
(check (elements (insert* '((a . 1)) (empty)))                  =>         (list 1))
(check (elements (insert* '((a . 1) (b . 2)) (empty))      )   (=> same?)  (list 2 1))
(check (elements (insert* '((a . 1) (b . 2) (c . 3)) (empty))) (=> same?)  (list 3 2 1))

; intersection
(check (keys (intersection fabc fcde))        (=> same?)  '(c))

; member?
(check (member? 'a (insert 'a 1 (empty))) => #t)
(check (member? 'b (insert 'a 1 (empty))) => #f)
(check (member? 'a (insert 'b 2 (insert 'a 1 (empty)))) => #t)

; singleton
(check (elements (singleton 'a 1)) => (list 1))
(check (keys (singleton 'a 1)) => (list 'a))
(check (size (singleton 'a 1)) => 1)

; size
(check (size (empty)) => 0)
(check (size fa) => 1)
(check (size fab) => 2)
(check (size fabc) => 3)


; union
(check (keys (union fabc fcde)) (=> same-symbols?)  '(a b c d e))

; select
(check (select fa) => 'a)


;;; REPORT

(check-report)
