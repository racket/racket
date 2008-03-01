;;; test-list-stack.scm  --  Jens Axel Søgaard  --  dec 10th 2005

(require "../list-stack.scm"
         "srfi-check.scm"
         (lib "42.ss" "srfi"))

; empty?
(check (empty? empty) => #t)
(check (empty? (insert 1 empty)) => #f)

; first
(check (first (insert 1 empty)) => 1)

; first+remove
(check (let-values  ([(f r) (first+remove (insert* (list 1 2 3) empty))])
         (list f (elements r)))
       => (list 3 (list 2 1)))

; fold
(check (fold + 0 (insert* (list 1 2 3) empty)) => 6)
(check (fold cons '() (insert* (list 1 2 3) empty)) => (list 1 2 3))

; insert, elements 
(check (elements (insert 1 empty)) => (list 1))
(check (elements (insert 2 empty)) => (list 2))
(check (elements (insert 1 (insert 2 empty))) => (list 1 2))
(check (elements (insert 1 (insert 2 (insert 3 empty)))) => (list 1 2 3))

; insert*
(check (elements (insert* '() empty)) => (list))
(check (elements (insert* (list 1) empty)) => (list 1))
(check (elements (insert* (list 1 2) empty)) => (list 2 1))
(check (elements (insert* (list 1 2 3) empty)) => (list 3 2 1))

; remove = remove-first
(check (elements (remove (insert 1 empty))) => '())
(check (elements (remove (insert 2 empty))) => '())
(check (elements (remove (insert 1 (insert 2 empty)))) => (list 2))
(check (elements (remove (insert* (list 1 2 3 4 5 ) empty))) => (list 4 3 2 1))


; remove-first
(check (elements (remove-first (insert 1 empty))) => '())
(check (elements (remove-first (insert 2 empty))) => '())
(check (elements (remove-first (insert 1 (insert 2 empty)))) => (list 2))
(check (elements (remove-first (insert* (list 1 2 3 4 5 ) empty))) => (list 4 3 2 1))

; size
(check (size empty) => 0)
(check (size (insert* (list 1) empty)) => 1)
(check (size (insert* (list 1 2) empty)) => 2)
(check (size (insert* (list 1 2 3) empty)) => 3)

; stack?
(check (stack? empty) => #t)
(check (stack? (insert 1 empty)) => #t)
(check (stack? '()) => #f)
(check (stack? '(1)) => #f)

; stack-ec
(check (elements (stack-ec (: i 3) i))
       => (list 2 1 0))

; :stack and : 
(check (list-ec (:stack x (insert* (list 1 2 3) empty)) x)
       => (list 3 2 1))
(check (list-ec (: x (insert* (list 1 2 3) empty)) x)
       => (list 3 2 1))

;;SYNONYMS
;(rename insert push)

(check-report)
