#lang scheme

(require "checks.ss"
         "../set.ss")

(provide set-suite)

(define (check/set a-set a-list #:= [== equal?])
  (check/sort (set->list a-set) a-list #:= ==))

(define-syntax-rule (test/set arg ...)
  (test (check/set arg ...)))

(define set-suite
  (test-suite "set.ss"
    (test-suite "Constructors"
      (test-suite "set"
        (test/set (set 1 2 3) (list 1 2 3))
        (test/set (set 3 2 1) (list 1 2 3))
        (test/set (set 3 1 2 #:mutable? #t) (list 1 2 3))
        (test/set (set 3 1 2 #:weak? #t) (list 1 2 3))
        (test/set (set 3 1 2 #:compare 'eqv) (list 1 2 3))
        (test/set (set 3 1 2 #:compare 'eq) (list 1 2 3)))
      (test-suite "empty-set"
        (test/set (empty-set) (list))
        (test/set (empty-set #:mutable? #t) (list))
        (test/set (empty-set #:weak? #t) (list))
        (test/set (empty-set #:compare 'eqv) (list))
        (test/set (empty-set #:compare 'eq) (list)))
      (test-suite "list->set"
        (test/set (list->set (list 1 2 3)) (list 1 2 3))
        (test/set (list->set (list 3 2 1)) (list 1 2 3))
        (test/set (list->set (list 3 1 2) #:mutable? #t) (list 1 2 3))
        (test/set (list->set (list 3 1 2) #:weak? #t) (list 1 2 3))
        (test/set (list->set (list 3 1 2) #:compare 'eqv) (list 1 2 3))
        (test/set (list->set (list 3 1 2) #:compare 'eq) (list 1 2 3)))
      (test-suite "custom-set"
        (test/set (custom-set #:compare string-ci=? "A" "a" "B" "b")
                  (list "A" "B")
                  #:= string-ci=?)
        (test/set (custom-set #:compare string-ci=?
                              #:hash string-length
                              "A" "a" "B" "b")
                  (list "A" "B")
                  #:= string-ci=?)
        (test/set (custom-set #:compare string-ci=?
                              #:hash string-length
                              #:mutable? #t
                              "A" "a" "B" "b")
                  (list "A" "B")
                  #:= string-ci=?)))
    (test-suite "Accessors"
      (test-suite "set-contains?"
        (test (check-true (set-contains? (set 1 2 3) 1)))
        (test (check-false (set-contains? (set 1 2 3) 4))))
      (test-suite "set-empty?"
        (test (check-true (set-empty? (set))))
        (test (check-false (set-empty? (set 1 2 3)))))
      (test-suite "set-count"
        (test (check = (set-count (set)) 0))
        (test (check = (set-count (set 1 2 3)) 3)))
      (test-suite "set=?"
        (test (check-false (set=? (set 1) (set 1 2 3))))
        (test (check-false (set=? (set 1 2 3) (set 1))))
        (test (check-true (set=? (set 1 2 3) (set 1 2 3)))))
      (test-suite "subset?"
        (test (check-true (subset? (set 1) (set 1 2 3))))
        (test (check-false (subset? (set 1 2 3) (set 1))))
        (test (check-true (subset? (set 1 2 3) (set 1 2 3)))))
      (test-suite "proper-subset?"
        (test (check-true (proper-subset? (set 1) (set 1 2 3))))
        (test (check-false (proper-subset? (set 1 2 3) (set 1))))
        (test (check-false (proper-subset? (set 1 2 3) (set 1 2 3)))))
      (test-suite "set->list"
        (test (check/sort (set->list (set 1 2 3)) (list 1 2 3))))
      (test-suite "in-set"
        (test (check/sort (for/list ([x (in-set (set 1 2 3))]) x)
                          (list 1 2 3)))))
    (test-suite "Updaters"
      (test-suite "set-insert"
        (test/set (set-insert (set 1 2 3) 4) (list 1 2 3 4))
        (test/set (set-insert (set 1 2 3) 1) (list 1 2 3)))
      (test-suite "set-remove"
        (test/set (set-remove (set 1 2 3) 1) (list 2 3))
        (test/set (set-remove (set 1 2 3) 4) (list 1 2 3)))
      (test-suite "set-insert!"
        (test (let* ([s (set 1 2 3 #:mutable? #t)])
                (set-insert! s 4)
                (check/set s (list 1 2 3 4))))
        (test (let* ([s (set 1 2 3 #:mutable? #t)])
                (set-insert! s 1)
                (check/set s (list 1 2 3)))))
      (test-suite "set-remove!"
        (test (let* ([s (set 1 2 3 #:mutable? #t)])
                (set-remove! s 1)
                (check/set s (list 2 3))))
        (test (let* ([s (set 1 2 3 #:mutable? #t)])
                (set-remove! s 4)
                (check/set s (list 1 2 3)))))
      (test-suite "set-union"
        (test/set (set-union (set 1 2) (set 1 3) (set 2 3)) (list 1 2 3))
        (test/set (set-union (set) (set 1 2) (set 3 4)) (list 1 2 3 4))
        (test/set (set-union (set 1 2) (set) (set 3 4)) (list 1 2 3 4))
        (test/set (set-union (set 1 2) (set 3 4) (set)) (list 1 2 3 4)))
      (test-suite "set-intersection"
        (test/set (set-intersection (set 1 2 3) (set 1 2) (set 2 3)) (list 2))
        (test/set (set-intersection (set 1 2) (set 1 2 3) (set 2 3)) (list 2))
        (test/set (set-intersection (set 1 2) (set 2 3) (set 1 2 3)) (list 2))
        (test/set (set-intersection (set 1 2) (set 2 3) (set 1 3)) (list)))
      (test-suite "set-difference"
        (test/set (set-difference (set 1 2 3) (set 1) (set 3)) (list 2))
        (test/set (set-difference (set 1 2 3 4) (set 5) (set 6)) (list 1 2 3 4))
        (test/set (set-difference (set 1 2 3) (set 1 2) (set 2 3)) (list)))
      (test-suite "set-exclusive-or"
        (test/set (set-exclusive-or (set 1) (set 1 2) (set 1 2 3)) (list 1 3))
        (test/set (set-exclusive-or (set 1) (set 2) (set 3)) (list 1 2 3))
        (test/set (set-exclusive-or (set 1 2) (set 2 3) (set 1 3)) (list))))
    (test-suite "Predicates"
      (test-suite "set?"
        (test (check-false (set? '(1 2))))
        (test (check-true (set? '((1 . one) (2 . two)))))
        (test (check-true (set? (set 1 2 3)))))
      (test-suite "set-can-insert?"
        (test (check-true (set-can-insert? (set 1 2 3))))
        (test (check-false (set-can-insert? (set 1 2 3 #:mutable? #t)))))
      (test-suite "set-can-remove?"
        (test (check-true (set-can-remove? (set 1 2 3))))
        (test (check-false (set-can-remove? (set 1 2 3 #:mutable? #t)))))
      (test-suite "set-can-insert!?"
        (test (check-false (set-can-insert!? (set 1 2 3))))
        (test (check-true (set-can-insert!? (set 1 2 3 #:mutable? #t)))))
      (test-suite "set-can-remove!?"
        (test (check-false (set-can-remove!? (set 1 2 3))))
        (test (check-true (set-can-remove!? (set 1 2 3 #:mutable? #t))))))
    (test-suite "Property"
      (test-suite "prop:set"
        (test
         (let ()
           (define (never-contains? set elem) #f)
           (define (never-remove! set elem) (void))
           (define (never-remove set elem) set)
           (define (always-zero set) 0)
           (define (no-elements set) null)

           (define-struct always-empty []
             #:transparent
             #:property prop:set
             (vector never-contains?
                     #f
                     #f
                     never-remove!
                     never-remove
                     always-zero
                     no-elements))

           (check-true (set? (make-always-empty)))
           (check/set (make-always-empty) (list))
           (check-false (set-contains? (make-always-empty) 1))
           (check-bad (set-insert! (make-always-empty) 2))
           (check-bad (set-insert (make-always-empty) 3))
           (check/set (let* ([s (make-always-empty)])
                        (set-remove! s 4)
                        s)
                      (list))
           (check/set (set-remove (make-always-empty) 5) (list))
           (check-true (set-empty? (make-always-empty)))
           (check-equal? (set->list (make-always-empty)) (list))))))))


