(require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 1)))

(require (lib "1.ss" "srfi")
         (lib "69.ss" "srfi"))

(define test-hash-table1
  (alist->hash-table '((a . 1) (b . 2) (c . 3))))
(define test-hash-table2
  (alist->hash-table '(("a" . 1) ("b" . 2) ("c" . 3)) string-ci=? string-ci-hash))


(define test-suite
  (make-test-suite
    "srfi-69 test suit"
    (make-test-case
      "make-hash-table and hash-table?"
      (assert-true
        (hash-table? (make-hash-table))))
    (make-test-case
      "alist->hash-table"
      (assert-true
        (hash-table? test-hash-table1)))
    (make-test-case
      "hash-table-equivalence-function"
      (assert-eq?
        (hash-table-equivalence-function (make-hash-table))
        equal?)
      (assert-eq?
        (hash-table-equivalence-function (make-hash-table eq?))
        eq?)
      (assert-eq?
        (hash-table-equivalence-function test-hash-table2)
        string-ci=?))
    (make-test-case
      "hash-table-hash-function"
      (assert-eq?
        (hash-table-hash-function (make-hash-table))
        hash)
      (assert-eq?
        (hash-table-hash-function (make-hash-table eq?))
        hash-by-identity)
      (assert-eq?
        (hash-table-hash-function test-hash-table2)
        string-ci-hash))
    (make-test-case
      "hash-table-ref"
      (assert-equal?
        (hash-table-ref test-hash-table1 'b)
        2)
      (assert-equal?
        (hash-table-ref test-hash-table2 "C")
        3)
      (assert-false
        (hash-table-ref test-hash-table1 'd (lambda () #f))))
    (make-test-case
      "hash-table-ref/default"
      (assert-false
        (hash-table-ref/default test-hash-table2 "d" #f)))
    (make-test-case
      "hash-table-set!"
      (assert-equal?
        (begin (hash-table-set! test-hash-table1 'c 4)
               (hash-table-ref test-hash-table1 'c))
        4)
      (assert-equal?
        (begin (hash-table-set! test-hash-table2 "d" 4)
               (hash-table-ref test-hash-table2 "D"))
        4))
    (make-test-case
      "hash-table-delete!"
      (assert-false
        (begin (hash-table-delete! test-hash-table2 "D")
               (hash-table-ref/default test-hash-table2 "d" #f))))
    (make-test-case
      "hash-table-exists?"
      (assert-true
        (hash-table-exists? test-hash-table2 "B"))
      (assert-false
        (hash-table-exists? test-hash-table1 'd)))
    (make-test-case
      "hash-table-update!"
      (assert-equal?
        (begin (hash-table-update! test-hash-table1 'c sub1)
               (hash-table-ref test-hash-table1 'c))
        3)
      (assert-equal?
        (begin (hash-table-update! test-hash-table2 "d" add1 (lambda () 3))
               (hash-table-ref test-hash-table2 "d"))
        4))
    (make-test-case
      "hash-table-update!/default"
      (assert-equal?
        (begin (hash-table-update!/default test-hash-table1 'd add1 3)
               (hash-table-ref test-hash-table1 'd))
        4))
    (make-test-case
      "hash-table-size"
      (assert-equal?
        (hash-table-size test-hash-table1)
        4)
      (assert-equal?
        (hash-table-size test-hash-table2)
        4))
    (make-test-case
      "hash-table-keys"
      (assert-true
        (lset= eq?
               (hash-table-keys test-hash-table1)
               '(a b c d)))
      (assert-true
        (lset= equal?
               (hash-table-keys test-hash-table2)
               (list "a" "b" "c" "d"))))
    (make-test-case
      "hash-table-values"
      (assert-true
        (lset= eqv?
               (hash-table-values test-hash-table1)
               '(1 2 3 4)))
      (assert-true
        (lset= eqv?
               (hash-table-values test-hash-table2)
               '(1 2 3 4))))
    (make-test-case
      "hash-table-walk"
      (assert-true
        (let ((a '()))
          (hash-table-walk test-hash-table1
                           (lambda (key value)
                             (set! a (cons (cons key value) a))))
          (lset= equal?
                 a
                 '((a . 1) (b . 2) (c . 3) (d . 4))))))
    (make-test-case
      "hash-table-fold"
      (assert-true
        (lset= equal?
               (hash-table-fold test-hash-table2
                                (lambda (key value accu)
                                  (cons (cons key value) accu))
                                '())
               (list (cons "a" 1)
                     (cons "b" 2)
                     (cons "c" 3)
                     (cons "d" 4)))))
    (make-test-case
      "hash-table->alist"
      (assert-true
        (lset= equal?
               (hash-table->alist test-hash-table1)
               '((a . 1) (b . 2) (c . 3) (d . 4)))))
    (make-test-case
      "hash-table-copy"
      (assert-true
        (lset= equal?
               (hash-table->alist (hash-table-copy test-hash-table2))
               (list (cons "a" 1)
                     (cons "b" 2)
                     (cons "c" 3)
                     (cons "d" 4))))
      (assert-false
        (eq? (hash-table-copy test-hash-table1)
             test-hash-table1))
      (assert-eq?
        (hash-table-equivalence-function
          test-hash-table1)
        (hash-table-equivalence-function
          (hash-table-copy test-hash-table1)))
      (assert-eq?
        (hash-table-hash-function
          test-hash-table2)
        (hash-table-hash-function
          (hash-table-copy test-hash-table2))))
    (make-test-case
      "hash-table->alist"
      (assert-true
        (lset= equal?
               (hash-table->alist
                 (hash-table-merge!  test-hash-table1
                                     test-hash-table2))
               '(("a" . 1)
                 ("b" . 2)
                 ("c" . 3)
                 ("d" . 4)
                 (a . 1)
                 (b . 2)
                 (c . 3)
                 (d . 4))))
      (assert-true
        (lset= equal?
               (hash-table->alist
                 (hash-table-merge!  test-hash-table2
                                     test-hash-table2))
               '(("a" . 1)
                 ("b" . 2)
                 ("c" . 3)
                 ("d" . 4)))))))

(test/text-ui test-suite)
      