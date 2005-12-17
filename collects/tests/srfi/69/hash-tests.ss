(module hash-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))

  (require (lib "list.ss" "srfi" "1")
           (prefix h: (lib "69.ss" "srfi")))

  (provide hash-tests)
  
  (define test-hash-table1
    (h:alist->hash-table '((a . 1) (b . 2) (c . 3))))
  (define test-hash-table2
    (h:alist->hash-table '(("a" . 1) ("b" . 2) ("c" . 3)) string-ci=? h:string-ci-hash))

  (define hash-tests
    (make-test-suite
     "srfi-69 test suite"
     (make-test-case
      "make-hash-table and hash-table?"
      (assert-true
       (h:hash-table? (h:make-hash-table))))
     (make-test-case
      "alist->hash-table"
      (assert-true
       (h:hash-table? test-hash-table1)))
     (make-test-case
      "hash-table-equivalence-function"
      (assert-eq?
       (h:hash-table-equivalence-function (h:make-hash-table))
       equal?)
      (assert-eq?
       (h:hash-table-equivalence-function (h:make-hash-table eq?))
       eq?)
      (assert-eq?
       (h:hash-table-equivalence-function test-hash-table2)
       string-ci=?))
     (make-test-case
      "hash-table-hash-function"
      (assert-eq?
       (h:hash-table-hash-function (h:make-hash-table))
       h:hash)
      (assert-eq?
       (h:hash-table-hash-function (h:make-hash-table eq?))
       h:hash-by-identity)
      (assert-eq?
       (h:hash-table-hash-function test-hash-table2)
       h:string-ci-hash))
     (make-test-case
      "hash-table-ref"
      (assert-equal?
       (h:hash-table-ref test-hash-table1 'b)
       2)
      (assert-equal?
       (h:hash-table-ref test-hash-table2 "C")
       3)
      (assert-false
       (h:hash-table-ref test-hash-table1 'd (lambda () #f))))
     (make-test-case
      "hash-table-ref/default"
      (assert-false
       (h:hash-table-ref/default test-hash-table2 "d" #f)))
     (make-test-case
      "hash-table-set!"
      (assert-equal?
       (begin (h:hash-table-set! test-hash-table1 'c 4)
              (h:hash-table-ref test-hash-table1 'c))
       4)
      (assert-equal?
       (begin (h:hash-table-set! test-hash-table2 "d" 4)
              (h:hash-table-ref test-hash-table2 "D"))
       4))
     (make-test-case
      "hash-table-delete!"
      (assert-false
       (begin (h:hash-table-delete! test-hash-table2 "D")
              (h:hash-table-ref/default test-hash-table2 "d" #f))))
     (make-test-case
      "hash-table-exists?"
      (assert-true
       (h:hash-table-exists? test-hash-table2 "B"))
      (assert-false
       (h:hash-table-exists? test-hash-table1 'd)))
     (make-test-case
      "hash-table-update!"
      (assert-equal?
       (begin (h:hash-table-update! test-hash-table1 'c sub1)
              (h:hash-table-ref test-hash-table1 'c))
       3)
      (assert-equal?
       (begin (h:hash-table-update! test-hash-table2 "d" add1 (lambda () 3))
              (h:hash-table-ref test-hash-table2 "d"))
       4))
     (make-test-case
      "hash-table-update!/default"
      (assert-equal?
       (begin (h:hash-table-update!/default test-hash-table1 'd add1 3)
              (h:hash-table-ref test-hash-table1 'd))
       4))
     (make-test-case
      "hash-table-size"
      (assert-equal?
       (h:hash-table-size test-hash-table1)
       4)
      (assert-equal?
       (h:hash-table-size test-hash-table2)
       4))
     (make-test-case
      "hash-table-keys"
      (assert-true
       (lset= eq?
              (h:hash-table-keys test-hash-table1)
              '(a b c d)))
      (assert-true
       (lset= equal?
              (h:hash-table-keys test-hash-table2)
              (list "a" "b" "c" "d"))))
     (make-test-case
      "hash-table-values"
      (assert-true
       (lset= eqv?
              (h:hash-table-values test-hash-table1)
              '(1 2 3 4)))
      (assert-true
       (lset= eqv?
              (h:hash-table-values test-hash-table2)
              '(1 2 3 4))))
     (make-test-case
      "hash-table-walk"
      (assert-true
       (let ((a '()))
         (h:hash-table-walk test-hash-table1
                            (lambda (key value)
                              (set! a (cons (cons key value) a))))
         (lset= equal?
                a
                '((a . 1) (b . 2) (c . 3) (d . 4))))))
     (make-test-case
      "hash-table-fold"
      (assert-true
       (lset= equal?
              (h:hash-table-fold test-hash-table2
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
              (h:hash-table->alist test-hash-table1)
              '((a . 1) (b . 2) (c . 3) (d . 4)))))
     (make-test-case
      "hash-table-copy"
      (assert-true
       (lset= equal?
              (h:hash-table->alist (h:hash-table-copy test-hash-table2))
              (list (cons "a" 1)
                    (cons "b" 2)
                    (cons "c" 3)
                    (cons "d" 4))))
      (assert-false
       (eq? (h:hash-table-copy test-hash-table1)
            test-hash-table1))
      (assert-eq?
       (h:hash-table-equivalence-function
        test-hash-table1)
       (h:hash-table-equivalence-function
        (h:hash-table-copy test-hash-table1)))
      (assert-eq?
       (h:hash-table-hash-function
        test-hash-table2)
       (h:hash-table-hash-function
        (h:hash-table-copy test-hash-table2))))
     (make-test-case
      "hash-table->alist"
      (assert-true
       (lset= equal?
              (h:hash-table->alist
               (h:hash-table-merge!  test-hash-table1
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
              (h:hash-table->alist
               (h:hash-table-merge!  test-hash-table2
                                   test-hash-table2))
              '(("a" . 1)
                ("b" . 2)
                ("c" . 3)
                ("d" . 4)))))))

  )
