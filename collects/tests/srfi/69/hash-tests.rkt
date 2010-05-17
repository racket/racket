(module hash-tests mzscheme

  (require rackunit)

  (require srfi/1/list
           (prefix h: srfi/69))

  (provide hash-tests)
  
  (define test-hash-table1
    (h:alist->hash-table '((a . 1) (b . 2) (c . 3))))
  (define test-hash-table2
    (h:alist->hash-table '(("a" . 1) ("b" . 2) ("c" . 3)) string-ci=? h:string-ci-hash))

  (define hash-tests
    (test-suite
     "srfi-69 test suite"
     (test-case
      "make-hash-table and hash-table?"
      (check-true
       (h:hash-table? (h:make-hash-table))))
     (test-case
      "alist->hash-table"
      (check-true
       (h:hash-table? test-hash-table1)))
     (test-case
      "hash-table-equivalence-function"
      (check-eq?
       (h:hash-table-equivalence-function (h:make-hash-table))
       equal?)
      (check-eq?
       (h:hash-table-equivalence-function (h:make-hash-table eq?))
       eq?)
      (check-eq?
       (h:hash-table-equivalence-function test-hash-table2)
       string-ci=?))
     (test-case
      "hash-table-hash-function"
      (check-eq?
       (h:hash-table-hash-function (h:make-hash-table))
       h:hash)
      (check-eq?
       (h:hash-table-hash-function (h:make-hash-table eq?))
       h:hash-by-identity)
      (check-eq?
       (h:hash-table-hash-function test-hash-table2)
       h:string-ci-hash))
     (test-case
      "hash-table-ref"
      (check-equal?
       (h:hash-table-ref test-hash-table1 'b)
       2)
      (check-equal?
       (h:hash-table-ref test-hash-table2 "C")
       3)
      (check-false
       (h:hash-table-ref test-hash-table1 'd (lambda () #f))))
     (test-case
      "hash-table-ref/default"
      (check-false
       (h:hash-table-ref/default test-hash-table2 "d" #f)))
     (test-case
      "hash-table-set!"
      (check-equal?
       (begin (h:hash-table-set! test-hash-table1 'c 4)
              (h:hash-table-ref test-hash-table1 'c))
       4)
      (check-equal?
       (begin (h:hash-table-set! test-hash-table2 "d" 4)
              (h:hash-table-ref test-hash-table2 "D"))
       4))
     (test-case
      "hash-table-delete!"
      (check-false
       (begin (h:hash-table-delete! test-hash-table2 "D")
              (h:hash-table-ref/default test-hash-table2 "d" #f))))
     (test-case
      "hash-table-exists?"
      (check-true
       (h:hash-table-exists? test-hash-table2 "B"))
      (check-false
       (h:hash-table-exists? test-hash-table1 'd)))
     (test-case
      "hash-table-update!"
      (check-equal?
       (begin (h:hash-table-update! test-hash-table1 'c sub1)
              (h:hash-table-ref test-hash-table1 'c))
       3)
      (check-equal?
       (begin (h:hash-table-update! test-hash-table2 "d" add1 (lambda () 3))
              (h:hash-table-ref test-hash-table2 "d"))
       4))
     (test-case
      "hash-table-update!/default"
      (check-equal?
       (begin (h:hash-table-update!/default test-hash-table1 'd add1 3)
              (h:hash-table-ref test-hash-table1 'd))
       4))
     (test-case
      "hash-table-size"
      (check-equal?
       (h:hash-table-size test-hash-table1)
       4)
      (check-equal?
       (h:hash-table-size test-hash-table2)
       4))
     (test-case
      "hash-table-keys"
      (check-true
       (lset= eq?
              (h:hash-table-keys test-hash-table1)
              '(a b c d)))
      (check-true
       (lset= equal?
              (h:hash-table-keys test-hash-table2)
              (list "a" "b" "c" "d"))))
     (test-case
      "hash-table-values"
      (check-true
       (lset= eqv?
              (h:hash-table-values test-hash-table1)
              '(1 2 3 4)))
      (check-true
       (lset= eqv?
              (h:hash-table-values test-hash-table2)
              '(1 2 3 4))))
     (test-case
      "hash-table-walk"
      (check-true
       (let ((a '()))
         (h:hash-table-walk test-hash-table1
                            (lambda (key value)
                              (set! a (cons (cons key value) a))))
         (lset= equal?
                a
                '((a . 1) (b . 2) (c . 3) (d . 4))))))
     (test-case
      "hash-table-fold"
      (check-true
       (lset= equal?
              (h:hash-table-fold test-hash-table2
                                 (lambda (key value accu)
                                   (cons (cons key value) accu))
                                 '())
              (list (cons "a" 1)
                    (cons "b" 2)
                    (cons "c" 3)
                    (cons "d" 4)))))
     (test-case
      "hash-table->alist"
      (check-true
       (lset= equal?
              (h:hash-table->alist test-hash-table1)
              '((a . 1) (b . 2) (c . 3) (d . 4)))))
     (test-case
      "hash-table-copy"
      (check-true
       (lset= equal?
              (h:hash-table->alist (h:hash-table-copy test-hash-table2))
              (list (cons "a" 1)
                    (cons "b" 2)
                    (cons "c" 3)
                    (cons "d" 4))))
      (check-false
       (eq? (h:hash-table-copy test-hash-table1)
            test-hash-table1))
      (check-eq?
       (h:hash-table-equivalence-function
        test-hash-table1)
       (h:hash-table-equivalence-function
        (h:hash-table-copy test-hash-table1)))
      (check-eq?
       (h:hash-table-hash-function
        test-hash-table2)
       (h:hash-table-hash-function
        (h:hash-table-copy test-hash-table2))))
     (test-case
      "hash-table->alist"
      (check-true
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
      (check-true
       (lset= equal?
              (h:hash-table->alist
               (h:hash-table-merge!  test-hash-table2
                                   test-hash-table2))
              '(("a" . 1)
                ("b" . 2)
                ("c" . 3)
                ("d" . 4)))))))

  )
