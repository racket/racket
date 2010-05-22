#lang scheme

(require mzlib/etc
         planet/util
         "checks.ss"
         "../syntax.ss")

(provide syntax-suite)

(define here
  (datum->syntax
   #f
   'here
   (list (build-path (this-expression-source-directory)
                     (this-expression-file-name))
         1 1 1 1)))

(define syntax-suite
  (test-suite "syntax.ss"

    (test-suite "Contracts"

      (test-suite "syntax-datum/c"
        (test-ok (with/c (syntax-datum/c (listof (listof natural-number/c)))
                         #'((0 1 2) () (3 4) (5))))
        (test-bad (with/c (syntax-datum/c (listof (listof natural-number/c)))
                          #'((x y z))))
        (test-bad (with/c (syntax-datum/c string?) "xyz")))

      (test-suite "syntax-listof/c"
        (test-ok (with/c (syntax-listof/c identifier?) #'(a b c)))
        (test-bad (with/c (syntax-listof/c identifier?) #'(1 2 3)))
        (test-bad (with/c (syntax-listof/c identifier?) #'(a b . c)))
        (test-bad (with/c (syntax-listof/c identifier?) (list #'a #'b #'c))))

      (test-suite "syntax-list/c"
        (test-ok (with/c (syntax-list/c identifier? (syntax/c string?))
                         #'(a "b")))
        (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                          #'(a "b" #:c)))
        (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                          #'(a b)))
        (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                          #'(a "b" . c)))
        (test-bad (with/c (syntax-list/c identifier? (syntax/c string?))
                          '(#'a #'"b")))))

    (test-suite "Source Location Representations"

      (test-suite "src/c"
        (test-ok (with/c src/c #f))
        (test-ok (with/c src/c (make-srcloc 'source 1 0 1 0)))
        (test-ok (with/c src/c #'here))
        (test-ok (with/c src/c (list 'source 1 0 1 0)))
        (test-bad (with/c src/c (list 'source 1 0 0 1)))
        (test-bad (with/c src/c (list 'source 0 0 0 0)))
        (test-ok (with/c src/c (vector 'source 1 0 1 0)))
        (test-bad (with/c src/c (vector 'source 1 0 0 1)))
        (test-bad (with/c src/c (vector 'source 0 0 0 0)))
        (test-bad (with/c src/c 'symbol)))

      (test-suite "src->srcloc"
        (test-ok (check-equal? (src->srcloc #f) (make-srcloc #f #f #f #f #f)))
        (test-ok (check-equal? (src->srcloc (make-srcloc 'source 1 0 1 0))
                               (make-srcloc 'source 1 0 1 0)))
        (test-ok (check-equal? (src->srcloc (datum->syntax #f 'here #f))
                               ;; Note known bug w/ syntax-span:
                               (make-srcloc #f #f #f #f 0)))
        (test-ok (check-equal? (src->srcloc (list 'source 1 0 1 0))
                               (make-srcloc 'source 1 0 1 0)))
        (test-ok (check-equal? (src->srcloc (vector 'source 1 0 1 0))
                               (make-srcloc 'source 1 0 1 0)))
        (test-ok (check-equal? (src->srcloc) (make-srcloc #f #f #f #f #f)))
        (test-ok (check-equal? (src->srcloc (make-srcloc 'one 1 0 1 0)
                                            (make-srcloc 'two 1 0 1 0))
                               (make-srcloc #f #f #f #f #f)))
        (test-ok (check-equal? (src->srcloc (make-srcloc 'source 1 0 1 0)
                                            (make-srcloc 'source 2 1 2 1))
                               (make-srcloc 'source 1 0 1 2))))

      (test-suite "src->list"
        (test-ok (check-equal? (src->list #f) (list #f #f #f #f #f)))
        (test-ok (check-equal? (src->list (make-srcloc 'source 1 0 1 0))
                               (list 'source 1 0 1 0)))
        (test-ok (check-equal? (src->list (datum->syntax #f 'here #f))
                               ;; Note known bug w/ syntax-span:
                               (list #f #f #f #f 0)))
        (test-ok (check-equal? (src->list (list 'source 1 0 1 0))
                               (list 'source 1 0 1 0)))
        (test-ok (check-equal? (src->list (vector 'source 1 0 1 0))
                               (list 'source 1 0 1 0)))
        (test-ok (check-equal? (src->list) (list #f #f #f #f #f)))
        (test-ok (check-equal? (src->list (make-srcloc 'one 1 0 1 0)
                                          (make-srcloc 'two 1 0 1 0))
                               (list #f #f #f #f #f)))
        (test-ok (check-equal? (src->list (make-srcloc 'source 1 0 1 0)
                                          (make-srcloc 'source 2 1 2 1))
                               (list 'source 1 0 1 2))))

      (test-suite "src->vector"
        (test-ok (check-equal? (src->vector #f) (vector #f #f #f #f #f)))
        (test-ok (check-equal? (src->vector (make-srcloc 'source 1 0 1 0))
                               (vector 'source 1 0 1 0)))
        (test-ok (check-equal? (src->vector (datum->syntax #f 'here #f))
                               ;; Note known bug w/ syntax-span:
                               (vector #f #f #f #f 0)))
        (test-ok (check-equal? (src->vector (list 'source 1 0 1 0))
                               (vector 'source 1 0 1 0)))
        (test-ok (check-equal? (src->vector (vector 'source 1 0 1 0))
                               (vector 'source 1 0 1 0)))
        (test-ok (check-equal? (src->vector) (vector #f #f #f #f #f)))
        (test-ok (check-equal? (src->vector (make-srcloc 'one 1 0 1 0)
                                            (make-srcloc 'two 1 0 1 0))
                               (vector #f #f #f #f #f)))
        (test-ok (check-equal? (src->vector (make-srcloc 'source 1 0 1 0)
                                            (make-srcloc 'source 2 1 2 1))
                               (vector 'source 1 0 1 2))))

      (test-suite "src->syntax"
        (test-ok (check-pred syntax? (src->syntax #f)))
        (test-ok (check-pred syntax?
                             (src->syntax (make-srcloc 'source 1 0 1 0))))
        (test-ok (check-pred syntax? (src->syntax (datum->syntax #f 'here #f))))
        (test-ok (check-pred syntax? (src->syntax (list 'source 1 0 1 0))))
        (test-ok (check-pred syntax? (src->syntax (vector 'source 1 0 1 0))))
        (test-ok (check-pred syntax? (src->syntax)))
        (test-ok (check-pred syntax? (src->syntax (make-srcloc 'one 1 0 1 0)
                                                  (make-srcloc 'two 1 0 1 0))))
        (test-ok (check-pred syntax?
                             (src->syntax (make-srcloc 'source 1 0 1 0)
                                          (make-srcloc 'source 2 1 2 1)))))

      (test-suite "src-known?"
        (test-ok (check-false (src-known? (list #f #f #f #f #f))))
        (test-ok (check-true (src-known? (vector 'source #f #f #f #f))))
        (test-ok (check-true (src-known? (datum->syntax #f 'x
                                                        (list 'a 1 2 3 4)))))))

    (test-suite "Syntax Lists"

      (test-suite "syntax-list"
        (test
         (check-equal?
          (with-syntax ([([x ...] ...) #'([1 2] [3] [4 5 6])])
            (map syntax->datum (syntax-list x ... ...)))
          (list 1 2 3 4 5 6))))

      (test-suite "syntax-map"
        (test-case "identifiers to symbols"
          (check-equal? (syntax-map syntax-e #'(a b c)) '(a b c)))))

    (test-suite "Syntax Conversions"

      (test-suite "to-syntax"
        (test-case "symbol + context = identifier"
          (check bound-identifier=?
                 (to-syntax #:stx #'context 'id)
                 #'id)))

      (test-suite "to-datum"
        (test-case "syntax"
          (check-equal? (to-datum #'((a b) () (c)))
                        '((a b) () (c))))
        (test-case "non-syntax"
          (check-equal? (to-datum '((a b) () (c)))
                        '((a b) () (c))))
        (test-case "nested syntax"
          (let* ([stx-ab #'(a b)]
                 [stx-null #'()]
                 [stx-c #'(c)])
            (check-equal? (to-datum (list stx-ab stx-null stx-c))
                          (list stx-ab stx-null stx-c))))))

    (test-suite "Syntax Source Locations"

      (test-suite "syntax-source-file-name"
        (test-case "here"
          (check-equal? (syntax-source-file-name here)
                        (this-expression-file-name)))
        (test-case "fail"
          (check-equal? (syntax-source-file-name (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-directory"
        (test-case "here"
          (check-equal? (syntax-source-directory here)
                        (this-expression-source-directory)))
        (test-case "fail"
          (check-equal? (syntax-source-directory (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package-owner"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package-owner
                         (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package-name"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package-name
                         (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package-major"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package-major
                         (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package-minor"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package-minor
                         (datum->syntax #f 'fail))
                        #f)))

      (test-suite "syntax-source-planet-package-symbol"
        (test-case "fail"
          (check-equal? (syntax-source-planet-package-minor
                         (datum->syntax #f 'fail))
                        #f)))

      (test-suite "make-planet-path"))

    (test-suite "Transformers"

      (test-suite "redirect-transformer"
        (test (check-equal?
               (syntax->datum ((redirect-transformer #'x) #'y))
               'x))
        (test (check-equal?
               (syntax->datum ((redirect-transformer #'x) #'(y z)))
               '(x z))))

      (test-suite "full-kernel-form-identifier-list"
        (test (check-pred list? (full-kernel-form-identifier-list)))
        (test (for ([id (in-list (full-kernel-form-identifier-list))])
                (check-pred identifier? id))))

      (test-suite "head-expand")

      (test-suite "trampoline-transformer")

      (test-suite "quote-transformer"))

    (test-suite "Pattern Bindings"

      (test-suite "with-syntax*"
        (test-case "identifier"
          (check bound-identifier=?
                 (with-syntax* ([a #'id] [b #'a]) #'b)
                 #'id))))))
