#lang racket/base

(provide case-tests)

(require racket/match
         racket/stxparam
         syntax/parse/define
         rackunit
         (for-syntax racket/base
                     racket/syntax))

(define-syntax-parameter this-value #f)

;; this macro defines
;; - `fragment`, a syntax fragment in the quoted form
;; - `test`, a function that runs the match clauses on the input value
(define-syntax-parse-rule (define-test clause ...)
  #:with result ((syntax-local-value #'match) #'(match 'val-expr clause ...))
  #:with fragment (format-id this-syntax "fragment")
  #:with test (format-id this-syntax "test")
  (begin
    (define fragment 'result)
    (define (test val)
      (syntax-parameterize ([this-value (make-rename-transformer #'val)])
        (match val
          clause ...)))))

;; A bit circular to use check-match to test match,
;; though the check-match mostly operates on lists, not caseable values,
;; so that should be ok.
(define case-tests
  (test-suite "Test for the compilation from match to case"
    (test-case "Supported types"
      (define-test
        [1 'a]
        [2 'a*]
        ['a 'b]
        ['b 'b*]
        ["a" 'c]
        ["b" 'c*]
        [#"a" 'd]
        [#"b" 'd*]
        [#px"a" 'e]
        [#px"b" 'e*]
        [#rx"a" 'f]
        [#rx"b" 'f*]
        ['#:a 'g]
        ['#:b 'g*]
        [#t 'h]
        [#f 'i])

      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1) ,_]
                [(2) ,_]
                [(a) ,_]
                [(b) ,_]
                [("a") ,_]
                [("b") ,_]
                [(#"a") ,_]
                [(#"b") ,_]
                [(#px"a") ,_]
                [(#px"b") ,_]
                [(#rx"a") ,_]
                [(#rx"b") ,_]
                [(#:a) ,_]
                [(#:b) ,_]
                [(#t) ,_]
                [(#f) ,_]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-equal? (test 2) 'a*)
      (check-equal? (test 'a) 'b)
      (check-equal? (test 'b) 'b*)
      (check-equal? (test "a") 'c)
      (check-equal? (test "b") 'c*)
      (check-equal? (test #"a") 'd)
      (check-equal? (test #"b") 'd*)
      (check-equal? (test #px"a") 'e)
      (check-equal? (test #px"b") 'e*)
      (check-equal? (test #rx"a") 'f)
      (check-equal? (test #rx"b") 'f*)
      (check-equal? (test '#:a) 'g)
      (check-equal? (test '#:b) 'g*)
      (check-equal? (test #t) 'h)
      (check-equal? (test #f) 'i)
      (check-exn exn:fail? (λ () (test 3))))

    (test-case "Or"
      (define-test
        [(or 1 (or 11 111)) 'a]
        [(or (or 2 112) 12) 'b]
        [(or 3 13 113) 'c]
        [4 'd]
        ;; this is parsed as (not _), so it will break the optimization
        ;; on subsequent clauses
        [(or) 'e]
        [5 'f])

      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1 11 111) ,_]
                [(2 112 12) ,_]
                [(3 13 113) ,_]
                [(4) ,_]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-equal? (test 11) 'a)
      (check-equal? (test 111) 'a)
      (check-equal? (test 2) 'b)
      (check-equal? (test 12) 'b)
      (check-equal? (test 112) 'b)
      (check-equal? (test 3) 'c)
      (check-equal? (test 13) 'c)
      (check-equal? (test 113) 'c)
      (check-equal? (test 4) 'd)
      (check-equal? (test 5) 'f))

    (test-case "Overlapping"
      (define x 0)

      (define-test
        [1 'a]
        [2 'b]
        [1 'c]
        [3 'd]
        [3 'e]
        [4 'f]
        [5 (set! x (add1 x))
           (failure-cont)]
        [5 (set! x (add1 x))
           'h])

      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1) (syntax-parameterize ,_ (let () 'a))]
                [(2) (syntax-parameterize ,_ (let () 'b))]
                [(3) (syntax-parameterize ,_ (let () 'd))]
                [(4) (syntax-parameterize ,_ (let () 'f))]
                [(5) (syntax-parameterize ,_ (let ()
                                               (set! x (add1 x))
                                               (failure-cont)))]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-equal? (test 2) 'b)
      (check-equal? (test 3) 'd)
      (check-equal? (test 4) 'f)
      (check-equal? x 0)
      (check-equal? (test 5) 'h)
      (check-equal? x 2))

    (test-case "Overlapping or"
      (define-test
        [(or 1 2) 'a]
        [(or 2 3) 'b]
        [(or 3 4) 'c]
        [(or 5 6) 'd]
        [(or 4 7) 'e]
        [(or 8 9) 'f])

      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1 2) (syntax-parameterize ,_ (let () 'a))]
                [(5 6) (syntax-parameterize ,_ (let () 'd))]
                [(8 9) (syntax-parameterize ,_ (let () 'f))]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-equal? (test 2) 'a)
      (check-equal? (test 3) 'b)
      (check-equal? (test 4) 'c)
      (check-equal? (test 5) 'd)
      (check-equal? (test 6) 'd)
      (check-equal? (test 7) 'e)
      (check-equal? (test 8) 'f)
      (check-equal? (test 9) 'f))

    (test-case "Duplicate in a clause"
      (define-test
        [(or 1 (or 1 2) 1) 'a]
        [(or 3 3 (or 3 4)) 'b])

      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1 1 2 1) ,_]
                [(3 3 3 4) ,_]
                [else ,_])))))
      (check-equal? (test 1) 'a)
      (check-equal? (test 2) 'a)
      (check-equal? (test 3) 'b)
      (check-equal? (test 4) 'b))

    (test-case "Non-case pattern"
      (define-test
        [1 'a]
        [2 'b]
        [(? odd?) 'c]
        [3 'd]
        [4 'e])
      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1) ,_]
                [(2) ,_]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-equal? (test 2) 'b)
      (check-equal? (test 3) 'c)
      (check-equal? (test 4) 'e)
      (check-equal? (test 5) 'c))

    (test-case "#:when"
      (define-test
        [1 #:when #f 'a]
        [1 #:when #t 'b]
        [2 #:when #t 'c]
        [2 #:when #f 'd]
        [3 'e])
      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let (,_) ; next
              (case ,_
                [(1) (syntax-parameterize (,_)
                       (let ()
                         (if #f
                             (let () 'a)
                             (fail))))]
                [(2) (syntax-parameterize (,_)
                       (let ()
                         (if #t
                             (let () 'c)
                             (fail))))]
                [(3) ,_]
                [else ,_])))))
      (check-equal? (test 1) 'b)
      (check-equal? (test 2) 'c)
      (check-equal? (test 3) 'e))

    (test-case "(=> exit-id)"
      (define x #f)
      (define-test
        [(or 1 2) (=> exit-id)
                  (cond
                    [(even? this-value)
                     (set! x #t)
                     (list (exit-id))]
                    [else 'a])]
        [_ 'b])
      (check-match
       fragment
       `(let ([,_ ,_]) ; val-expr
          (let (,_) ; fail
            (let ([,next ,_]) ; next
              (case ,_
                [(1 2) (call-with-continuation-prompt ,_ ,_ (lambda () (,next)))]
                [else ,_])))))

      (check-equal? (test 1) 'a)
      (check-false x)
      (check-equal? (test 2) 'b)
      (check-true x))))

(module+ test
  (require rackunit/text-ui)

  (run-tests case-tests))
