#lang racket/base

(require "test-utils.rkt"
         racket/list
         rackunit
         (static-contracts instantiate optimize combinators))

(provide tests)
(gen-test-main)

(define-check (check-optimize variance* argument* expected*)
  (let ([variance variance*]
        [argument argument*]
        [expected expected*])
    (with-check-info*
      (list (make-check-info 'original argument)
            (make-check-expected expected))
      (lambda ()
        (let  ([opt (optimize argument variance)])
          (with-check-info* (list (make-check-actual opt))
            (lambda ()
              (unless (equal? opt expected)
                (fail-check)))))))))


(define tests
  (test-suite "Static Contract Optimizer Tests"
    (check-optimize 'covariant
      (listof/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (listof/sc any/sc)
      list?/sc)
    (check-optimize 'covariant
      (set/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (set/sc any/sc)
      set?/sc)
    (check-optimize 'covariant
      (function/sc (list (listof/sc any/sc))
                   (list)
                   (list)
                   (list)
                   #f
                   (list (listof/sc any/sc)))
      (function/sc (list list?/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   #f))
    (check-optimize 'contravariant
      (function/sc (list (listof/sc any/sc))
                   (list)
                   (list)
                   (list)
                   #f
                   (list (listof/sc any/sc)))
      (function/sc (list any/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   (list list?/sc)))
    (check-optimize 'contravariant
      (function/sc (list (listof/sc any/sc))
                   (list)
                   (list)
                   (list)
                   #f
                   (list any/sc))
      (function/sc (list any/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   (list any/sc)))
    (check-optimize 'covariant
      (case->/sc empty)
      (case->/sc empty))
    (check-optimize 'contravariant
      (case->/sc empty)
      (case->/sc empty))
    (check-optimize 'covariant
      (parameter/sc list?/sc (flat/sc #'symbol?))
      (parameter/sc list?/sc any/sc))
    (check-optimize 'contravariant
      (case->/sc (list (arr/sc (list (listof/sc any/sc)) (listof/sc (set/sc any/sc)) (list (listof/sc any/sc)))))
      (case->/sc (list (arr/sc (list any/sc) any/sc (list list?/sc)))))
    (check-optimize 'covariant
      (case->/sc (list (arr/sc (list (listof/sc any/sc)) (listof/sc (set/sc any/sc)) (list (listof/sc any/sc)))))
      (case->/sc (list (arr/sc (list list?/sc) (listof/sc set?/sc) #f))))))
