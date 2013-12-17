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
        (let ([opt (optimize argument variance)])
          (with-check-info* (list (make-check-actual opt))
            (lambda ()
              (unless (equal? opt expected)
                (fail-check)))))))))


(define tests
  (test-suite "Static Contract Optimizer Tests"
    ;; Lists
    (check-optimize 'covariant
      (listof/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (listof/sc any/sc)
      list?/sc)
    (check-optimize 'covariant
      (listof/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (listof/sc none/sc)
      empty-list/sc)

    ;; Heterogeneous Lists
    (check-optimize 'covariant
      (list/sc any/sc)
      any/sc)
    ;; TODO fix ability to test equality here
    #;
    (check-optimize 'contravariant
      (list/sc any/sc)
      (list-length/sc 1))
    (check-optimize 'covariant
      (list/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (list/sc none/sc)
      none/sc)
    (check-optimize 'covariant
      (list/sc)
      any/sc)
    (check-optimize 'contravariant
      (list/sc)
      empty-list/sc)


    ;; Sets
    (check-optimize 'covariant
      (set/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (set/sc any/sc)
      set?/sc)
    (check-optimize 'covariant
      (set/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (set/sc none/sc)
      empty-set/sc)

    ;; Vectors
    (check-optimize 'covariant
      (vectorof/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (vectorof/sc any/sc)
      vector?/sc)
    (check-optimize 'covariant
      (vectorof/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (vectorof/sc none/sc)
      empty-vector/sc)

    ;; Heterogeneous Vectors
    (check-optimize 'covariant
      (vector/sc any/sc)
      any/sc)
    ;; TODO fix ability to test equality here
    #;
    (check-optimize 'contravariant
      (vector/sc any/sc)
      (vector-length/sc 1))
    (check-optimize 'covariant
      (vector/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (vector/sc none/sc)
      none/sc)
    (check-optimize 'covariant
      (vector/sc set?/sc)
      (vector/sc set?/sc))
    (check-optimize 'contravariant
      (vector/sc set?/sc)
      (vector/sc set?/sc))

    ;; HashTables
    (check-optimize 'covariant
      (hash/sc any/sc any/sc)
      any/sc)
    (check-optimize 'contravariant
      (hash/sc any/sc any/sc)
      hash?/sc)
    (check-optimize 'covariant
      (hash/sc none/sc any/sc)
      any/sc)
    (check-optimize 'covariant
      (hash/sc any/sc none/sc)
      any/sc)
    (check-optimize 'contravariant
      (hash/sc none/sc any/sc)
      empty-hash/sc)
    (check-optimize 'contravariant
      (hash/sc any/sc none/sc)
      empty-hash/sc)

    ;; And
    (check-optimize 'contravariant
      (and/sc set?/sc)
      set?/sc)
    (check-optimize 'contravariant
      (and/sc set?/sc any/sc)
      set?/sc)
    (check-optimize 'contravariant
      (and/sc set?/sc none/sc)
      none/sc)
    (check-optimize 'contravariant
      (and/sc)
      any/sc)
    (check-optimize 'contravariant
      (and/sc any/sc any/sc)
      any/sc)

    ;; Or
    (check-optimize 'contravariant
      (or/sc set?/sc)
      set?/sc)
    (check-optimize 'contravariant
      (or/sc set?/sc none/sc)
      set?/sc)
    (check-optimize 'contravariant
      (or/sc set?/sc any/sc)
      any/sc)
    (check-optimize 'covariant
      (or/sc)
      any/sc)
    (check-optimize 'contravariant
      (or/sc)
      none/sc)
    (check-optimize 'contravariant
      (or/sc any/sc any/sc)
      any/sc)

    ;; None
    (check-optimize 'covariant none/sc any/sc)
    (check-optimize 'contravariant none/sc none/sc)

    ;; TODO add these test cases
    ;; Boxes
    ;; Syntax
    ;; Promise

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
