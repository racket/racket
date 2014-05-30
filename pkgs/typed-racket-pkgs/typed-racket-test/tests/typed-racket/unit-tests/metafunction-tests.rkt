#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (typecheck tc-subst)
         (rep filter-rep type-rep object-rep)
         (types abbrev union filter-ops tc-result)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)


(define tests
  (test-suite "Metafunctions"
    (test-suite "open-Values"
      (check-equal?
        (open-Values (make-Values (list (-result -Symbol))) (list -empty-obj) (list Univ))
        (ret -Symbol))

      (check-equal?
        (open-Values (make-Values (list (-result -Symbol) (-result -String))) 
                     (list -empty-obj -empty-obj) (list Univ Univ))
        (ret (list -Symbol -String)))

      (check-equal?
        (open-Values (make-Values (list (-result -Symbol (-FS -top -bot)))) (list -empty-obj) (list Univ))
        (ret -Symbol (-FS -top -bot)))

      (check-equal?
        (open-Values (make-Values (list (-result -Symbol (-FS -top -bot) (make-Path null '(0 0)))))
                     (list -empty-obj) (list Univ))
        (ret -Symbol (-FS -top -bot)))

      (check-equal?
        (open-Values (make-Values (list (-result (-opt -Symbol) (-FS (-filter -String '(0 0)) -top))))
                     (list -empty-obj) (list Univ))
        (ret (-opt -Symbol) -top-filter))

      (check-equal?
        (open-Values (make-Values (list (-result (-opt -Symbol) (-FS (-not-filter -String '(0 0)) -top))))
                     (list -empty-obj) (list Univ))
        (ret (-opt -Symbol) -top-filter))

      (check-equal?
        (open-Values (make-Values (list (-result (-opt -Symbol) (-FS (-imp (-not-filter (-val #f) '(0 0))
                                                                           (-not-filter -String #'x))
                                                                     -top))))
                     (list -empty-obj) (list Univ))
        (ret (-opt -Symbol) -top-filter))

      (check-equal?
        (open-Values (make-Values (list (-result (-opt -Symbol) (-FS (-not-filter -String '(0 0)) -top)
                                                 (make-Path null '(0 0)))))
                     (list (make-Path null #'x)) (list Univ))
        (ret (-opt -Symbol) (-FS (-not-filter -String #'x) -top) (make-Path null #'x)))

      ;; Check additional filters
      (check-equal?
        (open-Values (make-Values (list (-result (-opt -Symbol) (-FS (-not-filter -String '(0 0)) -top)
                                                 (make-Path null '(0 0)))))
                     (list (make-Path null #'x)) (list -String))
        (ret (-opt -Symbol) -false-filter (make-Path null #'x)))

      ;; Substitute into ranges correctly
      (check-equal?
        (open-Values (make-Values (list (-result (-opt (-> Univ -Boolean : (-FS (-filter -Symbol '(0 0)) -top))))))
                     (list (make-Path null #'x)) (list Univ))
        (ret (-opt (-> Univ -Boolean : (-FS (-filter -Symbol '(0 0)) -top)))))

      (check-equal?
        (open-Values (make-Values (list (-result (-opt (-> Univ -Boolean : (-FS (-filter -Symbol '(1 0)) -top))))))
                     (list (make-Path null #'x)) (list Univ))
        (ret (-opt (-> Univ -Boolean : (-FS (-filter -Symbol #'x) -top)))))

    )
  ))
