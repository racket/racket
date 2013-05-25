#lang scheme/base

(require typed-racket/utils/utils
         (prefix-in ru: (combine-in rackunit rackunit/private/test-case rackunit/private/check))
         (for-syntax
          scheme/base syntax/parse
          (utils tc-utils)
          (env init-envs)
          (except-in (rep filter-rep object-rep type-rep) make-arr)
          (rename-in (types abbrev union) [make-arr* make-arr])))

(define-for-syntax unit-env
  (make-env
   [ru:check-around
    (-poly (a) (-> (-> a) a))]
   ;; current-test-case-around
   [(syntax-parse (local-expand #'(ru:test-begin 0) 'expression null)
      #:context #'ru:test-begin
      [(_ _ . _) #'ctca])
    (-poly (a) (-> (-> a) a))]))

(begin-for-syntax (initialize-type-env unit-env))
