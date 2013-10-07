#lang racket/base

(require "combinators.rkt"
         "structures.rkt"
         racket/set
         racket/list
         (except-in racket/contract recursive-contract)
         racket/match)

(provide 
  (contract-out
    [optimize (static-contract? (or/c 'covariant 'contravariant 'invariant ) . -> . static-contract?)]))


(define (any/sc-reduce sc)
  (match sc
    [(listof/sc: (any/sc:)) list?/sc]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]
    [(any/sc:) sc]
    [else sc]))


(define (covariant-any/sc-reduce sc)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [else sc]))

(define (flat-reduce sc)
  (match sc
    [(? flat/sc?)
     any/sc]
    [sc sc]))

(define (invert-variance v)
  (case v
    [(covariant) 'contravariant]
    [(contravariant) 'covariant]
    [(invariant) 'invariant]))

(define (combine-variance var1 var2)
  (case var1
    [(covariant) var2]
    [(contravariant) (invert-variance var2)]
    [(invariant) 'invariant]))

(define (optimize sc variance)
  (define (single-step sc variance)
    (define ((maybe/co reduce) sc)
      (case variance
        [(covariant) (reduce sc)]
        [(contravariant invariant) sc]
        [else (error 'maybe/co "Bad variance ~a" variance)]))

    ((maybe/co flat-reduce) ((maybe/co covariant-any/sc-reduce) (any/sc-reduce sc))))

  (define ((recur current-variance) sc variance)
    (define new-variance (combine-variance current-variance variance))
    (single-step (sc-map sc (recur new-variance)) new-variance))
  ((recur variance) sc 'covariant))


(module+ test
  (require rackunit)
  (provide optimizer-tests)
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


  (define optimizer-tests
    (test-suite "Optimizer Tests"
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
        (case->/sc (list (arr/sc (list list?/sc) (listof/sc set?/sc) #f)))))))
