#lang racket/base
(require "match.rkt")

(provide extract-functions
         (struct-out function))

;; A "function" is a known lambda that is bound at the top level. When
;; finding functions, we flatten `let[rec[*]]`s into the top level
;; when they're not under `lambda`, which exposes constant functions
;; that have been lifted by schemify's "lift" pass.

(struct function (e))

(define (extract-functions knowns e lambdas)
  (match e
    [`(define ,id ,rhs)
     (define-values (new-knowns just-functions?)
       (extract-expr-functions knowns rhs id lambdas))
     new-knowns]
    [`(begin ,es ...)
     (for/fold ([knowns knowns]) ([e (in-list es)])
       (extract-functions knowns e lambdas))]
    [`(define-values ,ids ,rhs)
     (define-values (new-knowns just-functions?)
       (extract-expr-functions knowns rhs #f lambdas))
     new-knowns]
    [`,_
     (define-values (new-knowns just-functions?)
       (extract-expr-functions knowns e #f lambdas))
     new-knowns]))

(define-syntax-rule (not-just-functions e)
  (let-values ([(new-knowns just-functions?) e])
    (values new-knowns #f)))

;; Returns second result is #f if `e` is not just functions
(define (extract-expr-functions knowns e id lambdas)
  (match e
    [`(lambda . ,_)
     (values (if id
                 (hash-set knowns id (function e))
                 knowns)
             #t)]
    [`(case-lambda . ,_)
     (values (if id
                 (hash-set knowns id (function e))
                 knowns)
             #t)]
    [`(let . ,_)
     (extract-let-functions knowns e id lambdas)]
    [`(letrec . ,_)
     (extract-let-functions knowns e id lambdas)]
    [`(letrec* . ,_)
     (extract-let-functions knowns e id lambdas)]
    [`(begin ,e)
     (extract-expr-functions knowns e id lambdas)]
    [`(begin ,e . ,r)
     (define-values (new-knowns just-functions?)
       (extract-expr-functions knowns e #f lambdas))
     (define-values (new-knowns2 just-functions2?)
       (extract-expr-functions new-knowns `(begin . ,r) id lambdas))
     (values new-knowns2 (and just-functions? just-functions2?))]
    [`(if ,tst ,thn ,els)
     (not-just-functions
      (extract-expr-functions knowns `(begin ,tst ,thn ,els) #f lambdas))]
    [`(with-continuation-marks ,tst ,thn ,els)
     (not-just-functions
      (extract-expr-functions knowns `(with-continuation-marks ,tst ,thn ,els) #f lambdas))]
    [`(set! ,id ,rhs)
     (not-just-functions
      (extract-expr-functions knowns rhs #f lambdas))]
    [`(#%app . ,r)
     (not-just-functions
      (extract-expr-functions knowns `(begin . ,r) #f lambdas))]
    [`(values ,r)
     (extract-expr-functions knowns r id lambdas)]
    [`(values . ,r)
     (extract-expr-functions knowns `(begin . ,r) #f lambdas)]
    [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
     (define-values (new-knowns just-functions1?)
       (extract-expr-functions knowns `(begin . ,body1) #f lambdas))
     (define-values (new-functions2 just-functions2?)
       (extract-expr-functions new-knowns `(begin . ,body2) (and just-functions1? id) lambdas))
     (values new-knowns (and just-functions1? just-functions2?))]
    [`(,rator ,rands ...)
     (not-just-functions
      (extract-expr-functions knowns `(begin . ,e) #f lambdas))]
    [`,_
     (cond
       [(function? (hash-ref knowns e #f))
        (values knowns #t)]
       [else
        (values knowns #f)])]))

(define (extract-let-functions knowns e id lambdas)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define-values (new-knowns just-functions?)
       (for/fold ([knowns knowns]
                  [just-functions? #t])
                 ([id (in-list ids)]
                  [rhs (in-list rhss)])
         ;; The "lift" pass uses `letrec` (as opposed to `letrec*`) to
         ;; bind a function that can be computed once at the top.
         (define function-id (and (not (eq? let-id 'letrec*)) id))
         (define-values (new-knowns new-just-functions?)
           (extract-expr-functions knowns rhs function-id lambdas))
         (values new-knowns (and just-functions? new-just-functions?))))
     (extract-expr-functions new-knowns `(begin . ,body) (and just-functions? id) lambdas)]))
