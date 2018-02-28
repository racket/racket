#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "../boot/runtime-primitive.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt")

;; Check whether a module fits the restricted grammar of a cross-phase
;; persistent module

(provide check-cross-phase-persistent-form)

(define (check-cross-phase-persistent-form bodys self-mpi)

  (define (check-body bodys)
    (for ([body (in-list bodys)])
      (define p (if (expanded+parsed? body)
                    (expanded+parsed-parsed body)
                    body))
      (cond
        [(parsed-define-values? p)
         (check-expr (parsed-define-values-rhs p) (length (parsed-define-values-syms p)) p)]
        [(or (parsed-#%declare? p)
             (parsed-module? p)
             (syntax? p)) ;; remaining unparsed forms, such as `#%require` and `#%provide`, are ok
         (void)]
        [else
         (disallow p)])))

  (define (check-expr e num-results enclosing)
    (cond
      [(parsed-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-case-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-quote? e)
       (check-datum (parsed-quote-datum e) e)
       (check-count 1 num-results enclosing)]
      [(parsed-app? e)
       (define rands (parsed-app-rands e))
       (for ([rand (in-list rands)])
         (check-expr rand 1 e))
       (case (cross-phase-primitive-name (parsed-app-rator e))
         [(cons list)
          (check-count 1 num-results enclosing)]
         [(make-struct-type)
          (check-count 5 num-results enclosing)]
         [(make-struct-type-property)
          (check-count 3 num-results enclosing)]
         [(gensym)
          (unless (or (= 0 (length rands))
                      (and (= 1 (length rands))
                           (quoted-string? (car rands))))
            (disallow e))]
         [(string->uninterned-symbol)
          (unless (and (= 1 (length rands))
                       (quoted-string? (car rands)))
            (disallow e))]
         [else (disallow e)])]
      [else (check-no-disallowed-expr e)]))

  (define (check-no-disallowed-expr e)
    (cond
      [(parsed-lambda? e)
       (check-body-no-disallowed-expr (parsed-lambda-body e))]
      [(parsed-case-lambda? e)
       (for ([clause (in-list (parsed-case-lambda-clauses e))])
         (check-body-no-disallowed-expr (cadr clause)))]
      [(parsed-app? e)
       (check-no-disallowed-expr (parsed-app-rator e))
       (for ([e (in-list (parsed-app-rands e))])
         (check-no-disallowed-expr e))]
      [(parsed-if? e)
       (check-no-disallowed-expr (parsed-if-tst e))
       (check-no-disallowed-expr (parsed-if-thn e))
       (check-no-disallowed-expr (parsed-if-els e))]
      [(parsed-set!? e)
       (define id (parsed-set!-id e))
       (define normal-b (parsed-id-binding id))
       (when (or (not normal-b)
                 (parsed-top-id? id)
                 (and (not (symbol? normal-b))
                      (eq? (module-binding-module normal-b) self-mpi)))
         (disallow e))
       (check-no-disallowed-expr (parsed-set!-rhs e))]
      [(parsed-with-continuation-mark? e)
       (check-no-disallowed-expr (parsed-with-continuation-mark-key e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-val e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-body e))]
      [(parsed-begin? e)
       (check-body-no-disallowed-expr (parsed-begin-body e))]
      [(parsed-begin0? e)
       (check-body-no-disallowed-expr (parsed-begin0-body e))]
      [(parsed-let_-values? e)
       (for ([clause (in-list (parsed-let_-values-clauses e))])
         (check-no-disallowed-expr (cadr clause)))
       (check-body-no-disallowed-expr (parsed-let_-values-body e))]
      [(or (parsed-quote-syntax? e)
           (parsed-#%variable-reference? e))
       (disallow e)]
      ;; Other forms have no subexpressions
      [else (void)]))

  (define (check-body-no-disallowed-expr l)
    (for ([e (in-list l)])
      (check-no-disallowed-expr e)))

  (check-body bodys))

(define (check-count is-num expected-num enclosing)
  (unless (= is-num expected-num)
    (disallow enclosing)))

(define (check-datum d e)
  (cond
    [(or (number? d) (boolean? d) (symbol? d) (string? d) (bytes? d) (null? d))
     (void)]
    [else (disallow e)]))

(define (quoted-string? e)
  (and (parsed-quote? e)
       (string? (parsed-quote-datum e))))

(define (cross-phase-primitive-name id)
  (cond
    [(parsed-id? id)
     (define b (parsed-id-binding id))
     (and (module-binding? b)
          (eq? runtime-module-name (module-path-index-resolve (module-binding-module b)))
          (module-binding-sym b))]
    [else #f]))

(define (disallow body)
  (raise-syntax-error 'module
                      "not allowed in a cross-phase persistent module"
                      (if (parsed? body)
                          (datum->syntax #f body (parsed-s body))
                          body)))
