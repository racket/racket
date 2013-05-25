#lang scheme/base

;; used for quasiquote and quasisyntax

(require (for-syntax scheme/base))

(provide define-generalized-qq)

(define-syntax-rule (define-generalized-qq r6rs:quasiquote 
                      quasiquote unquote unquote-splicing uq-wrap)
  (...
   (define-syntax (r6rs:quasiquote stx)
     ;; Replace (unquote expr ...) with (unquote expr) ...
     (syntax-case stx ()
       [(_ tmpl)
        (let ([new-tmpl
               (let loop ([tmpl #'tmpl][level 0])
                 (syntax-case tmpl (r6rs:quasiquote)
                   [((u expr ...) . rest)
                    (and (identifier? #'u)
                         (or (free-identifier=? #'u #'unquote)
                             (free-identifier=? #'u #'unquote-splicing)))
                    (let ([new-rest (loop #'rest level)])
                      (if (zero? level)
                          (if (and (eq? new-rest #'rest)
                                   (= 1 (length (syntax->list #'(expr ...))))
                                   (free-identifier=? #'uq-wrap #'values))
                              tmpl
                              (datum->syntax
                               tmpl
                               (append (let ([a (car (syntax-e tmpl))])
                                         (map (lambda (expr)
                                                (datum->syntax
                                                 a
                                                 (list (car (syntax-e a))
                                                       (list (syntax uq-wrap)
                                                             expr))
                                                 a a a))
                                              (syntax->list #'(expr ...))))
                                       new-rest)
                               tmpl tmpl tmpl))
                          (let* ([first (car (syntax-e tmpl))]
                                 [new-first (loop first (sub1 level))])
                            (if (and (eq? new-first first)
                                     (eq? new-rest #'rest))
                                tmpl
                                (datum->syntax
                                 tmpl
                                 (cons new-first new-rest)
                                 tmpl tmpl tmpl)))))]
                   [(r6rs:quasiquote expr)
                    (let ([new-expr (loop #'(expr) (add1 level))])
                      ;; We have to replace the old qq with the new one:
                      (datum->syntax
                       tmpl
                       (cons (datum->syntax #'quasiquote
                                            'quasiquote
                                            (car (syntax-e tmpl)))
                             new-expr)
                       tmpl tmpl tmpl))]
                   [(a . b)
                    (let ([new-a (loop #'a level)]
                          [new-b (loop #'b level)])
                      (if (and (eq? new-a #'a)
                               (eq? new-b #'b))
                          tmpl
                          (datum->syntax
                           tmpl
                           (cons new-a new-b)
                           tmpl tmpl tmpl)))]
                   [#(a ...)
                    (let* ([as (syntax->list #'(a ...))]
                           [new-as (loop as level)])
                      (if (eq? as new-as)
                          tmpl
                          (datum->syntax
                           tmpl
                           (list->vector new-as)
                           tmpl tmpl tmpl)))]
                   [_ tmpl]))])
          (datum->syntax
           stx
           (list #'quasiquote new-tmpl)
           stx stx stx))]))))
