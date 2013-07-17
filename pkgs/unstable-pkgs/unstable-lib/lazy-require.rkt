#lang racket/base
(require (for-syntax racket/base)
         racket/lazy-require)
(provide lazy-require
         begin-on-demand)

#|
begin-on-demand notes/todo

Currently like lazy-require: only supports functions.

The #:export kw is clunky... one might think it would be nice to just
re-use 'provide' syntax. Would get rename, prefix, etc for free. OTOH,
might be misleading: might seem like provide should apply to
*apparent* enclosing module, not module implicitly created by
begin-on-demand.

Another nice idea would be to implement lazy-require in terms of
begin-on-demand and real require. Unfortunately, that would mean we
couldn't have cyclic lazy-requires, which is very useful.
|#

(define-syntax (begin-on-demand stx)
  (syntax-case stx ()
    [(begin-on-demand #:export (exp ...) body ...)
     (with-syntax ([fresh-name (gensym 'on-demand-submodule)])
       #'(begin
           (module* fresh-name #f
             (no-provide body ...)
             (check-procedure exp 'exp) ...
             (provide exp ...))
           (define-namespace-anchor anchor)
           (define get-sym
             (let ([sub-mpi
                    (module-path-index-join '(submod "." fresh-name)
                                            (variable-reference->resolved-module-path
                                             (#%variable-reference)))])
               (lambda (sym)
                 (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
                   (dynamic-require sub-mpi sym)))))
           (define exp (make-lazy-function 'exp get-sym)) ...))]))

(define-syntax (no-provide stx)
  (syntax-case stx ()
    [(_ form)
     (let ([eform (local-expand #'form
                                (syntax-local-context)
                                #f)])
       (syntax-case eform (begin #%provide)
         [(begin e ...)
          #'(begin (no-provide e) ...)]
         [(#%provide . _)
          (raise-syntax-error #f "provide not allowed" eform)]
         [_ eform]))]
    [(_ form ...)
     #'(begin (no-provide form) ...)]))

(define (check-procedure x name)
  (unless (procedure? x)
    (error 'begin-on-demand "name bound on-demand is not a procedure: ~a" name)))
