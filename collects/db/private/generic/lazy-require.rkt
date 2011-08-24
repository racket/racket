#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path
         racket/promise)
(provide define-lazy-require-definer)

(define-syntax (define-lazy-require-definer stx)
  (syntax-case stx ()
    [(_ name modpath)
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected identifier" stx #'name))
       #'(begin (define-runtime-module-path-index mpi-var modpath)
                (define-syntax name (make-lazy-require-definer #'mpi-var))))]))

(define-for-syntax (make-lazy-require-definer mpi-var)
  (lambda (stx)
    (syntax-case stx ()
      [(_ fun ...)
       (begin
         (for ([fun (in-list (syntax->list #'(fun ...)))])
           (unless (identifier? fun)
             (raise-syntax-error #f "expected identifier for function name" stx fun)))
         (with-syntax ([(fun-p ...) (generate-temporaries #'(fun ...))]
                       [mpi-var mpi-var])
           ;; Use 'delay/sync' because 'delay' promise is not reentrant.
           ;; FIXME: OTOH, 'delay/sync' promise is not kill-safe.
           #'(begin (define fun-p (delay/sync (dynamic-require mpi-var 'fun)))
                    ...
                    (define fun (make-delayed-function 'fun fun-p))
                    ...)))])))

(define (make-delayed-function name fun-p)
  (procedure-rename
   (make-keyword-procedure
    (lambda (kws kwargs . args)
      (keyword-apply (force fun-p) kws kwargs args)))
   name))
