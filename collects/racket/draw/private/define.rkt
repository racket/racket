#lang racket/base
(require (for-syntax racket/base)
         ffi/unsafe)

(provide define-definer
         define-private-definer
         define-enum
         define/provide)

(define-syntax define-enum
  (syntax-rules ()
    [(_ n) (begin)]
    [(_ n id . ids) (begin
                      (define id n)
                      (provide id)
                      (define-enum (+ n 1) . ids))]))
(define-syntax-rule (define/provide id val)
  (begin
    (define id val)
    (provide id)))

(define-syntax-rule (skip id) (begin))

(define-syntax-rule (define-definer LIB ffi-lib) 
  (define-definer* LIB ffi-lib #t))
(define-syntax-rule (define-private-definer LIB ffi-lib) 
  (define-definer* LIB ffi-lib #f))

(define (make-not-found id)
  (lambda args
    (error id "implementation not found; arguments where ~e" args)))

(define (trace id proc)
  proc
  #;
  (lambda args
    (printf "~s\n" id)
    (apply proc args)))

(define-syntax (define-definer* stx)
  (syntax-case stx ()
    [(_ LIB ffi-lib p?)
     (let ([make-id
            (lambda (tmpl)
              (datum->syntax #'LIB
                             (string->symbol (format tmpl (syntax-e #'LIB)))
                             #'LIB))])
       (with-syntax ([define-LIB/private (make-id "define-~a/private")]
                     [define-LIB (make-id "define-~a")]
                     [PROVIDE (if (syntax-e #'p?)
                                  #'provide
                                  #'skip)])
         #`(begin
             (define-syntax define-LIB/private
               (syntax-rules ()
                 [(_ id c-id type fail #:wrap wrapper)
                  (define id (trace 'c-id (wrapper (get-ffi-obj 'c-id ffi-lib type fail))))]
                 [(_ id c-id type fail)
                  (define-LIB/private id c-id type fail #:wrap values)]
                 [(_ id type fail #:wrap wrapper)
                  (define-LIB/private id id type fail #:wrap wrapper)]
                 [(_ id type fail)
                  (define-LIB/private id id type fail #:wrap values)]
                 [(_ id type #:wrap wrapper)
                  (define-LIB/private id id type (lambda () (make-not-found 'id)) #:wrap wrapper)]
                 [(_ id type)
                  (define-LIB/private id id type (lambda () (make-not-found 'id)) #:wrap values)]))

             (define-syntax define-LIB
               (syntax-rules ()
                 [(_ id type default #:wrap wrapper)
                  (begin
                    (PROVIDE id)
                    (define-LIB/private id id type (lambda () default) #:wrap wrapper))]
                 [(_ id type #:wrap wrapper)
                  (define-LIB id type (make-not-found 'id) #:wrap wrapper)]
                 [(_ id type)
                  (define-LIB id type (make-not-found 'id) #:wrap values)]
                 [(_ id type default)
                  (define-LIB id type default #:wrap values)])))))]))
