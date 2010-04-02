#lang scheme/base
(require (for-syntax scheme/base))

(provide new-define-struct)

(define-syntax (new-define-struct stx)
  (define (config-has-name? config)
    (cond
     [(syntax? config) (config-has-name? (syntax-e config))]
     [(pair? config) (or (eq? (syntax-e (car config)) '#:constructor-name)
                         (config-has-name? (cdr config)))]
     [else #f]))
  (with-syntax ([orig stx])
    (syntax-case stx ()
      [(_ id+super fields . config)
       (not (config-has-name? #'config))
       (with-syntax ([id (syntax-case #'id+super ()
                           [(id super) #'id]
                           [else #'id+super])])
         (syntax/loc stx
           (define-struct/derived orig id+super fields  #:constructor-name id . config)))]
      [_ (syntax/loc stx
           (define-struct/derived orig id+super fields . config))])))
