(module simple frtime
  
  (require "fred.rkt"
           racket/class
           (rename-in mred [frame% frame%])
           (for-syntax racket/base))
  
  (define widget (lambda (x) x))
  (define value-b (lambda (x) (send x get-value-b)))
  (define value-e (lambda (x) (send x get-value-e)))
  
  (define default-parent
    (let ([fr #f])
      (lambda ()
        (unless fr
          (set! fr (new ft-frame%)))
        fr)))
  
  (define creation-filter (make-parameter value-b
                                          (lambda (f) (if (and (procedure? f) (procedure-arity-includes? f 1))
                                                          f
                                                          (error 'creation-filter
                                                                 "expected a procedure of arity 1")))))
  
  (define current-widget-parent (make-parameter #f))
  
  (define-syntax add-widget
    (syntax-rules ()
      [(_ type arg ...)
       ((creation-filter) (new type (parent (current-widget-parent)) arg ...))]))
  
  
  (define (filter-widget w)
    ((creation-filter) w))
  
  (define-syntax mode
    (syntax-rules ()
      [(_ proc type arg ...) (parameterize ([creation-filter proc])
                               (add-widget type arg ...))]))
  
  (define-syntax define-values-rec
    (syntax-rules ()
      [(_ [id0 exp0] [id exp] ...)
       (define-values (id0 id ...)
         (letrec ([id0 exp0]
                  [id exp] ...)
           (values id0 id ...)))]))
  


  
  (provide (all-defined-out)
           (all-from-out "fred.rkt")
           (all-from-out racket/class)))
