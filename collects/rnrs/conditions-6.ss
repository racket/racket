#lang scheme/base

(require rnrs/records/syntactic-6
         rnrs/records/procedural-6
         r6rs/private/conds)

(provide &condition
         condition?
         condition
         simple-conditions
         condition-predicate
         condition-accessor
         define-condition-type

         &message make-message-condition message-condition? condition-message
         &warning make-warning warning?
         &serious make-serious-condition serious-condition?
         &error make-error error?
         &violation make-violation violation?
         &assertion make-assertion-violation assertion-violation?
         &irritants make-irritants-condition irritants-condition? condition-irritants
         &who make-who-condition who-condition? condition-who
         &non-continuable make-non-continuable-violation non-continuable-violation?
         &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
         &lexical make-lexical-violation lexical-violation?
         &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
         &undefined make-undefined-violation undefined-violation?)

(define-record-type &condition (fields))

(define-struct (compound-condition exn) (conditions))
(define-struct (compound-condition:fail exn:fail) (conditions))

(define-struct has-continuation-marks (marks))

(define (condition? v)
  (or (&condition? v)
      (compound-condition? v)
      (compound-condition:fail? v)
      (exn? v)))

(define (condition . conds)
  (for-each (lambda (c)
              (unless (condition? c)
                (raise-type-error 'condition "condition" c)))
            conds)
  (let ([conditions
         (make-compound-condition
          (apply append
                 (map simple-conditions conds)))])
    ((if (ormap serious-condition? conditions)
         make-compound-condition:fail
         make-compound-condition)
     (ormap (lambda (c)
              (and (message-condition? c)
                   (condition-message c)))
            conditions)
     (or (ormap (lambda (c)
                  (and (has-continuation-marks? c)
                       (has-continuation-marks-marks c)))
                conditions)
         (current-continuation-marks))
     conditions)))

(define (condition-predicate rtd)
  (let ([pred (record-predicate rtd)])
    (lambda (v)
      (and (condition? v)
           (ormap pred (simple-conditions v))))))

(define (condition-accessor rtd proc)
  (let ([pred (record-predicate rtd)])
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-type-error 'condition-accessor "procedure (arity 1)" proc))
    (lambda (v)
      (let ([v (ormap pred (simple-conditions v))])
        (if v
            (proc v)
            (raise-type-error 'a-condition-accessor "specific kind of condition" v))))))

(define (simple-conditions c)
  (cond
   [(&condition? c) (list c)]
   [(compound-condition? c)
    (compound-condition-conditions c)]
   [(compound-condition:fail? c)
    (compound-condition:fail-conditions c)]
   [(exn? c)
    (append
     (list
      (make-message-condition (exn-message c))
      (make-has-continuation-marks (exn-continuation-marks c)))
     (if (exn:fail? c)
         (list (make-error))
         null)     
     (if (exn:fail:contract? c)
         (list (make-assertion-violation))
         null)
     (if (exn:fail:r6rs? c)
         (append
          (if (exn:fail:r6rs-who c)
              (list (make-who-condition (exn:fail:r6rs-who c)))
              null)
          (list (make-irritants-condition (exn:fail:r6rs-irritants c))))
         null)
     (if (exn:fail:contract:r6rs? c)
         (append
          (if (exn:fail:contract:r6rs-who c)
              (list (make-who-condition (exn:fail:contract:r6rs-who c)))
              null)
          (list (make-irritants-condition (exn:fail:contract:r6rs-irritants c))))
         null)
     (list (make-non-continuable-violation))
     (if (exn:fail:unsupported? c)
         (list (make-implementation-restriction-violation))
         null)
     (if (exn:fail:read? c)
         (list (make-lexical-violation))
         null)
     (if (exn:fail:syntax? c)
         (let ([forms (exn:fail:syntax-exprs c)])
           (list (make-syntax-violation
                  (if (pair? forms)
                      (car forms)
                      #f)
                  (if (and (pair? forms)
                           (pair? (cdr forms)))
                      (cadr forms)
                      #f))))
         null)
     (if (exn:fail:contract:variable? c)
         (make-undefined-violation)
         null))]
   [else (raise-type-error 'simple-conditions
                           "condition"
                           c)]))


(define-syntax-rule (define-condition-type type supertype
                      constructor predicate
                      field ...)
  (define-record-type (type constructor predicate)
    (fields (immutable . field) ...)
    (parent supertype)))

(define-condition-type &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type &warning &condition
  make-warning warning?)

(define-condition-type &serious &condition
  make-serious-condition serious-condition?)

(define-condition-type &error &serious
  make-error error?)

(define-condition-type &violation &serious
  make-violation violation?)

(define-condition-type &assertion &violation
  make-assertion-violation assertion-violation?)

(define-condition-type &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-condition-type &who &condition
  make-who-condition who-condition?
  (who condition-who))

(define-condition-type &non-continuable &violation
  make-non-continuable-violation
  non-continuable-violation?)

(define-condition-type &implementation-restriction
    &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-condition-type &lexical &violation
  make-lexical-violation lexical-violation?)

(define-condition-type &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)
