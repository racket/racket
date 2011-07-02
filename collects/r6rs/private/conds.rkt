#lang scheme/base

(require rnrs/records/syntactic-6
         rnrs/records/procedural-6
         scheme/mpair
         "exns.rkt"
         (for-syntax scheme/base))

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
         &undefined make-undefined-violation undefined-violation?

         &i/o make-i/o-error i/o-error?
         &i/o-read make-i/o-read-error i/o-read-error?
         &i/o-write make-i/o-write-error i/o-write-error?
         &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
         &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
         &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
         &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
         &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
         &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
         &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port)

(define-record-type &condition (fields))

(define-struct (compound-condition exn) (conditions) #:transparent)
(define-struct (compound-condition:fail exn:fail) (conditions) #:transparent)

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
         (apply append
                (map simple-conditions/list conds))])
    ((if (ormap serious-condition? conditions)
         make-compound-condition:fail
         make-compound-condition)
     (or (ormap (lambda (c)
                  (and (message-condition? c)
                       (condition-message c)))
                conditions)
         "exception")
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
           (ormap pred (simple-conditions/list v))))))

(define (condition-accessor rtd proc)
  (let ([pred (record-predicate rtd)])
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-type-error 'condition-accessor "procedure (arity 1)" proc))
    (lambda (v)
      (let ([v (ormap (lambda (x) 
                        (and (pred x) x))
                      (simple-conditions/list v))])
        (if v
            (proc v)
            (raise-type-error 'a-condition-accessor "specific kind of condition" v))))))

(define (simple-conditions/list c)
  (cond
   [(&condition? c) (list c)]
   [(compound-condition? c)
    (compound-condition-conditions c)]
   [(compound-condition:fail? c)
    (compound-condition:fail-conditions c)]
   [(exn? c)
    (append
     (list
      (make-message-condition (cond
                               [(exn:fail:r6rs? c)
                                (exn:fail:r6rs-message c)]
                               [(exn:fail:contract:r6rs? c)
                                (exn:fail:contract:r6rs-message c)]
                               [(exn:fail:syntax:r6rs? c)
                                (exn:fail:syntax:r6rs-message c)]
                               [else (exn-message c)]))
      (make-has-continuation-marks (exn-continuation-marks c)))
     (if (and (exn:fail? c)
              (not (exn:fail:contract? c)))
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
          (list (make-irritants-condition (list->mlist (exn:fail:r6rs-irritants c)))))
         null)
     (if (exn:fail:contract:r6rs? c)
         (append
          (if (exn:fail:contract:r6rs-who c)
              (list (make-who-condition (exn:fail:contract:r6rs-who c)))
              null)
          (list (make-irritants-condition (list->mlist (exn:fail:contract:r6rs-irritants c)))))
         null)
     (if (or (exn:fail:unsupported? c)
             (exn:fail:contract:divide-by-zero? c)
             (exn:fail:contract:non-fixnum-result? c))
         (list (make-implementation-restriction-violation))
         null)
     (if (exn:fail:read? c)
         (list (make-lexical-violation))
         null)
     (if (exn:fail:syntax? c)
         (if (exn:fail:syntax:r6rs? c)
             (append
              (list (make-syntax-violation
                     (exn:fail:syntax:r6rs-form c)
                     (exn:fail:syntax:r6rs-subform c)))
              (if (exn:fail:syntax:r6rs-who c)
                  (list (make-who-condition (exn:fail:syntax:r6rs-who c)))
                  null))
             (let ([forms (exn:fail:syntax-exprs c)])
               (list (make-syntax-violation
                      (if (pair? forms)
                          (car forms)
                          #f)
                      (if (and (pair? forms)
                               (pair? (cdr forms)))
                          (cadr forms)
                          #f)))))
         null)
     (if (exn:fail:contract:variable? c)
         (list (make-undefined-violation))
         null)
     (if (exn:fail:filesystem:exists? c)
         (list (make-i/o-file-already-exists-error "???"))
         null)
     (if (exn:fail:filesystem:exists-not? c)
         (list (make-i/o-file-does-not-exist-error 
                (exn:fail:filesystem:exists-not-filename
                 c)))
         null)
     (if (exn:fail:contract:non-continuable? c)
         (list (make-non-continuable-violation))
         null))]
   [else (raise-type-error 'simple-conditions
                           "condition"
                           c)]))

(define (simple-conditions c)
  (list->mlist (simple-conditions/list c)))

(define-syntax (define-condition-type stx)
  (syntax-case stx ()
    [(_ type supertype
        constructor predicate
        (field accessor) ...)
     (with-syntax ([(tmp-acc ...) (generate-temporaries #'(field ...))])
       #'(begin
           (define-record-type (type constructor base-predicate)
             (fields (immutable field tmp-acc) ...)
             (parent supertype))
           (define predicate (condition-predicate type))
           (define accessor (condition-accessor type tmp-acc)) ...))]))

(define-condition-type &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type &cont-marks &condition
  make-has-continuation-marks has-continuation-marks?
  (marks has-continuation-marks-marks))

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

;; ----------------------------------------
;; i/o

(define-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)

(define-condition-type &i/o &error
  make-i/o-error i/o-error?)

(define-condition-type &i/o-read &i/o
  make-i/o-read-error i/o-read-error?)

(define-condition-type &i/o-write &i/o
  make-i/o-write-error i/o-write-error?)

(define-condition-type &i/o-invalid-position &i/o
  make-i/o-invalid-position-error
  i/o-invalid-position-error?
  (position i/o-error-position))

(define-condition-type &i/o-filename &i/o
  make-i/o-filename-error i/o-filename-error?
  (filename i/o-error-filename))

(define-condition-type &i/o-file-protection
  &i/o-filename
  make-i/o-file-protection-error
  i/o-file-protection-error?)

(define-condition-type &i/o-file-is-read-only
  &i/o-file-protection
  make-i/o-file-is-read-only-error
  i/o-file-is-read-only-error?)

(define-condition-type &i/o-file-already-exists
  &i/o-filename
  make-i/o-file-already-exists-error
  i/o-file-already-exists-error?)

(define-condition-type &i/o-file-does-not-exist
  &i/o-filename
  make-i/o-file-does-not-exist-error
  i/o-file-does-not-exist-error?)

(define-condition-type &i/o-port &i/o
  make-i/o-port-error i/o-port-error?
  (port i/o-error-port))
