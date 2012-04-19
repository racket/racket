#lang racket/base
(require "arrow.rkt"
         "guts.rkt"
         "prop.rkt"
         "misc.rkt"
         racket/private/class-internal
         racket/stxparam)

(require (for-syntax racket/base))

(provide mixin-contract
         make-mixin-contract
         is-a?/c
         subclass?/c
         implementation?/c
         object-contract)

(define-for-syntax (parse-object-contract stx args)
  (let loop ([args (syntax->list args)]
             [mtds '()]
             [flds '()])
    (cond
      [(null? args) (list mtds flds)]
      [else (syntax-case (car args) (field)
              [(field id ctc)
               (identifier? #'id)
               (loop (cdr args) mtds (cons #'(id ctc) flds))]
              [(field . rst)
               (raise-syntax-error #f "malformed field specification" stx (car args))]
              [(id ctc)
               (identifier? #'id)
               (loop (cdr args) (cons #`(id ctc) mtds) flds)]
              [_ 
               (raise-syntax-error #f "malformed object-contract clause" stx (car args))])])))

(define-struct object-contract (methods method-ctcs fields field-ctcs)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (make-wrapper-object ctc val blame
                              (object-contract-methods ctc) (object-contract-method-ctcs ctc)
                              (object-contract-fields ctc) (object-contract-field-ctcs ctc)))))
   #:name
   (λ (ctc) `(object-contract ,@(map (λ (fld ctc) (build-compound-type-name 'field fld ctc))
                                     (object-contract-fields ctc)
                                     (object-contract-field-ctcs ctc))
                              ,@(map (λ (mtd ctc) (build-compound-type-name mtd ctc))
                                     (object-contract-methods ctc)
                                     (object-contract-method-ctcs ctc))))

   #:first-order 
   (λ (ctc)
     (λ (val)
       (let/ec ret
         (check-object-contract val (object-contract-methods ctc) (object-contract-fields ctc)
                                (λ args (ret #f))))))))

(define-syntax (object-contract stx)
  (syntax-case stx ()
    [(_ spec ...)
     (with-syntax ([(((method-id method-ctc) ...)
                     ((field-id field-ctc) ...))
                    (parse-object-contract stx #'(spec ...))])
       (with-syntax ([(method-name ...) (map (λ (x) (string->symbol (format "~a method" (syntax-e x)))) 
                                             (syntax->list #'(method-id ...)))])
         #'(build-object-contract '(method-id ...)
                                  (syntax-parameterize ((making-a-method #t)) (list (let ([method-name method-ctc]) method-name) ...))
                                  '(field-id ...)
                                  (list field-ctc ...))))]))

(define (build-object-contract methods method-ctcs fields field-ctcs)
  (make-object-contract methods 
                        (map (λ (x) (coerce-contract 'object-contract x)) method-ctcs)
                        fields 
                        (map (λ (x) (coerce-contract 'object-contract x)) field-ctcs)))


(define (make-mixin-contract . %/<%>s)
  (->d ([c% (and/c (flat-contract class?)
                   (apply and/c (map sub/impl?/c %/<%>s)))])
       ()
       [res (subclass?/c c%)]))

(define (subclass?/c %)
  (unless (class? %)
    (error 'subclass?/c "expected <class>, given: ~e" %))
  (let ([name (object-name %)])
    (flat-named-contract
     `(subclass?/c ,(or name 'unknown%))
     (lambda (x) (subclass? x %)))))

(define (implementation?/c <%>)
  (unless (interface? <%>)
    (error 'implementation?/c "expected <interface>, given: ~e" <%>))
  (let ([name (object-name <%>)])
    (flat-named-contract
     `(implementation?/c ,(or name 'unknown<%>))
     (lambda (x) (implementation? x <%>)))))

(define (sub/impl?/c %/<%>)
  (cond
    [(interface? %/<%>) (implementation?/c %/<%>)]
    [(class? %/<%>) (subclass?/c %/<%>)]
    [else (error 'make-mixin-contract "unknown input ~e" %/<%>)]))

(define (is-a?/c <%>)
  (unless (or (interface? <%>)
              (class? <%>))
    (error 'is-a?/c "expected <interface> or <class>, given: ~e" <%>))
  (let ([name (object-name <%>)])
    (flat-named-contract
     (cond
       [name
        `(is-a?/c ,name)]
       [(class? <%>)
        `(is-a?/c unknown%)]
       [else `(is-a?/c unknown<%>)])
     (lambda (x) (is-a? x <%>)))))

(define mixin-contract (->d ([c% class?]) () [res (subclass?/c c%)]))
