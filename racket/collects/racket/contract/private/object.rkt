#lang racket/base
(require "arrow.rkt"
         "arr-i.rkt"
         "guts.rkt"
         "prop.rkt"
         "misc.rkt"
         "opt.rkt"
         "blame.rkt"
         (for-syntax "opt-guts.rkt")
         racket/private/class-internal
         racket/private/class-c-old
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
  #:property prop:custom-write custom-write-property-proc
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
       (with-syntax ([(method-name ...)
                      (map (λ (x) (string->symbol (format "~a method" (syntax-e x))))
                           (syntax->list #'(method-id ...)))])
         #'(build-object-contract '(method-id ...)
                                  (syntax-parameterize
                                   ((making-a-method #t))
                                   (list (let ([method-name method-ctc]) method-name) ...))
                                  '(field-id ...)
                                  (list field-ctc ...))))]))

(define (build-object-contract methods method-ctcs fields field-ctcs)
  (make-object-contract methods 
                        (map (λ (x) (coerce-contract 'object-contract x)) method-ctcs)
                        fields 
                        (map (λ (x) (coerce-contract 'object-contract x)) field-ctcs)))


(define (make-mixin-contract . %/<%>s)
  (->i ([c% (and/c (flat-contract class?)
                   (apply and/c (map sub/impl?/c %/<%>s)))])
       [res (c%) (subclass?/c c%)]))

(struct subclass/c (%)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (define % (subclass/c-% ctc)) (λ (x) (subclass? x %)))
   #:stronger (λ (this that)
                (cond
                  [(subclass/c? that)
                   (subclass? (subclass/c-% this) (subclass/c-% that))]
                  [else #f]))
   #:name (λ (ctc) `(subclass?/c ,(or (object-name (subclass/c-% ctc)) 'unknown%)))))
(define (subclass?/c %)
  (unless (class? %)
    (raise-argument-error 'subclass?/c "class?" %))
  (subclass/c %))

(struct implementation/c (<%>)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (define <%> (implementation/c-<%> ctc)) (λ (x) (implementation? x <%>)))
   #:stronger (λ (this that)
                (cond
                  [(implementation/c? that)
                   (interface-extension? (implementation/c-<%> this)
                                         (implementation/c-<%> that))]
                  [else #f]))
   #:name (λ (ctc) `(implementation?/c ,(or (object-name (implementation/c-<%> ctc)) 'unknown<%>)))))
            
(define (implementation?/c <%>)
  (unless (interface? <%>)
    (raise-argument-error 'implementation?/c "interface?" <%>))
  (implementation/c <%>))

(define (sub/impl?/c %/<%>)
  (cond
    [(interface? %/<%>) (implementation?/c %/<%>)]
    [(class? %/<%>) (subclass?/c %/<%>)]
    [else
     (raise-argument-error
      'make-mixin-contract
      (format "~s" '(or/c interface? class?))
      %/<%>)]))

(struct is-a?-ctc (<%>)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order
   (λ (ctc)
     (define <%> (is-a?-ctc-<%> ctc))
     (λ (x) (is-a? x <%>)))
   #:stronger
   (λ (this that)
     (define this-<%> (is-a?-ctc-<%> this))
     (cond
       [(is-a?-ctc? that)
        (define that-<%> (is-a?-ctc-<%> that))
        (cond
          [(and (class? this-<%>) (class? that-<%>))
           (subclass? this-<%> that-<%>)]
          [(and (class? this-<%>) (interface? that-<%>))
           (implementation? this-<%> that-<%>)]
          [(and (interface? this-<%>) (interface? that-<%>))
           (interface-extension? this-<%> that-<%>)]
          [else #f])]
       [else #f]))
   #:name
   (λ (ctc)
     (define <%> (is-a?-ctc-<%> ctc))
     (define name (object-name <%>))
     (cond
       [name `(is-a?/c ,name)]
       [(class? <%>) `(is-a?/c unknown%)]
       [else `(is-a?/c unknown<%>)]))))


(define (is-a?/c <%>)
  (check-is-a?/c <%>)
  (is-a?-ctc <%>))
  
(define mixin-contract (->i ([c% class?]) [res (c%) (subclass?/c c%)]))

(define/opter (is-a?/c opt/i opt/info stx)
  (syntax-case stx ()
    [(_ cls) 
     (let ()
       (define-values (lift-cls lifts1) (lift/binding #'cls 'is-a?/c-cls empty-lifts))
       (with-syntax ([cls-x lift-cls])
         (define lifts2 (lift/effect #'(check-is-a?/c cls-x) lifts1))
         (with-syntax ([val (opt/info-val opt/info)]
                       [ctc (opt/info-contract opt/info)]
                       [blame (opt/info-blame opt/info)]
                       [this (opt/info-this opt/info)]
                       [that (opt/info-that opt/info)])
           (build-optres
            #:exp #'(if (is-a? val cls-x)
                        val
                        (raise-is-a?/c-error val cls-x blame))
            #:lifts lifts2
            #:superlifts null
            #:partials null
            #:flat #'(is-a? val cls-x)
            #:opt #f
            #:stronger-ribs '()
            #:chaperone #t
            #:name #'`(is-a?/c ,(object-name cls-x))))))]
    [_ (opt/unknown opt/i opt/info stx)]))

(define (raise-is-a?/c-error val cls-x blame)
  (raise-blame-error blame val
                     '(expected: "a class matching ~e" given: "~e")
                     cls-x val))

(define (check-is-a?/c <%>)
  (unless (or (interface? <%>) (class? <%>))
    (raise-argument-error
     'is-a?/c
     (format "~s" '(or/c interface? class?))
     <%>)))
  