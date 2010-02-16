#lang scheme/base
(require "arrow.ss"
         "guts.ss"
         scheme/private/class-internal
         scheme/stxparam)

(require (for-syntax scheme/base))

(provide mixin-contract
         make-mixin-contract
         is-a?/c 
         subclass?/c 
         implementation?/c
         object-contract)

;; example of how one contract is constructed
#;
(let* ([cm (syntax-parameterize ((making-a-method #t)) (-> any/c integer? integer?))]
       [cf (-> integer? integer?)]
       [m-proj ((contract-projection cm)
                (make-blame #'here #f "whatever" 'pos 'neg #t))]
       [f-proj ((contract-projection cf)
                (make-blame #'here #f "whatever" 'pos 'neg #t))]
       [cls (make-wrapper-class 'wrapper-class
                                '(m)
                                (list 
                                 (m-proj (λ (this x) (send (wrapper-object-wrapped this) m x))))
                                '(f)
                                #f)]
       [o (new (class object%
                 (field [f (λ (x) x)])
                 (define/public (m x) x)
                 (super-new)))]
       [wo (make-object cls o (f-proj (get-field/proc 'f o)))])
  ((get-field/proc 'f wo) #f))

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

(define-struct object-contract (methods method-ctcs method-wrappers fields field-ctcs)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let ([meth-names (object-contract-methods ctc)]
            [meth-param-projs (map contract-projection (object-contract-method-ctcs ctc))]
            [ctc-field-names (object-contract-fields ctc)]
            [field-param-projs (map contract-projection (object-contract-field-ctcs ctc))])
        (λ (blame)
           (let* ([meth-projs (map (λ (x) (x blame)) meth-param-projs)]
                  [meths (map (λ (p x) (p x)) meth-projs (object-contract-method-wrappers ctc))]
                  [cls (make-wrapper-class 'wrapper-class meth-names meths ctc-field-names #f)]
                  [field-projs (map (λ (x) (x blame)) field-param-projs)])
             (λ (val)
                
                (unless (object? val)
                  (raise-blame-error blame val "expected an object, got ~e" val))
                
               (for-each (λ (m proj)
                           (let-values ([(method unwrapper) 
                                         (find-method/who 'object-contract val m #:error? #f)])
                             (unless method
                               (raise-blame-error blame val "expected an object with method ~s" m))
                             ;; verify the first-order properties by apply the projection and 
                             ;; throwing the result away. Without this, the contract wrappers
                             ;; just check the first-order properties of the wrappers, which is
                             ;; the wrong thing.
                             (proj method)))
                         meth-names
                         meth-projs)
                
                (let ([fields (field-names val)])
                  (for-each (λ (f)
                               (unless (memq f fields)
                                 (raise-blame-error blame val "expected an object with field ~s" f)))
                            ctc-field-names))
                
                (apply make-object cls val
                       (map (λ (field proj) (proj (get-field/proc field val)))
                            ctc-field-names field-projs)))))))
   #:name
   (λ (ctc) `(object-contract ,@(map (λ (fld ctc) (build-compound-type-name 'field fld ctc))
                                     (object-contract-fields ctc)
                                     (object-contract-field-ctcs ctc))
                              ,@(map (λ (mtd ctc) (build-compound-type-name mtd ctc))
                                     (object-contract-methods ctc)
                                     (object-contract-method-ctcs ctc))))

   #:first-order (λ (ctc) (λ (val) #f))))

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
                                  (list (λ (this . x) (send (wrapper-object-wrapped this) method-id . x)) ...)
                                  '(field-id ...)
                                  (list field-ctc ...))))]))

(define (build-object-contract methods method-ctcs wrappers fields field-ctcs)
  (make-object-contract methods 
                        (map (λ (x) (coerce-contract 'object-contract x)) method-ctcs)
                        wrappers 
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
