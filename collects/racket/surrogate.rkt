#lang racket/base

(require racket/class
         (for-syntax racket/base))

(provide surrogate)

(define-syntax (surrogate stx)
  
  (define (make-empty-method method-spec)
    (syntax-case method-spec (override augment)
      [(override name argspec ...)
       (identifier? (syntax name))
       (make-empty-method-from-argspec #'name (syntax (argspec ...)))]
      [(augment def-expr name argspec ...)
       (identifier? (syntax name))
       (make-empty-method-from-argspec #'name (syntax (argspec ...)))]))
  
  (define (make-empty-method-from-argspec name argspecs)
    (with-syntax ([(cases ...) (map make-empty-lambda-case
                                    (syntax->list argspecs))]
                  [name name])
      (syntax
       (begin
         (define/public name
           (case-lambda cases ...))))))
  
  (define (make-empty-lambda-case spec)
    (syntax-case spec ()
      [(id ...) (syntax [(ths super-call id ...) (super-call id ...)])]
      [id
       (identifier? (syntax id))
       (syntax [(ths super-call . name) (apply super-call name)])]))
  
  (define (make-overriding-method method-spec)
    (syntax-case method-spec (override augment)
      [(override name argspec ...)
       (identifier? (syntax name))
       (make-overriding-method-with-inner-default 
        #'name #f #'(argspec ...))]
      [(augment def-expr name argspec ...)
       (identifier? (syntax name))
       (make-overriding-method-with-inner-default 
        #'name #'def-expr #'(argspec ...))]))
  
  (define (make-overriding-method-with-inner-default name def-expr argspecs)
    ;; (not def-expr) => normal override
    ;; def-expr => beta override
    (let ([super-call-name
           (datum->syntax 
            name
            (string->symbol
             (string-append
              (if def-expr
                  "inner-proc-"
                  "super-proc-")
              (symbol->string
               (syntax->datum
                name)))))])
      (with-syntax ([(cases ...) 
                     (map (make-lambda-case name
                                            super-call-name)
                          (syntax->list argspecs))]
                    [(super-proc-cases ...)
                     (map (make-super-proc-case name def-expr)
                          (syntax->list argspecs))]
                    [super-call-name super-call-name]
                    [name name]
                    [ren/inn (if def-expr
                                 #'inner
                                 #'rename)]
                    [define/override/fnl (if def-expr
                                             #'define/augment
                                             #'define/override)])
        (syntax
         (begin
           (field [super-call-name
                   (case-lambda super-proc-cases ...)])
           (define/override/fnl name
             (case-lambda cases ...)))))))
  
  (define ((extract-id stx) method-spec)
    (syntax-case method-spec (override augment)
      [(override name argspec ...)
       (identifier? #'name)
       (syntax name)]
      [(augment result-expr name argspec ...)
       (identifier? #'name)
       (syntax name)]
      [else (raise-syntax-error 
             #f
             "bad method specification"
             stx
             method-spec)]))
  
  (define (make-super-proc-case name def-expr)
    (lambda (spec)
      (with-syntax ([name name])
        (syntax-case spec ()
          ;; Not a rest arg: normal mode
          [(id ...) (quasisyntax [(id ...)
                                  (#,@(if def-expr 
                                          (list #'inner def-expr)
                                          (list #'super))
                                   name
                                   id ...)])]
          ;; A rest arg: take args as list
          [id 
           (identifier? (syntax id))
           (quasisyntax [(id) (#,@(if def-expr 
                                      (list #'inner def-expr)
                                      (list #'super))
                               name
                               . id)])]))))
  
  (define (make-lambda-case name super-call)
    (with-syntax ([name name]
                  [super-call super-call])
      (lambda (spec)
        (syntax-case spec ()
          ;; Not a rest arg: normal mode for super-call
          [(id ...) (syntax [(id ...)
                             (if surrogate
                                 (send surrogate name this super-call id ...)
                                 (super-call id ...))])]
          ;; A rest arg: super-class takes args as a list
          [id
           (identifier? (syntax id))
           (syntax [name 
                    (if surrogate
                        (send surrogate name this (lambda args (super-call args)) . id)
                        (super-call id))])]))))
  
  (syntax-case stx ()
    [(_ method-spec ...)
     (with-syntax ([(ids ...) (map (extract-id stx) (syntax->list (syntax (method-spec ...))))]
                   [(overriding-methods ...)
                    (map make-overriding-method 
                         (syntax->list
                          (syntax (method-spec ...))))]
                   [(empty-methods ...)
                    (map make-empty-method
                         (syntax->list
                          (syntax (method-spec ...))))])
       (syntax/loc stx
         (let ([surrogate<%>
                (interface ()
                  on-disable-surrogate
                  on-enable-surrogate
                  ids ...)]
               [host<%> 
                (interface ()
                  set-surrogate
                  get-surrogate
                  ids ...)])
           (values
            (lambda (super%)
              (class* super% (host<%>)
                (field [surrogate #f])
                (define/public (set-surrogate d)
                  (when surrogate
                    (send surrogate on-disable-surrogate this))
                  
                  ;; error checking
                  (when d
                    (unless (object? d)
                      (error 'set-surrogate "expected an object, got: ~e" d))
                    (let ([methods-to-impl '(on-enable-surrogate on-disable-surrogate ids ...)]
                          [i (object-interface d)])
                      (for-each (lambda (x) 
                                  (unless (method-in-interface? x i)
                                    (error 'set-surrogate "expected object to implement an ~s method" x)))
                                methods-to-impl)))
                  
                  (set! surrogate d)
                  (when surrogate
                    (send surrogate on-enable-surrogate this)))
                (define/public (get-surrogate) surrogate)
                
                overriding-methods ...
                
                (super-new)))
            host<%>
            
            (class* object% (surrogate<%>)
              (define/public (on-enable-surrogate x) (void))
              (define/public (on-disable-surrogate x) (void))
              empty-methods ...
              (super-new))
            surrogate<%>))))]))
