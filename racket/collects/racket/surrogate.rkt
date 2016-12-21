#lang racket/base

(require racket/class
         (for-syntax racket/base syntax/parse))

(provide surrogate)

(define-syntax (surrogate stx)

  (define-splicing-syntax-class maybe-surrogate-wrapper
    (pattern (~seq #:use-wrapper-proc)
             #:with wrapper? #t)
    (pattern (~seq)
             #:with wrapper? #f))
  
  (syntax-parse stx
    [(_ msw:maybe-surrogate-wrapper method-spec ...)

     (define use-surrogate-wrapper-proc? (syntax-e #'msw.wrapper?))
     
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
         [(override . whatever)
          (raise-syntax-error
           #f
           "bad override method specification"
           stx
           method-spec)]
         [(augment . whatever)
          (raise-syntax-error
           #f
           "bad augment method specification"
           stx
           method-spec)]
         [(id . whatever)
          (identifier? #'id)
          (raise-syntax-error
           #f
           "bad method specification, expected either override or augment"
           stx
           #'id)]
         [whatever
          (raise-syntax-error
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
  
     (define (maybe-call-surrogate-wrapper-proc id fallback-stx body-stx)
       (if use-surrogate-wrapper-proc?
           #`(surrogate-wrapper-proc
              (λ () #,fallback-stx)
              (let ([id (λ () #,body-stx)]) id))
           body-stx))

     (define (make-lambda-case name super-call)
       (with-syntax ([name name]
                     [super-call super-call])
         (lambda (spec)
           (syntax-case spec ()
             ;; Not a rest arg: normal mode for super-call
             [(id ...) #`[(id ...)
                          (if the-surrogate
                              #,(maybe-call-surrogate-wrapper-proc
                                 #'name
                                 #`(super-call id ...)
                                 #`(send the-surrogate name this super-call id ...))
                              (super-call id ...))]]
             ;; A rest arg: super-class takes args as a list
             [id
              (identifier? (syntax id))
              #`[name 
                 (if the-surrogate
                     #,(maybe-call-surrogate-wrapper-proc
                        #'name
                        #`(super-call name)
                        #`(send the-surrogate name this (lambda args (super-call args)) . name))
                     (super-call name))]]))))

     (with-syntax ([(ids ...) (map (extract-id stx) (syntax->list (syntax (method-spec ...))))]
                   [(overriding-methods ...)
                    (map make-overriding-method 
                         (syntax->list
                          (syntax (method-spec ...))))]
                   [(empty-methods ...)
                    (map make-empty-method
                         (syntax->list
                          (syntax (method-spec ...))))])
       (quasisyntax/loc stx
         (let ([surrogate<%>
                (interface ()
                  on-disable-surrogate
                  on-enable-surrogate
                  ids ...)]
               [host<%> 
                (interface ()
                  set-surrogate
                  get-surrogate
                  #,@(if use-surrogate-wrapper-proc?
                         (list #'set-surrogate-wrapper-proc
                               #'get-surrogate-wrapper-proc)
                         (list))
                  ids ...)])
           (values
            (λ (super%)
              (class* super% (host<%>)
                (define the-surrogate #f)
                #,(if use-surrogate-wrapper-proc?
                      #'(begin
                          (define surrogate-wrapper-proc always-do-the-call-surrogate-wrapper-proc)
                          (define/public-final (set-surrogate-wrapper-proc _p)
                            (check-surrogate-wrapper-proc _p)
                            (set! surrogate-wrapper-proc _p))
                          (define/public-final (get-surrogate-wrapper-proc)
                            surrogate-wrapper-proc))
                      #'(begin))
                (define/public-final (set-surrogate new-surrogate)
                  (do-set-surrogate (λ (s) (set! the-surrogate s))
                                    the-surrogate
                                    this
                                    new-surrogate
                                    #,(if use-surrogate-wrapper-proc?
                                          #'surrogate-wrapper-proc
                                          #'always-do-the-call-surrogate-wrapper-proc)
                                    '(ids ...)))
                (define/public-final (get-surrogate) the-surrogate)
                
                overriding-methods ...
                
                (super-new)))

            host<%>

            #,(syntax/loc stx
                (class* object% (surrogate<%>)
                  (define/public (on-enable-surrogate x) (void))
                  (define/public (on-disable-surrogate x) (void))
                  empty-methods ...
                  (super-new)))
            surrogate<%>))))]))

(define (always-do-the-call-surrogate-wrapper-proc fallback main) (main))

(define (check-surrogate-wrapper-proc p)
  (unless (procedure-arity-includes? p 2)
    (raise-argument-error 'set-surrogate-wrapper-proc
                          "procedure of arity 2"
                          p)))

(define (do-set-surrogate set-the-surrogate
                          the-surrogate
                          this
                          new-surrogate
                          surrogate-wrapper-proc
                          ids)
  (when the-surrogate
    (surrogate-wrapper-proc
     void
     (λ ()
       (send the-surrogate on-disable-surrogate this))))

  ;; error checking
  (when new-surrogate
    (unless (object? new-surrogate)
      (raise-argument-error 'set-surrogate "object?" new-surrogate))
    (let ([methods-to-impl (list* 'on-enable-surrogate 'on-disable-surrogate ids)]
          [i (object-interface new-surrogate)])
      (for ([x (in-list methods-to-impl)])
        (unless (method-in-interface? x i)
          (raise-argument-error 'set-surrogate
                                (format "object with method ~s" x)
                                new-surrogate)))))

  (set-the-surrogate new-surrogate)
  (when new-surrogate
    (surrogate-wrapper-proc
     void
     (λ ()
       (send new-surrogate on-enable-surrogate this)))))
