#lang racket/base

(require syntax/stx
         (for-syntax racket/base)
         (for-template racket/base
                       racket/unsafe/undefined
                       "class-wrapped.rkt"
                       "class-undef.rkt"))

(define insp (variable-reference->module-declaration-inspector
              (#%variable-reference)))
(define (class-syntax-protect stx)
  (syntax-arm stx insp #t))

(define-values (struct:s!t make-s!t s!t? s!t-ref s!t-set!)
  (make-struct-type 'set!-transformer #f 2 0 #f null (current-inspector) 0))

(define (mk-set!-trans old-id proc)
  (make-set!-transformer (make-s!t proc old-id)))

(define (make-method-apply id this orig-args)
  (let loop ([args orig-args][accum null])
    (cond
      [(stx-null? args)
       (list* id this orig-args)]
      [(stx-pair? args)
       (loop (stx-cdr args) (cons (stx-car args) accum))]
      [else
       (list* 'apply id this (reverse (cons args accum)))])))

(define (find the-finder name src)
  (let ([this-id (syntax-local-value (syntax-local-get-shadower the-finder))])
    (datum->syntax this-id name src)))

;; Check Syntax binding info:
(define (binding from to stx)
  stx)

;; Declarations used to determine whether a chaperone is
;; needed to protect against unsafe-undefined access
(define (add-declare-this-escapes src-stx stx)
  (quasisyntax/loc src-stx (begin '(declare-this-escapes) #,stx)))
(define (add-declare-field-use id inherited? src-stx stx)
  (if inherited?
      (quasisyntax/loc src-stx (begin '(declare-inherit-use #,id) #,stx))
      (quasisyntax/loc src-stx (begin '(declare-field-use #,id) #,stx))))
(define (add-declare-field-assignment id inherited? src-stx stx)
  (if inherited?
      stx
      (quasisyntax/loc src-stx (begin '(declare-field-assignment #,id) #,stx))))
(define (add-declare-field-initialization id src-stx stx)
  (quasisyntax/loc src-stx (begin '(declare-field-initialization #,id) #,stx)))

(define (make-this-map orig-id the-finder the-obj)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (mk-set!-trans
     orig-id
     (lambda (stx)
       (syntax-case stx ()
         [(set! id expr)
          (free-identifier=? (syntax set!) set!-stx)
          (raise-syntax-error 'class "cannot mutate object identifier" stx)]
         [(id . args)
          (add-declare-this-escapes
           stx
           (datum->syntax
            stx
            (cons (find the-finder the-obj stx) (syntax args))
            stx))]
         [id (add-declare-this-escapes stx (find the-finder the-obj stx))])))))

(define (make-this%-map replace-stx the-finder)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (make-set!-transformer
     (λ (stx)
       (syntax-case stx ()
         [(set! id expr)
          (free-identifier=? #'set! set!-stx)
          (raise-syntax-error 'class "cannot mutate this% identifier" stx)]
         [id
          (identifier? #'id)
          (quasisyntax/loc stx #,replace-stx)]
         [(f . args)
          (quasisyntax/loc stx (#,replace-stx . args))])))))

(define (make-field-map inherited? the-finder the-obj the-binder the-binder-localized
                        field-accessor field-mutator)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (define (choose-src a b) (if (syntax-source a) a b))
    (mk-set!-trans
     the-binder-localized
     (lambda (stx)
       (class-syntax-protect
        (with-syntax ([obj-expr (find the-finder the-obj stx)])
          (syntax-case stx (field-initialization-value)
            [(set! id (field-initialization-value expr))
             (free-identifier=? (syntax set!) set!-stx)
             (add-declare-field-initialization
              #'id
              #'id
              (with-syntax ([bindings (syntax/loc stx ([obj obj-expr] [id expr]))]
                            [set (quasisyntax/loc stx
                                   ;; This continuation mark disables the chaperone on field assignement
                                   ;; (if any) installed via `prop:chaperone-unsafe-undefined`:
                                   (with-continuation-mark prop:chaperone-unsafe-undefined unsafe-undefined
                                     #,(quasisyntax/loc stx
                                         ((unsyntax field-mutator) obj id))))])
                (syntax/loc (choose-src stx #'id) (let* bindings set))))]
            [(set! id expr)
             (free-identifier=? (syntax set!) set!-stx)
             (add-declare-field-assignment
              #'id
              inherited?
              #'id
              (with-syntax ([bindings (syntax/loc stx ([obj obj-expr] [id expr]))]
                            [set (quasisyntax/loc stx
                                   ((unsyntax field-mutator) obj id))])
                (syntax/loc (choose-src stx #'id) (let* bindings set))))]
            [(id . args)
             (add-declare-field-use
              #'id
              inherited?
              #'id
              (with-syntax ([bindings (syntax/loc stx ([obj obj-expr]))]
                            [call (quasisyntax/loc stx
                                    (((unsyntax field-accessor) obj) . args))])
                (syntax/loc (choose-src stx #'id) (let* bindings call))))]
            [id
             (add-declare-field-use
              #'id
              inherited?
              stx
              (with-syntax ([bindings (syntax/loc stx ([obj obj-expr]))]
                            [get (quasisyntax/loc stx
                                   ((unsyntax field-accessor) obj))])
                (syntax/loc (choose-src stx #'id) (let* bindings get))))])))))))

(define (make-method-map the-finder the-obj the-binder the-binder-localized method-accessor)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (mk-set!-trans
     the-binder-localized
     (lambda (stx)
       (class-syntax-protect
        (syntax-case stx ()
          [(set! id expr)
           (free-identifier=? (syntax set!) set!-stx)
           (raise-syntax-error 'class "cannot mutate method" stx)]
          [(id . args)
           (add-declare-this-escapes
            stx
            (binding
             the-binder (syntax id)
             (datum->syntax 
              the-finder
              (make-method-apply
               (list method-accessor (find the-finder the-obj stx))
               (find the-finder the-obj stx)
               (syntax args))
              stx)))]
          [_else
           (raise-syntax-error 
            'class 
            "misuse of method (not in application)" 
            stx)]))))))

;; For methods that are dirrectly available via their names
;;  (e.g., private methods)
(define (make-direct-method-map the-finder the-obj the-binder the-binder-localized new-name)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (mk-set!-trans
     the-binder-localized
     (lambda (stx)
       (class-syntax-protect
        (syntax-case stx ()
          [(set! id expr)
           (free-identifier=? (syntax set!) set!-stx)
           (raise-syntax-error 'class "cannot mutate method" stx)]
          [(id . args)
           (add-declare-this-escapes
            stx
            (binding
             the-binder (syntax id)
             (datum->syntax 
              the-finder
              (make-method-apply (find the-finder new-name stx) (find the-finder the-obj stx) (syntax args))
              stx)))]
          [_else
           (raise-syntax-error 
            'class 
            "misuse of method (not in application)" 
            stx)]))))))

(define (make-rename-super-map the-finder the-obj the-binder the-binder-localized rename-temp)
  (let ([set!-stx (datum->syntax the-finder 'set!)])
    (mk-set!-trans
     the-binder-localized
     (lambda (stx)
       (class-syntax-protect
        (syntax-case stx ()
          [(set! id expr)
           (free-identifier=? (syntax set!) set!-stx)
           (raise-syntax-error 'class "cannot mutate super method" stx)]
          [(id . args)
           (add-declare-this-escapes
            stx
            (binding
             the-binder (syntax id)
             (datum->syntax 
              the-finder
              (make-method-apply (find the-finder rename-temp stx) (find the-finder the-obj stx) (syntax args))
              stx)))]
          [_else
           (raise-syntax-error 
            'class 
            "misuse of super method (not in application)" 
            stx)]))))))

(define (make-rename-inner-map the-finder the-obj the-binder the-binder-localized rename-temp)
  (let ([set!-stx (datum->syntax the-finder 'set!)]
        [lambda-stx (datum->syntax the-finder 'lambda)])
    (mk-set!-trans
     the-binder-localized
     (lambda (stx)
       (class-syntax-protect
        (syntax-case stx ()
          [(set! id expr)
           (free-identifier=? (syntax set!) set!-stx)
           (raise-syntax-error 'class "cannot mutate inner method" stx)]
          [(id (lambda () default) . args)
           (free-identifier=? (syntax lambda) lambda-stx)
           (let ([target (find the-finder the-obj stx)])
             (add-declare-this-escapes
              stx
              (binding
               the-binder (syntax id)
               (datum->syntax 
                the-finder
                (make-method-apply (list (find the-finder rename-temp stx) target #'default)
                                   target (syntax args))
                stx))))]
          [(id (lambda largs default) . args)
           (free-identifier=? (syntax lambda) lambda-stx)
           (raise-syntax-error 
            'class 
            "misuse of inner method (lambda for default does not take zero arguments)" 
            stx)]
          [(id (lambda . rest) . args)
           (free-identifier=? (syntax lambda) lambda-stx)
           (raise-syntax-error 
            'class 
            "misuse of inner method (ill-formed lambda for default)" 
            stx)]
          [(id . args)
           (raise-syntax-error 
            'class 
            "misuse of inner method (no lambda-wrapped default after name)" 
            stx)]
          [_else
           (raise-syntax-error 
            'class 
            "misuse of inner method (not in application)" 
            stx)]))))))

(define (generate-super-call stx the-finder the-obj rename-temp args)
  (add-declare-this-escapes
   stx
   (class-syntax-protect
    (datum->syntax 
     the-finder
     (make-method-apply (find the-finder rename-temp stx) 
                        (find the-finder the-obj stx) 
                        args)
     stx))))

(define (generate-inner-call stx the-finder the-obj default-expr rename-temp args)
  (add-declare-this-escapes
   stx
   (class-syntax-protect
    (datum->syntax 
     the-finder
     (let ([target (find the-finder the-obj stx)])
       (datum->syntax 
        the-finder
        `(let ([i (,(find the-finder rename-temp stx) ,target)])
           (if i
               ,(make-method-apply 'i target args)
               ,default-expr))
        stx))
     stx))))

(define (make-init-error-map localized-id)
  (mk-set!-trans
   localized-id
   (lambda (stx)
     (raise-syntax-error 
      'class
      "cannot use non-field init variable in a method"
      stx))))

(define (make-init-redirect set!-stx #%app-stx local-id localized-id)
  (mk-set!-trans
   localized-id
   (lambda (stx)
     (class-syntax-protect
      (syntax-case stx ()
        [(set! id expr)
         (free-identifier=? (syntax set!) set!-stx)
         (with-syntax ([local-id local-id])
           (syntax/loc stx (set! local-id expr)))]
        [(id . args)
         (with-syntax ([local-id (datum->syntax
                                  local-id
                                  (syntax-e local-id)
                                  #'id
                                  #'id)]
                       [#%app #%app-stx])
           (syntax/loc stx (#%app (#%app check-not-unsafe-undefined local-id 'id) . args)))]
        [id (quasisyntax/loc stx
              (#,#%app-stx
               check-not-unsafe-undefined
               #,(datum->syntax
                  local-id
                  (syntax-e local-id)
                  stx
                  stx)
               'id))])))))

(define super-error-map
  (lambda (stx)
    (raise-syntax-error 
     'class
     "cannot use superclass initialization form in a method"
     stx)))

(define (make-with-method-map set!-stx id-stx
                              method-stx method-obj-stx)
  (make-set!-transformer
   (lambda (stx)
     (class-syntax-protect
      (syntax-case stx ()
        [(set! id expr)
         (and (identifier? (syntax id))
              (free-identifier=? (syntax set!) set!-stx))
         (raise-syntax-error 'with-method "cannot mutate method" stx)]
        [(id . args)
         (identifier? (syntax id))
         (let* ([args-stx (syntax args)]
                [proper? (stx-list? args-stx)]
                [flat-args-stx (if proper? args-stx (flatten-args args-stx))])
           (make-method-call-to-possibly-wrapped-object
            stx #f flat-args-stx (not proper?)
            #''id method-stx method-obj-stx method-obj-stx))]
        [id
         (identifier? (syntax id))
         (raise-syntax-error 
          'with-method 
          "misuse of method (not in application)" 
          stx)])))))

(define (flatten-args orig-args)
  (let loop ([args orig-args][accum null])
    (cond
      [(stx-null? args) orig-args]
      [(stx-pair? args)
       (loop (stx-cdr args) (cons (stx-car args) accum))]
      [else
       (reverse (cons args accum))])))

(define-struct private-name (orig-id gen-id)
  #:property prop:procedure (lambda (self stx)
                              (if (not (eq? (syntax-local-context) 'expression))
                                  #`(#%expression #,stx)
                                  (raise-syntax-error
                                   #f
                                   "unbound local member name"
                                   stx))))

(define (do-localize orig-id validate-local-member-stx)
  (let loop ([id orig-id])
    (let ([v (syntax-local-value id (lambda () #f))])
      (cond
        [(and v (private-name? v))
         (list 'unquote 
               (list validate-local-member-stx
                     (list 'quote orig-id)
                     (binding (private-name-orig-id v)
                              id
                              (private-name-gen-id v))))]
        [(and (set!-transformer? v)
              (s!t? (set!-transformer-procedure v)))
         (s!t-ref (set!-transformer-procedure v) 1)]
        [else orig-id]))))

(define-struct class-context ())

(define (generate-class-expand-context)
  (let ([c (syntax-local-context)]
        [v (make-class-context)])
    (if (pair? c)
        (cons v c)
        (list v))))

(define (class-top-level-context? ctx)
  (and (pair? ctx)
       (class-context? (car ctx))))

(define (make-method-call source-stx object-stx
                          method-proc-stx method-name-stx args-stx 
                          rest-arg? kw-args)
  
  (define-syntax (qstx stx)
    (syntax-case stx ()
      [(form body) (syntax/loc stx (quasisyntax/loc source-stx body))]))
  
  (class-syntax-protect
   (with-syntax ([object object-stx]
                 [method method-proc-stx]
                 [app (if rest-arg? 
                          (if kw-args
                              (qstx keyword-apply)
                              (qstx apply))
                          (qstx #%app))]
                 [(kw-arg ...) (or kw-args'())]
                 [args args-stx])
     (qstx (app method kw-arg ... object . args)))))

(define (make-method-call-to-possibly-wrapped-object
         stx kw-args/var arg-list rest-arg?
         sym method receiver method-in-wrapper-fallback-case)
  (with-syntax ([sym sym]
                [method method]
                [receiver receiver]
                [method-in-wrapper-fallback-case method-in-wrapper-fallback-case])
    (quasisyntax/loc stx
      (if (wrapped-object? receiver)
          (if method
              ;; this is a hack: passing the neg party in
              ;; as the object to 'make-method-call' so that the
              ;; arguments end up in the right order.
              (unsyntax
               (syntax-property
                (make-method-call
                 stx
                 #`(wrapped-object-neg-party receiver)
                 (syntax/loc stx method)
                 (syntax/loc stx sym)
                 #`((wrapped-object-object #,(syntax/loc stx receiver)) #,@arg-list)
                 rest-arg?
                 kw-args/var)
                'feature-profile:send-dispatch 'antimark))
              (let ([receiver (wrapped-object-object receiver)])
                (unsyntax
                 (syntax-property
                  (make-method-call
                   stx
                   (syntax/loc stx receiver)
                   (syntax/loc stx method-in-wrapper-fallback-case)
                   (syntax/loc stx sym)
                   arg-list
                   rest-arg?
                   kw-args/var)
                  'feature-profile:send-dispatch 'antimark))))
          (unsyntax
           (make-method-call
            stx
            (syntax/loc stx receiver)
            (syntax/loc stx method)
            (syntax/loc stx sym)
            arg-list
            rest-arg?
            kw-args/var))))))

(provide (protect-out make-this-map make-this%-map make-field-map make-method-map 
                      make-direct-method-map 
                      make-rename-super-map make-rename-inner-map
                      make-init-error-map make-init-redirect super-error-map 
                      make-with-method-map
                      flatten-args make-method-call 
                      make-method-call-to-possibly-wrapped-object
                      do-localize make-private-name
                      generate-super-call generate-inner-call
                      generate-class-expand-context class-top-level-context?))
