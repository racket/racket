#lang racket/base
(require (for-syntax racket/base)
         "arrow-common.rkt"
         "arrow-higher-order.rkt"
         "blame.rkt"
         "guts.rkt"
         "../../private/class-struct.rkt")
(provide (for-syntax build-object/c-wrapper/real)
         wrong-number-of-results-blame)

(define object-c-wrapper-unspecified-dom (gensym 'object-c-wrapper-unspecified-dom))

(define-for-syntax (build-object/c-wrapper/real
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds ;; must be sorted by keyword<?
                    pre pre/desc
                    rest
                    rngs
                    post post/desc
                    method?)
  (cond
    [(null? regular-args)
     ;; wrappers like this can never be called, as methods
     ;; always require at least one argument
     #'#f]
    [else
     ;; method-name : symbol, method name
     ;; blame : a blame specialized to the method
     ;; c : base->? contract, which has all of the subcontracts
     (unless (equal? (sort optional-kwds keyword<? #:key (λ (x) (if (syntax? x) (syntax-e x) x))) optional-kwds)
       (error 'object-c-wrapper.rkt "expected optional-keywords to already be sorted!\n  keywords: ~s"
              optional-kwds))
     (with-syntax ([(this-x arg-x ...) (generate-temporaries regular-args)]
                   [(this-ctc-x dom-ctc-x ...) (generate-temporaries regular-args)]
                   [(kwd-arg-x ...) (generate-temporaries mandatory-kwds)]
                   [(kwd-ctc-x ...) (generate-temporaries mandatory-kwds)]
                   [(kwd ...) mandatory-kwds]
                   [(opt-kwd-arg-x ...) (generate-temporaries optional-kwds)]
                   [(opt-kwd-ctc-x ...) (generate-temporaries optional-kwds)]
                   [(opt-kwd ...) optional-kwds]
                   [(opt-arg-x ...) (generate-temporaries optional-args)]
                   [(opt-ctc-x ...) (generate-temporaries optional-args)]
                   [(rng-x ...) (if rngs (generate-temporaries rngs) '())]
                   [(rng-ctc-x ...) (if rngs (generate-temporaries rngs) '())])
       (with-syntax ([(pre-check ...)
                     (cond
                       [pre 
                        (list #`(check-pre-cond #,pre blame+neg-party this-x))]
                       [pre/desc
                        (list #`(check-pre-cond/desc #,pre/desc blame+neg-party this-x))]
                       [else (list)])]
                    [(post-check ...)
                     (cond
                       [post
                        (list #`(check-post-cond #,post blame+neg-party this-x))]
                       [post/desc
                        (list #`(check-post-cond/desc #,post/desc blame+neg-party this-x))]
                       [else (list)])]
                    ;; this is just to allow the double ellipsis append used below
                    [((kwd+kwd-arg-x ...) ...) #`((kwd kwd-arg-x) ...)]
                    [((kwd+ctc-arg ...) ...)
                     #`((kwd
                         (with-contract-continuation-mark
                             blame+neg-party
                           (kwd-ctc-x kwd-arg-x neg-party)))
                        ...)]
                    [((opt-kwd+opt-kwd-arg-x ...) ...) #`((opt-kwd [opt-kwd-arg-x object-c-wrapper-unspecified-dom]) ...)])
       (define prefix
         (list
          #'(define wrapped-object-info (object-ref this-x))
          #'(define blame+neg-party (object/c-wrapper-info-blame+neg-party wrapped-object-info))
          #'(define unwrapped (object/c-wrapper-info-val wrapped-object-info))
          #'(define neg-party (cdr blame+neg-party))
          #'(define meth (find-method/who 'method-lookup-in-object/c-should-never-fail unwrapped method-name))
          ))
         (define no-rest-args-call
           #`(meth #,(if method?
                         #'unwrapped
                         #'(with-contract-continuation-mark
                               blame+neg-party
                             (this-ctc-x unwrapped neg-party)))
                   (with-contract-continuation-mark
                       blame+neg-party
                     (dom-ctc-x arg-x neg-party))
                   ...
                   kwd+ctc-arg ... ...))
         (define call
           (cond
             [(or rest (pair? optional-args) (pair? optional-kwds))
              (define last-arg-to-apply
                (for/fold ([i (if rest
                                  #'(with-contract-continuation-mark
                                        blame+neg-party
                                      (rest-ctc-x rest-arg-id neg-party))
                                  #''())])
                          ([o (in-list (reverse
                                        (syntax->list
                                         #'((opt-ctc-x opt-arg-x neg-party) ...))))]
                           [opt-arg-x (in-list (reverse (syntax->list #'(opt-arg-x ...))))])
                  #`(let ([r #,i])
                      (if (eq? object-c-wrapper-unspecified-dom #,opt-arg-x)
                          r
                          (cons #,o r)))))
              (cond
                [(pair? optional-kwds)
                 (with-syntax ([(meth args ...) no-rest-args-call])
                   #`(let-values ([(kwds kwd-vals)
                                   (filter-down-to-actually-supplied-keywords
                                    '(opt-kwd ...)
                                    (list (if (eq? object-c-wrapper-unspecified-dom opt-kwd-arg-x)
                                              opt-kwd-arg-x
                                              (with-contract-continuation-mark
                                                  blame+neg-party
                                                (opt-kwd-ctc-x opt-kwd-arg-x neg-party)))
                                          ...))])
                       (keyword-apply meth
                                      kwds
                                      kwd-vals
                                      args ...
                                      #,last-arg-to-apply)))]
                [else
                 #`(apply #,@no-rest-args-call
                           #,last-arg-to-apply)])]
             [else no-rest-args-call]))
         #`(λ (method-name blame
                           this-ctc-x ;; even if it is an ->m contract, we get an argument here; it is always any/c so we ignore it
                           dom-ctc-x ...
                           opt-ctc-x ...
                           kwd-ctc-x ...
                           opt-kwd-ctc-x ...
                           #,@(if rest
                                  (list #'rest-ctc-x)
                                  (list))
                           rng-ctc-x ...)
             (procedure-rename
              ;; the replacement method
              #,(syntax-property
                 #`(λ (this-x arg-x ...
                              kwd+kwd-arg-x ... ...
                              [opt-arg-x object-c-wrapper-unspecified-dom] ...
                              opt-kwd+opt-kwd-arg-x ... ...
                              #,@(if rest #'rest-arg-id (list)))
                     #,@prefix
                     pre-check ...
                     #,(if rngs
                           #`(call-with-values
                              (λ () #,call)
                              (case-lambda
                                [(rng-x ...)
                                 (with-contract-continuation-mark
                                     blame+neg-party
                                   post-check ...
                                   (values (rng-ctc-x rng-x neg-party) ...))]
                                [args
                                 (with-contract-continuation-mark
                                     blame+neg-party
                                   (wrong-number-of-results-blame
                                    blame neg-party meth
                                    args
                                    #,(length (syntax->list #'(rng-x ...)))))]))
                           call))
                 'method-arity-error #t)
              (add-method-suffix method-name)))))]))

(define (filter-down-to-actually-supplied-keywords kwds args)
  (let loop ([kwds kwds]
             [args args])
    (cond
      [(null? kwds)
       (values '() '())]
      [else
       (define arg (car args))
       (define kwd (car kwds))
       (cond
         [(eq? object-c-wrapper-unspecified-dom arg)
          (loop (cdr kwds) (cdr args))]
         [else
          (define-values (kwds-ans args-ans) (loop (cdr kwds) (cdr args)))
          (values (cons kwd kwds-ans) (cons arg args-ans))])])))

(define (add-method-suffix method-name)
  (string->symbol (string-append (symbol->string method-name) " method")))

(define (wrong-number-of-results-blame blame neg-party val reses expected-values)
  (define length-reses (length reses))
  (raise-blame-error 
   blame #:missing-party neg-party val
   '("received ~a value~a" expected: "~a value~a")
   length-reses
   (if (= 1 length-reses) "" "s")
   expected-values
   (if (= 1 expected-values) "" "s")))
