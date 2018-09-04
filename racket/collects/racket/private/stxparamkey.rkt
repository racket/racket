
(module stxparamkey '#%kernel
  (#%require "small-scheme.rkt" "define.rkt" 
             "stxcase.rkt" "stxloc.rkt" "with-stx.rkt"
             (only '#%unsafe unsafe-root-continuation-prompt-tag)
             (for-template '#%kernel))
  
  ;; Consulted before the expander's table, for use by compile-time
  ;; code wrapped by a run-time-phased `syntax-parameterize`:
  (define (current-parameter-environment)
    ;; Implemented with continuation marks, not parameters, so that the
    ;; "state" is not inherited by new threads
    (continuation-mark-set-first #f current-parameter-environment #hasheq()
                                 (unsafe-root-continuation-prompt-tag)))

  ;; Wrap the value for a syntax parameter in a `parameter-value` struct,
  ;; so that we can distinguish it from rename transformers that arrive
  ;; at the value
  (define-values (struct:parameter-value make-parameter-value parameter-value? parameter-value-ref parameter-value-set!)
    (make-struct-type 'parameter-value #f 1 0 #f null (current-inspector) #f '(0)))
  (define parameter-value-content (make-struct-field-accessor parameter-value-ref 0))

  (define (wrap-parameter-value who/must-be-transformer v)
    (unless (or (not who/must-be-transformer) (rename-transformer? v))
      (raise-argument-error who/must-be-transformer
                            "rename-transformer?"
                            v))
    (make-parameter-value v))

  (define (extend-parameter-environment env binds)
    (with-syntax ([((key sp-id) ...) binds])
      (let loop ([ht (current-parameter-environment)]
                 [keys (syntax->datum #'(key ...))]
                 [ids (syntax->list #'(sp-id ...))])
        (cond
         [(null? keys) ht]
         [else (loop (hash-set ht (car keys) (car ids))
                     (cdr keys)
                     (cdr ids))]))))

  ;; Used to propagate to a submodule, where the parameter
  ;; will get a frash key as the submodule compilation starts
  (define (update-parameter-keys ids binds)
    (let loop ([ids (syntax->list ids)]
               [binds (syntax->list binds)])
      (cond
        [(null? ids) null]
        [else
         (with-syntax ([(key rhs) (car binds)]
                       [new-key (syntax-parameter-key (syntax-local-value (car ids)))])
           (cons #'[new-key rhs]
                 (loop (cdr ids) (cdr binds))))])))
  
  (define (apply-syntax-parameter sp stx)
    (let ([v (syntax-parameter-key-value (syntax-parameter-key sp)
                                         (syntax-parameter-default-id sp))])
      (apply-transformer v stx #'set!)))

  (define-values (struct:syntax-parameter make-syntax-parameter syntax-parameter? syntax-parameter-ref syntax-parameter-set!)
    (make-struct-type 'syntax-parameter #f 2 0 #f (list (cons prop:set!-transformer apply-syntax-parameter)) (current-inspector) 0 '(0 1)))
                   
  (define (syntax-parameter-default-id sp)
    (syntax-parameter-ref sp 0))
  
  (define (syntax-parameter-key sp)
    (syntax-parameter-ref sp 1))

  (define (rename-transformer-parameter-target rtp)
    (define key (syntax-parameter-key rtp))
    (define default-id (syntax-parameter-default-id rtp))
    ;; (syntax-transforming?) is not always true when the
    ;; prop:rename-transformer procedure is evaluated, because it is
    ;; used to test the rename-transformer
    (define lt
      (if (syntax-transforming?)
          (rename-transformer-target (syntax-parameter-key-value key default-id))
          default-id))
    (syntax-property lt 'not-free-identifier=? #t))
  
  (define-values (struct:rename-transformer-parameter make-rename-transformer-parameter rename-transformer-parameter? rename-transformer-parameter-ref rename-transformer-parameter-set!)
    (make-struct-type 'rename-transformer-parameter struct:syntax-parameter 0 0 #f (list (cons prop:rename-transformer rename-transformer-parameter-target)) (current-inspector) #f))

  (define (syntax-parameter-key-value key default-id)
    (define id (hash-ref
                (current-parameter-environment)
                key
                (lambda () #f)))
    (let loop ([id (or id default-id)])
      (define-values (val next-id) (syntax-local-value/immediate id (lambda () (values #f #f))))
      (cond
       [(parameter-value? val) (parameter-value-content val)]
       [next-id
        ;; Some part of expansion introduced a rename transformer
        ;; between our identifier and its binding
        (loop next-id)]
       [else val])))
  
  (define (syntax-parameter-local-value id)
    (let loop ([id id])
      (define-values (sp next-id) (syntax-local-value/immediate id (lambda () (values #f #f))))
      (cond
       [(syntax-parameter? sp) sp]
       [next-id
        ;; Might be a rename of a syntax-parameter binding
        (loop next-id)]
       [else #f])))

  (define (apply-transformer v stx set!-stx)
    (cond
     [(rename-transformer? v) 
      (with-syntax ([target  (rename-transformer-target v)])
	(syntax-case stx ()
	  [(set! id _expr) 
	   (free-identifier=? #'set! set!-stx)
	   (syntax/loc stx (set! target _expr))]
	  [(id . rest)
	   (let ([v (syntax (target . rest))])
	     (datum->syntax
	      stx
	      (syntax-e v)
	      stx))]
	  [_
	   #'target]))]
     [(set!-transformer? v) ((set!-transformer-procedure v) stx)]
     [(and (procedure? v)
	   (procedure-arity-includes? v 1))
      (syntax-case stx ()
	[(set! id _) 
	 (free-identifier=? #'set! set!-stx)
	 (raise-syntax-error
	  #f
	  "cannot mutate syntax identifier"
	  stx
	  #'id)]
	[else (v stx)])]
     [else
      (raise-syntax-error
       #f
       "bad syntax"
       stx
       #f)]))

  (#%provide wrap-parameter-value
             current-parameter-environment
             extend-parameter-environment
             update-parameter-keys
             apply-transformer
             syntax-parameter?
             make-syntax-parameter
             rename-transformer-parameter?
             make-rename-transformer-parameter
             syntax-parameter-local-value
             syntax-parameter-key
             syntax-parameter-default-id
             syntax-parameter-key-value))
