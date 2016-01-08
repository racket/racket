
(module stxparamkey '#%kernel
  (#%require "small-scheme.rkt" "define.rkt" 
             "stxcase.rkt" "stxloc.rkt" "with-stx.rkt")

  (-define-struct wrapped-renamer (renamer))

  (define-values (struct:parameter-binding make-parameter-binding parameter-binding? parameter-binding-ref parameter-binding-set!)
    (make-struct-type 'parameter-binding #f 2 0 #f null (current-inspector) #f '(0 1)))
  (define parameter-binding-val (make-struct-field-accessor parameter-binding-ref 0))
  (define parameter-binding-param (make-struct-field-accessor parameter-binding-ref 1))

  (define (parameter-binding-rt-target pbr)
    (rename-transformer-target (wrapped-renamer-renamer (parameter-binding-val pbr))))
  
  (define-values (struct:parameter-binding-rt make-parameter-binding-rt parameter-binding-rt? parameter-binding-rt-ref parameter-binding-rt-set!)
    (make-struct-type 'parameter-binding-rt struct:parameter-binding 0 0 #f (list (cons prop:rename-transformer parameter-binding-rt-target)) (current-inspector) #f))
    
  (define-values (struct:syntax-parameter make-syntax-parameter syntax-parameter? syntax-parameter-ref syntax-parameter-set!)
    (make-struct-type 'syntax-parameter #f 2 0 #f null (current-inspector) 0 '(0 1)))

  (define (rename-transformer-parameter-target rtp)
    (define t (syntax-parameter-target rtp))
    ;; XXX (syntax-transforming?) is not always true when the
    ;; prop:rename-transformer procedure is evaluated. I think this is
    ;; because it used to test rename-transformer?
    (define lt
      (if (syntax-transforming?)
          (syntax-local-get-shadower t #t)
          t))
    (syntax-property lt 'not-free-identifier=? #t))
  
  (define-values (struct:rename-transformer-parameter make-rename-transformer-parameter rename-transformer-parameter? rename-transformer-parameter-ref rename-transformer-parameter-set!)
    (make-struct-type 'rename-transformer-parameter struct:syntax-parameter 0 0 #f (list (cons prop:rename-transformer rename-transformer-parameter-target)) (current-inspector) #f))

  (define (syntax-parameter-target sp)
    (syntax-parameter-ref sp 1))

  (define (target-value target)
    (syntax-local-value (syntax-local-get-shadower target #t)
                        (lambda ()
                          (syntax-local-value 
                           target
                           (lambda () #f)))))

  (define (syntax-parameter-target-value target)
    (let* ([v (target-value target)]
           [v (if (parameter-binding? v)
                  (or (let ([id ((parameter-binding-param v))])
                        (and id
                             (let ([v (syntax-local-value id)])
                               (parameter-binding-val v))))
                      (parameter-binding-val v))
                  v)])
      (if (wrapped-renamer? v)
	  (wrapped-renamer-renamer v)
          v)))

  (define (syntax-parameter-target-parameter target)
    (let ([v (target-value target)])
      (parameter-binding-param v)))

  (define (convert-renamer must-be-renamer?-stx v)
    (when must-be-renamer?-stx
      (unless (rename-transformer? v)
        (raise-syntax-error #f "rename-transformer-parameter must be bound to rename-transformer" must-be-renamer?-stx)))
    ((if must-be-renamer?-stx
         make-parameter-binding-rt
         make-parameter-binding)
     (if (rename-transformer? v)
         (make-wrapped-renamer v)
         v)
     ;; compile-time parameter needed for `splicing-syntax-parameterize':
     (make-parameter #f)))

  (define (apply-transformer v stx set!-stx)
    (cond
     [(rename-transformer? v) 
      (with-syntax ([target (rename-transformer-target v)])
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


  (#%provide convert-renamer
             apply-transformer
             syntax-parameter?
             make-syntax-parameter
             rename-transformer-parameter?
             make-rename-transformer-parameter
             syntax-parameter-target
             syntax-parameter-target-value
             syntax-parameter-target-parameter))
