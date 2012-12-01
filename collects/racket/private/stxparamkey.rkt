
(module stxparamkey '#%kernel
  (#%require "small-scheme.rkt" "define.rkt" 
             "stxcase.rkt" "stxloc.rkt" "with-stx.rkt")

  (-define-struct wrapped-renamer (renamer))
  (-define-struct parameter-binding (val param))
  
  (define-values (struct:syntax-parameter make-syntax-parameter syntax-parameter? syntax-parameter-ref syntax-parameter-set!)
    (make-struct-type 'syntax-parameter #f 2 0 #f null (current-inspector) 0))

  (define (syntax-parameter-target sp)
    (syntax-parameter-ref sp 1))

  (define (target-value target)
    (syntax-local-value (syntax-local-get-shadower target)
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

  (define (convert-renamer v)
    (make-parameter-binding
     (if (rename-transformer? v)
         (make-wrapped-renamer v)
         v)
     ;; comile-time parameter needed for `splicing-syntax-parameterize':
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
             syntax-parameter-target
             syntax-parameter-target-value
             syntax-parameter-target-parameter))
