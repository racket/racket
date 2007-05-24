(module closure mzscheme
  (require-for-template mzscheme
                        (lib "serialize.ss")
                        (lib "etc.ss"))
  (provide make-closure-definition-syntax) 
  
  (define myprint printf)
   
  ;; borrowed this from Matthew's code
  ;; creates the deserialize-info identifier
  (define (make-deserialize-name id)
	(datum->syntax-object
	 id
	 (string->symbol
	  (format "web-deserialize-info:~a" (syntax-e id)))
	 id))
  
  (define (make-closure-definition-syntax tag formals fvars proc-body)
    (let ([make-id (lambda (str)
                       (datum->syntax-object
                        tag (string->symbol (format str (syntax-object->datum tag)))))])
      (let ([deserialize-info:CLOSURE (make-deserialize-name tag)])
        (with-syntax ([CLOSURE:serialize-info (make-id "~a:serialize-info")]
                      [make-CLOSURE (make-id "make-~a")]
                      [CLOSURE? (make-id "~a?")]
                      [CLOSURE-ref (make-id "~a-ref")]
                      [CLOSURE-set! (make-id "~a-set!")]
                      [CLOSURE-env (make-id "~a-env")]
                      [set-CLOSURE-env! (make-id "set-~a-env!")]
                      [struct:CLOSURE (make-id "struct:~a")])
          (values 
           #'make-CLOSURE
           (list
            #`(define #,deserialize-info:CLOSURE
                (make-deserialize-info
                 
                 ;; make-proc: value ... -> CLOSURE
                 #,(if (null? fvars)
                       #'(lambda () (make-CLOSURE))
                       #`(lambda #,fvars (make-CLOSURE (lambda () (values #,@fvars)))))
                 
                 ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
                 (lambda ()
                   (let ([new-closure
                          #,(if (null? fvars)
                                #'(make-CLOSURE)
                                #'(make-CLOSURE (lambda () (error "closure not initialized"))))])
                     (values
                      new-closure
                      #,(if (null? fvars)
                            #'void
                            #'(lambda (clsr)
                                (set-CLOSURE-env! new-closure (CLOSURE-env clsr)))))))))
            
            #`(provide #,deserialize-info:CLOSURE)
            
            #`(define CLOSURE:serialize-info
                (make-serialize-info
                 
                 ;; to-vector: CLOSURE -> vector
                 #,(if (null? fvars)
                       #'(lambda (clsr) (vector))
                       #'(lambda (clsr)
                           (call-with-values
                            (lambda () ((CLOSURE-env clsr)))
                            vector)))
                 
                 ;; The serializer id: --------------------
                 ;(syntax deserialize-info:CLOSURE)
                 ;; I still don't know what to put here.
                 ;; oh well.
                 ;(quote-syntax #,(syntax deserialize-info:CLOSURE))
                 (let ([b (identifier-binding (quote-syntax #,deserialize-info:CLOSURE))])
                   (if (list? b)
                       (cons '#,deserialize-info:CLOSURE (caddr b))
                       '#,deserialize-info:CLOSURE))
                 
                 ;; can-cycle?
                 #t
                 
                 ;; Directory for last-ditch resolution --------------------
                 (or (current-load-relative-directory) (current-directory))
                 ))
            
            #`(define-values (struct:CLOSURE make-CLOSURE CLOSURE? #,@(if (null? fvars)
                                                                          #'()
                                                                          #'(CLOSURE-env set-CLOSURE-env!)))
                (let-values ([(struct:CLOSURE make-CLOSURE CLOSURE? CLOSURE-ref CLOSURE-set!)
                              (make-struct-type '#,tag ;; the tag goes here
                                                #f  ; no super type
                                                #,(if (null? fvars) 0 1)
                                                0   ; number of auto-fields
                                                #f  ; auto-v
                                                
                                                ; prop-vals:
                                                (list (cons prop:serializable CLOSURE:serialize-info))
                                                
                                                #f  ; inspector
                                                
                                                ;; the struct apply proc:
                                                #,(if (null? fvars)
                                                      #`(lambda (clsr #,@formals)
                                                          #,proc-body)
                                                      #`(lambda (clsr #,@formals)
                                                          (let-values ([#,fvars ((CLOSURE-env clsr))])
                                                            #,proc-body)))
                                                )])
                  (values struct:CLOSURE make-CLOSURE CLOSURE?
                          #,@(if (null? fvars)
                                 #'()
                                 #'((lambda (clsr) (CLOSURE-ref clsr 0))
                                    (lambda (clsr new-env) (CLOSURE-set! clsr 0 new-env))))))))))))))