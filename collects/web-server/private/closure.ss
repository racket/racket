(module closure mzscheme
  (require-for-template mzscheme
                        (lib "serialize.ss")
                        (lib "etc.ss"))
  (require (lib "list.ss")
           (lib "serialize.ss"))
  (provide make-closure-definition-syntax
           closure->deserialize-name) 
  
  (define (closure->deserialize-name proc)
    (cdr (first (second (serialize proc)))))
  
  ;; borrowed this from Matthew's code
  ;; creates the deserialize-info identifier
  (define (make-deserialize-name id)
    (datum->syntax-object
     id
     (string->symbol
      (format "web-deserialize-info:~a" (syntax-e id)))
     id))
  
  (define (make-closure-definition-syntax tag fvars proc)
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
           (syntax/loc proc make-CLOSURE)
           (list
            (quasisyntax/loc proc 
              (define #,deserialize-info:CLOSURE
                (make-deserialize-info
                 
                 ;; make-proc: value ... -> CLOSURE
                 (lambda args
                   (apply #,(if (null? fvars)
                                (syntax/loc proc (lambda () (make-CLOSURE)))
                                (quasisyntax/loc proc (lambda #,fvars (make-CLOSURE (lambda () (values #,@fvars))))))
                          args))
                 
                 ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
                 (lambda ()
                   (let ([new-closure
                          #,(if (null? fvars)
                                (syntax/loc proc (make-CLOSURE))
                                (syntax/loc proc (make-CLOSURE (lambda () (error "closure not initialized")))))])
                     (values
                      new-closure
                      #,(if (null? fvars)
                            (syntax/loc proc void)
                            (syntax/loc proc 
                              (lambda (clsr)
                                (set-CLOSURE-env! new-closure (CLOSURE-env clsr)))))))))))
            
            (quasisyntax/loc proc 
              (provide #,deserialize-info:CLOSURE))
            
            (quasisyntax/loc proc 
              (define CLOSURE:serialize-info
                (make-serialize-info
                 
                 ;; to-vector: CLOSURE -> vector
                 #,(if (null? fvars)
                       (syntax/loc proc (lambda (clsr) (vector)))
                       (syntax/loc proc
                         (lambda (clsr)
                           (call-with-values
                            (lambda () ((CLOSURE-env clsr)))
                            vector))))
                 
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
                 )))
            
            (quasisyntax/loc proc
              (define-values (struct:CLOSURE make-CLOSURE CLOSURE? 
                                             #,@(if (null? fvars)
                                                    (syntax/loc proc ())
                                                    (syntax/loc proc (CLOSURE-env set-CLOSURE-env!))))
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
                                                      (quasisyntax/loc proc
                                                        (lambda (clsr . args)
                                                          (apply #,proc args)))
                                                      (quasisyntax/loc proc
                                                        (lambda (clsr . args)
                                                          (let-values ([#,fvars ((CLOSURE-env clsr))])
                                                            (apply #,proc args)))))
                                                )])
                  (values struct:CLOSURE make-CLOSURE CLOSURE?
                          #,@(if (null? fvars)
                                 (syntax/loc proc ())
                                 (syntax/loc proc
                                   ((lambda (clsr) (CLOSURE-ref clsr 0))
                                    (lambda (clsr new-env) (CLOSURE-set! clsr 0 new-env))))))))))))))))