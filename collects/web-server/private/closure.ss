#lang scheme/base
(require (for-template scheme/base)
         (for-template mzlib/serialize)
         mzlib/list
         scheme/contract
         mzlib/serialize)
(provide/contract
 [closure->deserialize-name (serializable? . -> . symbol?)])
(provide make-closure-definition-syntax) 

(define (closure->deserialize-name proc)
  (cdr (first (third (serialize proc)))))

(define (make-closure-definition-syntax tag fvars proc)
  (define (make-id str)
    (datum->syntax tag (string->symbol (format str (syntax->datum tag)))))
  (with-syntax ([CLOSURE:deserialize-info (make-id "~a:deserialize-info")]
                [CLOSURE:serialize-info (make-id "~a:serialize-info")]
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
        (define CLOSURE:deserialize-info
          (make-deserialize-info
           
           ;; make-proc: value ... -> CLOSURE
           (lambda args
             (apply #,(if (null? fvars)
                          (syntax/loc proc
                            (#%plain-lambda () (#%plain-app make-CLOSURE)))
                          (quasisyntax/loc proc 
                            (#%plain-lambda #,fvars
                                            (#%plain-app make-CLOSURE
                                                         (#%plain-lambda ()
                                                                         (#%plain-app values #,@fvars))))))
                    args))
           
           ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
           (lambda ()
             (let ([new-closure
                    #,(if (null? fvars)
                          (syntax/loc proc (#%plain-app make-CLOSURE))
                          (syntax/loc proc
                            (#%plain-app make-CLOSURE
                                         (#%plain-lambda () (#%plain-app error "closure not initialized")))))])
               (values
                new-closure
                #,(if (null? fvars)
                      (syntax/loc proc void)
                      (syntax/loc proc 
                        (#%plain-lambda (clsr)
                                        (#%plain-app set-CLOSURE-env! new-closure (#%plain-app CLOSURE-env clsr)))))))))))
      
      (quasisyntax/loc proc 
        (provide CLOSURE:deserialize-info))
      
      (quasisyntax/loc proc 
        (define CLOSURE:serialize-info
          (make-serialize-info
           
           ;; to-vector: CLOSURE -> vector
           #,(if (null? fvars)
                 (syntax/loc proc (#%plain-lambda (clsr) (#%plain-app vector)))
                 (syntax/loc proc
                   (#%plain-lambda (clsr)
                                   (#%plain-app call-with-values
                                                (#%plain-lambda () (#%plain-app (#%plain-app CLOSURE-env clsr)))
                                                vector))))
           
           ;; The serializer id: --------------------
           ;(syntax deserialize-info:CLOSURE)
           ;; I still don't know what to put here.
           ;; oh well.
           ;(quote-syntax #,(syntax deserialize-info:CLOSURE))
           (let ([b (identifier-binding (quote-syntax CLOSURE:deserialize-info))])
             (if (list? b)
                 (cons 'CLOSURE:deserialize-info (caddr b))
                 'CLOSURE:deserialize-info))
           
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
                        (make-struct-type 
                         '#,tag ;; the tag goes here
                         #f  ; no super type
                         #,(if (null? fvars) 0 1)
                         0   ; number of auto-fields
                         #f  ; auto-v
                         
                         ; prop-vals:
                         (list (cons prop:serializable CLOSURE:serialize-info)
                               (cons prop:procedure
                                     #,(if (null? fvars)
                                           (quasisyntax/loc proc
                                             (#%plain-lambda (clsr . args)
                                                             (#%plain-app apply #,proc args)))
                                           (quasisyntax/loc proc
                                             (#%plain-lambda (clsr . args)
                                                             (let-values ([#,fvars (#%plain-app 
                                                                                    (#%plain-app CLOSURE-env clsr))])
                                                               (#%plain-app apply #,proc args)))))))
                         
                         #f  ; inspector
                         
                         ;; the struct apply proc:
                         #f)])
            (values struct:CLOSURE make-CLOSURE CLOSURE?
                    #,@(if (null? fvars)
                           (syntax/loc proc ())
                           (syntax/loc proc
                             ((#%plain-lambda (clsr) (#%plain-app CLOSURE-ref clsr 0))
                              (#%plain-lambda (clsr new-env) (#%plain-app CLOSURE-set! clsr 0 new-env)))))))))))))
