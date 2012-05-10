#lang racket/base
(require racket/list
         syntax/free-vars
         racket/syntax
         (for-template
          racket/base
          racket/serialize)
         "util.rkt")

(define (define-closure! label fvars stx)
  ; Boxes
  (define make-CLOSURE-box
    (syntax-local-lift-expression
     (quasisyntax/loc stx
       (box (lambda (env) (error 'make-CLOSURE "Closure<~.s> not initialized" '#,label))))))
  (define CLOSURE-set-env!-box
    (syntax-local-lift-expression
     (quasisyntax/loc stx
       (box (lambda (clsr new-env) (error 'CLOSURE-set-env! "Closure<~.s> not initialized" '#,label))))))
  (define CLOSURE-env-box
    (syntax-local-lift-expression
     (quasisyntax/loc stx
       (box (lambda (clsr) (error 'CLOSURE-env "Closure<~.s> not initialized" '#,label))))))
  ; Define the deserializer (req closure struct values under lambdas)
  (define CLOSURE:deserialize-info-id
    (syntax-local-lift-expression
     (quasisyntax/loc stx
       (make-deserialize-info
        
        ;; make-proc: value ... -> CLOSURE
        (lambda args
          (apply (#%plain-lambda #,fvars
                                 ((unbox #,make-CLOSURE-box) (#%plain-lambda () (values #,@fvars))))
                 args))
        
        ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
        (lambda ()
          (let ([new-closure
                 ((unbox #,make-CLOSURE-box)
                  (#%plain-lambda () (error 'deserialize "closure not initialized")))])
            (values
             new-closure
             (#%plain-lambda (clsr)
                             ((unbox #,CLOSURE-set-env!-box) new-closure ((unbox #,CLOSURE-env-box) clsr))))))))))
  ; Define the serializer (req closure struct values + deserializer identifier)
  (define CLOSURE:serialize-info-id
    (syntax-local-lift-expression
     (quasisyntax/loc stx
       (make-serialize-info
        
        ;; to-vector: CLOSURE -> vector
        (#%plain-lambda (clsr)
                        (#%plain-app call-with-values
                                     (#%plain-lambda () (((unbox #,CLOSURE-env-box) clsr)))
                                     vector))
        
        ;; The serializer id: --------------------
        (quote-syntax #,CLOSURE:deserialize-info-id)
        
        ;; can-cycle?
        #t
        
        ;; Directory for last-ditch resolution --------------------
        (or (current-load-relative-directory) (current-directory))))))
  ; Define the closure struct (req serialize info value)
  (define fun-name
    (or (syntax-local-name) (generate-temporary)))
  (define-values
    (make-CLOSURE-id CLOSURE?-id CLOSURE-env-id CLOSURE-set-env!-id)
    (apply
     values
     (syntax-local-lift-values-expression
      4
      (quasisyntax/loc stx
        (letrec-values ([(struct:CLOSURE make-CLOSURE CLOSURE? CLOSURE-ref CLOSURE-set!)
                         (make-struct-type 
                          '#,label ;; the tag goes here
                          #f  ; no super type
                          1
                          0   ; number of auto-fields
                          #f  ; auto-v
                          
                          ; prop-vals:
                          (list
                           (cons prop:serializable #,CLOSURE:serialize-info-id)
                           (cons prop:procedure
                                 (make-keyword-procedure
                                  (lambda (kws kw-vals clsr . rst)
                                    (let-values ([#,fvars ((CLOSURE-ref clsr 0))])
                                      (let ([#,fun-name #,stx])
                                        (keyword-apply #,fun-name
                                                       kws kw-vals
                                                       rst)))))))
                          
                          #f  ; inspector
                          
                          ;; the struct apply proc:
                          #f)]
                        [(CLOSURE-env)
                         (#%plain-lambda (clsr) (CLOSURE-ref clsr 0))]
                        [(CLOSURE-set-env!)
                         (#%plain-lambda (clsr new-env) (CLOSURE-set! clsr 0 new-env))])
          (set-box! #,CLOSURE-env-box CLOSURE-env)
          (set-box! #,CLOSURE-set-env!-box CLOSURE-set-env!)
          (set-box! #,make-CLOSURE-box make-CLOSURE)
          (values make-CLOSURE CLOSURE? CLOSURE-env CLOSURE-set-env!))))))
  ; Provide the deserializer (req deserializer identifier)
  (syntax-local-lift-provide
   (quasisyntax/loc stx
     #,CLOSURE:deserialize-info-id))
  (values make-CLOSURE-id CLOSURE?-id CLOSURE-env-id))

(define (make-closure stx)
  (syntax-case stx  ()
    [(_ label lambda-stx)
     (let*-values 
         ([(lambda-fe-stx) (local-expand #'lambda-stx 'expression empty)]
          [(fvars) (free-vars (disarm lambda-fe-stx))]
          ; Define the closure struct (req serialize info value)
          [(make-CLOSURE-id CLOSURE?-id CLOSURE-env-id)
           (define-closure! #'label fvars lambda-fe-stx)])
       ; Instantiate the closure
       (quasisyntax/loc stx
         (#,make-CLOSURE-id (#%plain-lambda () (values #,@fvars)))))]))

(provide
 make-closure
 define-closure!)
