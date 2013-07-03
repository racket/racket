#lang racket/base
(require 2htdp/image
         racket/runtime-path
         (for-syntax "private/planetcute-image-list.rkt")
         (for-syntax racket/base))

(define-syntax (definitions stx)
  #`(begin
      #,@(for/list ([img (in-list (apply append (map cdr images)))])
           #`(begin
               (provide #,img)
               (define-syntax #,img (make-planetcute-transformer '#,img))))))


(define-for-syntax (make-planetcute-transformer img)
  (make-set!-transformer
   (let ([saved-id-table (make-hasheq)])
     (λ (stx)
       (if (eq? 'expression (syntax-local-context))
           ;; In an expression context:
           (let* ([key (syntax-local-lift-context)]
                  ;; Already lifted in this lifting context?
                  [lifted-id
                   (or (hash-ref saved-id-table key #f)
                       ;; No: lift the require for the image:
                       (syntax-local-lift-require `(lib ,(format "~a.rkt" img) "2htdp" "planetcute")
                                                  (datum->syntax stx img)))])
             (when key (hash-set! saved-id-table key lifted-id))
             ;; Expand to a use of the lifted expression:
             (with-syntax ([saved-id (syntax-local-introduce lifted-id)])
               (syntax-case stx (set!)
                 [name (identifier? #'name) #'saved-id]
                 [(set! id arg)
                  (raise-syntax-error
                   '2htdp/planetcute
                   "cannot set! a Planet Cute variable"
                   stx #'id)]
                 [(name . more)
                  (raise-syntax-error
                   '2htdp/planetcute
                   "the Planet Cute variables cannot be used after an open parenthesis as they are not functions"
                   stx #'id)])))
           ;; In case of partial expansion for module-level and internal-defn
           ;; contexts, delay expansion until it's a good time to lift
           ;; expressions:
           (quasisyntax/loc stx (#%expression #,stx)))))))

(definitions)
