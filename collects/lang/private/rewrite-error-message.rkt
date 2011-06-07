#lang scheme/base

(require mzlib/etc 
         mzlib/list
         (for-template scheme/base "rewrite-error-message-for-tpl.rkt")
         (for-syntax "firstorder.ss"
                     scheme/base))

(provide wrap-top-for-lookup-error-message
         wrap-for-contract-error-message)

(define (wrap-top-for-lookup-error-message stx was-in-app-position)
  (syntax-case stx ()
    [(_ . id)
     (quasisyntax/loc 
      stx
      (with-handlers ([exn:fail:contract:variable?
                       (lambda (e) (rewrite-lookup-error-message e #'id #,was-in-app-position))])
                     (#%top . id)))]))


(define ((wrap-for-contract-error-message app) orig-name stx)
  (syntax-case stx ()
    [(id . args)
     (quasisyntax/loc stx
                      (with-handlers ([exn:fail:contract? (compose raise rewrite-contract-error-message)])
                                     #,(quasisyntax/loc stx (#,app #,orig-name . args))))]))


