#lang racket/base
(require (for-syntax racket/base))

(begin-for-syntax
  (define module->submodule->stxs-box (make-weak-hash))
  (define (get-stxs-box the-submodule-stx lift?)
    (define the-module (syntax-source-module the-submodule-stx))
    (define submodule->stxs-box
      (hash-ref! module->submodule->stxs-box the-module make-weak-hasheq))
    (define the-submodule-id
      (syntax->datum the-submodule-stx))
    (define stxs-box
      (hash-ref! submodule->stxs-box the-submodule-id
                 (λ ()
                   (when lift?
                     (syntax-local-lift-module-end-declaration
                      (quasisyntax/loc the-submodule-stx
                        (define-module #,the-submodule-stx))))
                   (box null))))
    stxs-box))

(define-syntax (slice stx)
  (syntax-case stx ()
    [(_ the-submodule e ...)
     (identifier? #'the-submodule)
     (begin
       ;; This looks it up the first time and is allowed to create a
       ;; list if necessary
       (get-stxs-box #'the-submodule #t)
       #'(begin-for-syntax
           (define stxs-box
             (get-stxs-box #'the-submodule #f))
           (set-box! stxs-box
                     (append (unbox stxs-box)
                             (syntax->list #'(e ...))))))]))

(define-syntax (define-module stx)
  (syntax-case stx ()
    [(_ the-submodule)
     (begin
       (define stxs-box
         (get-stxs-box #'the-submodule #f))
       (quasisyntax/loc #'the-submodule
         (module* the-submodule #f
           #,@(unbox stxs-box))))]))

(provide slice)
