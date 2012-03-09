#lang racket/base
(require (for-syntax racket/base))

(begin-for-syntax
  (define module->submodule->stxs-box (make-hash))
  (define (lang=? x y)
    (cond
      [(and (identifier? x)
             (identifier? y))
       (free-identifier=? x y)]
      [(and (not (syntax->datum x))
            (not (syntax->datum y)))
       #t]
      [else
       #f]))
  (define (get-stxs-box the-submodule-stx lang-stx)
    (define the-module (syntax-source-module the-submodule-stx))
    (define submodule->stxs-box
      (hash-ref! module->submodule->stxs-box the-module make-hash))
    (define the-submodule-id
      (syntax->datum the-submodule-stx))
    (define lang-stx*stxs-box
      (hash-ref! submodule->stxs-box the-submodule-id
                 (λ ()
                   (when lang-stx
                     (syntax-local-lift-module-end-declaration
                      (quasisyntax/loc the-submodule-stx
                        (define-module #,the-submodule-stx))))
                   (cons lang-stx (box null)))))
    (values (car lang-stx*stxs-box)
            (cdr lang-stx*stxs-box))))

(define-syntax (module** stx)
  (syntax-case stx ()
    [(_ the-submodule lang e ...)
     (begin
       ;; This looks it up the first time and is allowed to create a
       ;; list if necessary
       (define-values (lang-should-be _)
         (get-stxs-box #'the-submodule #'lang))
       (unless (lang=? #'lang lang-should-be)
         (raise-syntax-error 'module** (format "All occurrences of module** for the same submodule should use the same language position; given ~e, where previous use had ~e" #'lang lang-should-be)))
       #'(begin-for-syntax
           (define-values (_ stxs-box)
             (get-stxs-box #'the-submodule #f))
           (set-box! stxs-box
                     (append (unbox stxs-box)
                             (syntax->list #'(e ...))))))]))

(define-syntax (define-module stx)
  (syntax-case stx ()
    [(_ the-submodule)
     (begin
       (define-values (lang-stx stxs-box)
         (get-stxs-box #'the-submodule #f))
       (quasisyntax/loc #'the-submodule
         (module* the-submodule #,lang-stx
           #,@(unbox stxs-box))))]))

(define-syntax-rule (define-shorthand when-testing test)
  (define-syntax (when-testing stx)
    (syntax-case stx ()
      [(_ e (... ...))
       (quasisyntax/loc stx
         (module** #,(datum->syntax stx 'test) #f 
           e (... ...)))])))
(define-shorthand when-testing test)

(provide module**
         when-testing)
