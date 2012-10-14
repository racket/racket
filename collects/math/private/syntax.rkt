#lang racket/base

(require (for-syntax racket/base
                     syntax/strip-context
                     racket/syntax
                     typed-racket/utils/tc-utils)
         typed/racket/base)

(provide (all-defined-out))

(define-syntax (require/untyped-contract stx)
  (syntax-case stx (begin)
    [(_ (begin form ...) from-module-spec [name T] ...)
     (with-syntax* ([(typed-name ...)  (generate-temporaries #'(name ...))]
                    [(untyped-name ...)  (generate-temporaries #'(name ...))]
                    [(untyped2-name ...)  (generate-temporaries #'(name ...))]
                    [(macro-name ...)  (generate-temporaries #'(name ...))]
                    [(typed-module)  (generate-temporaries #'(typed-module))]
                    [(untyped-module)  (generate-temporaries #'(untyped-module))])
       (syntax/loc stx
         (begin
           (module typed-module typed/racket/base
             (begin form ...)
             (require (only-in from-module-spec name ...))
             (provide untyped-name ...)
             (: untyped-name T) ...
             (define untyped-name name) ...)
           
           (module untyped-module racket/base
             (require (for-syntax racket/base
                                  typed-racket/utils/tc-utils)
                      (rename-in from-module-spec [name typed-name] ...)
                      (rename-in (submod ".." typed-module) [untyped-name untyped2-name] ...))
             
             (provide macro-name ...)
             
             (define-for-syntax (rename-head stx id)
               (syntax-case stx ()
                 [(_ . args)  (quasisyntax/loc stx (#,id . args))]
                 [_  (quasisyntax/loc stx #,id)]))
             
             (define-syntax (macro-name stx)
               (if (unbox typed-context?)
                   (rename-head stx #'typed-name)
                   (rename-head stx #'untyped2-name)))
             ...)
           
           (require (rename-in (submod "." untyped-module) [macro-name name] ...)))))]
    [(_ from-module-spec [name T] ...)
     (syntax/loc stx (require/untyped-contract (begin) from-module-spec [name T] ...))]))

(define-syntax (syntax-value stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx (syntax-value expr (λ (x) x) (λ (x) x)))]
    [(_ expr encode decode)
     (syntax/loc stx
       (let ()
         (define-syntax (make-constant inner-stx)
           (define val expr)
           (with-syntax ([enc-val  (datum->syntax inner-stx (encode val))])
             (syntax/loc inner-stx (decode enc-val))))
         (make-constant)))]))
