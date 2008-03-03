#lang scheme/base

(require scheme/contract (for-syntax scheme/base syntax/kerncase))
(provide require/contract define-ignored)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr 'expression 
                                              (list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) e*)
        #'(begin (define-values (n) e)
                 (define name e*))]
       [e #'(define name e)])]))

(define-syntax (require/contract stx)
  (syntax-case stx ()
    [(require/contract nm cnt lib)
     #`(begin (require (only-in lib [nm tmp]))     
              (define-ignored nm (contract cnt tmp '#,(syntax->datum #'nm) 'never-happen #'#,stx)))]))
#|
(module a mzscheme
  (provide x)
  (define (x a) 'hi))

(module z mzscheme
  (require require-contract)
  
  (require (lib "contract.ss"))
  
  (define-struct b (X Y))
  
  (require/contract x (b? . -> . b?) a )
  
  (x 'no)
  )

(require z)
|#
