#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     racket/pretty
                     "../errortrace-lib.rkt"
                     "../stacktrace.rkt"
                     "../private/utils.rkt"))

(provide (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ lang . body)
     (let ([e (let ([expanded-e
                     (local-expand #`(module . #,(strip-context #`(n lang . body)))
                                   'top-level
                                   null)])
                (parameterize ([original-stx stx]
                               [expanded-stx expanded-e])
                  (annotate-top expanded-e 0)))])
       (collect-garbage)
       (syntax-case e ()
         [(mod nm lang (mb . body)) 
          #`(#%plain-module-begin 
             (require (only-in lang))
             #,(generate-key-imports ((count-meta-levels 0) #'(begin . body)))
             . body)]))]))
