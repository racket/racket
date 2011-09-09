#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer]))

(require (for-syntax scheme/base syntax/parse)
         (utils tc-utils)
         (env init-envs)
         (r:infer infer)
         (only-in (r:infer infer-dummy) infer-param)
         (except-in (rep object-rep filter-rep type-rep) make-arr)
         (types convenience union filter-ops)
         (only-in (types convenience) [make-arr* make-arr]))

(define-syntax (-#%module-begin stx)
  (define-syntax-class clause
    #:description "[id type]"
    (pattern [id:identifier ty]))
  (syntax-parse stx #:literals (require begin)
    [(mb (~optional (~and extra (~or (begin . _) (require . args))))
	 ~! :clause ...)
     #'(#%plain-module-begin
	(begin
	  extra
	  (define e
	    (parameterize ([infer-param infer])
	      (make-env [id (λ () ty)] ...)))
	  (define (init)
	   (initialize-type-env e))
          (provide init)))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (except-out (all-from-out racket/base) #%module-begin)
         types rep private utils
         (types-out convenience union filter-ops))
