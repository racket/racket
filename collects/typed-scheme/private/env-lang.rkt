#lang scheme/base

(require (rename-in "../utils/utils.ss" [infer r:infer]))

(require (for-syntax (utils tc-utils)
                     (env init-envs)
                     scheme/base syntax/parse
                     (r:infer infer)
                     (only-in (r:infer infer-dummy) infer-param)
                     (except-in (rep object-rep filter-rep type-rep) make-arr)
                     (types convenience union)
                     (only-in (types convenience) [make-arr* make-arr])))

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
	  (define-for-syntax e
	    (parameterize ([infer-param infer])
	      (make-env [id ty] ...)))
	  (begin-for-syntax
	   (initialize-type-env e))))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (except-out (all-from-out scheme/base) #%module-begin)
         types rep private utils
         (for-syntax          
          (types-out convenience union)
          (all-from-out scheme/base)))
