#lang racket/base

(require (rename-in "utils/utils.rkt" [infer r:infer])
         (private with-types)
         (for-syntax 
          (except-in syntax/parse id)
          racket/match unstable/syntax racket/base unstable/match
          (private type-contract optimize)
          (types utils convenience)
	  (typecheck typechecker provide-handling tc-toplevel)
	  (env type-environments type-name-env type-alias-env)
	  (r:infer infer)
	  (utils tc-utils)
	  (rep type-rep)
	  (except-in (utils utils) infer)
          (only-in (r:infer infer-dummy) infer-param)          
          "tc-setup.rkt"))

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type)

(define-syntax (module-begin stx)
  (syntax-parse stx
     [(mb (~optional (~and #:optimize (~bind [opt? #'#t]))) forms ...)
      (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))])
        (parameterize ([optimize? (or (optimize?) (attribute opt?))])
          (tc-setup 
           stx pmb-form 'module-begin new-mod tc-module after-code
           (with-syntax*
            (;; pmb = #%plain-module-begin    
             [(pmb . body2) new-mod]
             ;; add in syntax property on useless expression to draw check-syntax arrows
             [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))]
             ;; perform the provide transformation from [Culpepper 07]
             [transformed-body (remove-provides #'body2)]
             ;; add the real definitions of contracts on requires
             [transformed-body (change-contract-fixups #'transformed-body)]
             ;; potentially optimize the code based on the type information
             [(optimized-body ...)
              ;; do we optimize?
              (if (optimize?)
                  (begin0 (map optimize (syntax->list #'transformed-body))
                    (do-time "Optimized"))
                  #'transformed-body)])
            ;; reconstruct the module with the extra code
            ;; use the regular %#module-begin from `racket/base' for top-level printing
            #`(#%module-begin optimized-body ... #,after-code check-syntax-help)))))]))

(define-syntax (top-interaction stx)
  (syntax-parse stx 
    [(_ . ((~datum module) . rest))
     #'(module . rest)]
    [(_ . form)
     (tc-setup
      stx #'form 'top-level body2 tc-toplevel-form type
      (syntax-parse body2
        ;; any of these do not produce an expression to be printed
        [(head:invis-kw . _) body2]
        [_ (let ([ty-str (match type
                           ;; don't print results of type void
                           [(tc-result1: (== -Void type-equal?)) #f]
                           [(tc-result1: t f o)
                            (format "- : ~a\n" t)]
                           [(tc-results: t)
                            (format "- : ~a\n" (cons 'Values t))]
                           [x (int-err "bad type result: ~a" x)])])
             (if ty-str                  
                 #`(let ([type '#,ty-str])
                     (begin0 #,body2 (display type)))
                 body2))]))]))



