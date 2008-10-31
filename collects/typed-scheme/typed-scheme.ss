#lang scheme/base

(require (rename-in "utils/utils.ss" [infer r:infer]))

(require (private #;base-env base-types)
         (for-syntax 
          scheme/base
	  (private type-utils type-contract type-effect-convenience)
	  (typecheck typechecker provide-handling)
	  (env type-environments type-name-env type-alias-env)
	  (r:infer infer)
	  (utils tc-utils)
	  (rep type-rep)
	  (except-in (utils utils) infer extend)
          (only-in (r:infer infer-dummy) infer-param)
          scheme/nest
          syntax/kerncase
          scheme/match))




(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require]))

(define-for-syntax catch-errors? #f)

;(begin (init-tnames))


(define-syntax (module-begin stx)
  (define module-name (syntax-property stx 'enclosing-module-name))
  ;(printf "BEGIN: ~a~n" (syntax->datum stx))
  (with-logging-to-file 
   (build-path (find-system-path 'temp-dir) "ts-poly.log")
   #;
   (log-file-name (syntax-source stx) module-name)
   (syntax-case stx ()
     [(mb forms ...)
      (nest
          ([begin (set-box! typed-context? #t)
                  (start-timing module-name)]
           [with-handlers
               ([(lambda (e) (and catch-errors? (exn:fail? e) (not (exn:fail:syntax? e))))
                 (lambda (e) (tc-error "Internal error: ~a" e))])]
           [parameterize (;; a cheat to avoid units
                          [infer-param infer]
                          ;; do we report multiple errors
                          [delay-errors? #t]
                          ;; this parameter is for parsing types
                          [current-tvars initial-tvar-env]
                          ;; this parameter is just for printing types
                          ;; this is a parameter to avoid dependency issues
                          [current-type-names
                           (lambda ()
                             (append 
                              (type-name-env-map (lambda (id ty)
                                                   (cons (syntax-e id) ty)))
                              (type-alias-env-map (lambda (id ty)
                                                    (cons (syntax-e id) ty)))))]
                          ;; reinitialize seen type variables
                          [type-name-references null])]
           [begin (do-time "Initialized Envs")]
           ;; local-expand the module
           ;; pmb = #%plain-module-begin                            
           [with-syntax ([new-mod 
                          (local-expand (syntax/loc stx
                                          (#%plain-module-begin 
                                           forms ...))
                                        'module-begin 
                                        null)])]
           [with-syntax ([(pmb body2 ...) #'new-mod])]
           [begin (do-time "Local Expand Done")]
           [with-syntax ([after-code (parameterize ([orig-module-stx stx]
                                                    [expanded-module-stx #'new-mod])
                                       (type-check #'(body2 ...)))]
                         [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))]
                         [(transformed-body ...) (remove-provides #'(body2 ...))])]
           [with-syntax ([(transformed-body ...) (change-contract-fixups #'(transformed-body ...))])])
        (do-time "Typechecked")
        #;(printf "checked ~a~n" module-name)
        #;(printf "created ~a types~n" (count!))
        #;(printf "tried to create ~a types~n" (all-count!))
        #;(printf "created ~a union types~n" (union-count!))
        ;; reconstruct the module with the extra code
        #'(#%module-begin transformed-body ... after-code check-syntax-help))])))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . (module . rest))
     (eq? 'module (syntax-e #'module))
     #'(module . rest)]
    [(_ . form)     
     (nest
         ([begin (set-box! typed-context? #t)]
          [parameterize (;; a cheat to avoid units
                         [infer-param infer]
                         ;; this paramter is for parsing types
                         [current-tvars initial-tvar-env]
                         ;; this parameter is just for printing types
                         ;; this is a parameter to avoid dependency issues
                         [current-type-names
                          (lambda () 
                            (append 
                             (type-name-env-map (lambda (id ty)
                                                  (cons (syntax-e id) ty)))
                             (type-alias-env-map (lambda (id ty)
                                                   (cons (syntax-e id) ty)))))])]
         ;(do-time "Initialized Envs")
          ;; local-expand the module
          [let ([body2 (local-expand #'(#%top-interaction . form) 'top-level null)])]
          [parameterize ([orig-module-stx #'form]
                         [expanded-module-stx body2])]
          ;; typecheck the body, and produce syntax-time code that registers types
          [let ([type (tc-toplevel-form body2)])])
       (kernel-syntax-case body2 #f
         [(head . _)
          (or (free-identifier=? #'head #'define-values)
              (free-identifier=? #'head #'define-syntaxes)
              (free-identifier=? #'head #'require)
              (free-identifier=? #'head #'provide)
              (free-identifier=? #'head #'begin)
              (void? type)
              (type-equal? -Void (tc-result-t type)))
          body2]
         ;; construct code to print the type
         [_           
          (nest
              ([with-syntax ([b body2]
                             [ty-str (match type
                                       [(tc-result: t)
                                        (format "- : ~a\n" t)]
                                       [x (int-err "bad type result: ~a" x)])])])
            #`(let ([v b] [type 'ty-str])
                (begin0 
                  v
                  (printf type))))]))]))



