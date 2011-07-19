#lang racket/base

(require (rename-in "utils/utils.rkt" [infer r:infer])
         (for-syntax racket/base)
         (for-template racket/base)
         (private with-types type-contract)
         (except-in syntax/parse id)
         racket/match racket/syntax unstable/match racket/list
         (types utils convenience)
         (typecheck typechecker provide-handling tc-toplevel tc-app-helper)
         (env type-name-env type-alias-env)
         (r:infer infer)
         (rep type-rep)
         (except-in (utils utils tc-utils arm) infer)
         (only-in (r:infer infer-dummy) infer-param)
         "tc-setup.rkt")

(provide mb-core ti-core wt-core)

(define (mb-core stx)
  (syntax-parse stx
    [(mb (~optional (~or (~and #:optimize    (~bind [opt? #'#t])) ; kept for backward compatibility
                         (~and #:no-optimize (~bind [opt? #'#f]))))
         forms ...)
     (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))])
       (parameterize ([optimize? (or (and (not (attribute opt?)) (optimize?))
                                     (and (attribute opt?) (syntax-e (attribute opt?))))])
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
            [(optimized-body ...) (maybe-optimize #'transformed-body)])
           ;; reconstruct the module with the extra code
           ;; use the regular %#module-begin from `racket/base' for top-level printing
           (arm #`(#%module-begin optimized-body ... #,after-code check-syntax-help))))))]))

;; Don't display the whole types at the REPL. Some case-lambda types are just too large to print.
;; Returns the type to be printed, and whether the type was pruned or not.
(define (cleanup-type t)
  (match t
    ;; function type, prune if possible.
    [(Function: (list (arr: doms rngs rests drests kws) ...))
     (let-values ([(pdoms rngs rests drests) (possible-domains doms rests drests rngs #f)])
       (let ([res (make-Function (map make-arr
                                      pdoms rngs rests drests (make-list (length pdoms) null)))])
         (values res (not (equal? res t)))))]
    ;; not a function type. display as is.
    [_ (values t #f)]))

(define (ti-core stx)
  (syntax-parse stx
    [(_ . ((~datum module) . rest))
     #'(module . rest)]
    [(_ . form)
     (tc-setup
      stx #'form 'top-level body2 tc-toplevel-form type
      (with-syntax*
       ([optimized-body (car (maybe-optimize #`(#,body2)))])
       (syntax-parse body2
         ;; any of these do not produce an expression to be printed
         [(head:invis-kw . _) (arm #'optimized-body)]
         [_ (let ([ty-str (match type
                            ;; don't print results of type void
                            [(tc-result1: (== -Void type-equal?))
                             #f]
                            [(tc-result1: t f o)
                             (let-values ([(t pruned?) (cleanup-type t)])
                               (format "- : ~a~a\n" t (if pruned? " and more..." "")))]
                            [(tc-results: t)
                             ;; map the first component and ormap the second.
                             (define-values (ts any-pruned?)
                               (for/fold ([ts '()]
                                          [pruned? #f])
                                   ([orig t])
                                 (let-values ([(t new-pruned?) (cleanup-type orig)])
                                   (values (cons t ts) (or pruned? new-pruned?)))))
                             (format "- : ~a~a\n"
                                     (cons 'Values (reverse ts))
                                     (if any-pruned? " and more...\n" ""))]
                            [x (int-err "bad type result: ~a" x)])])
              (if ty-str
                  #`(let ([type '#,ty-str])
                      (begin0 #,(arm #'optimized-body) (display type)))
                  (arm #'optimized-body)))])))]))
