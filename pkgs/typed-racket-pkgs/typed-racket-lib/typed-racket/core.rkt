#lang racket/base

(require (rename-in "utils/utils.rkt")
         (for-syntax racket/base)
         (for-template racket/base)
         (private with-types type-contract)
         (except-in syntax/parse id)
         racket/match racket/syntax
         (types utils abbrev generalize)
         (typecheck provide-handling tc-toplevel tc-app-helper)
         (rep type-rep)
         (for-template (base-env top-interaction))
         (utils utils tc-utils arm)
         "tc-setup.rkt")

(provide mb-core ti-core wt-core)

(define (mb-core stx init)
  (syntax-parse stx
    [(mb (~optional (~or (~and #:optimize    (~bind [opt? #'#t])) ; kept for backward compatibility
                         (~and #:no-optimize (~bind [opt? #'#f]))))
         forms ...)
     (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))])
       (parameterize ([optimize? (or (and (not (attribute opt?)) (optimize?))
                                     (and (attribute opt?) (syntax-e (attribute opt?))))])
         (tc-setup
          stx pmb-form 'module-begin new-mod init tc-module before-code after-code
          (with-syntax*
           (;; pmb = #%plain-module-begin
            [(pmb . body2) new-mod]
            ;; perform the provide transformation from [Culpepper 07]
            [transformed-body (begin0 (remove-provides #'body2) (do-time "Removed provides"))]
            ;; add the real definitions of contracts on requires
            [transformed-body (begin0 (change-contract-fixups #'transformed-body) (do-time "Fixed contract ids"))]
            ;; potentially optimize the code based on the type information
            [(optimized-body ...) (maybe-optimize #'transformed-body)] ;; has own call to do-time
            ;; add in syntax property on useless expression to draw check-syntax arrows
            [check-syntax-help (syntax-property
                                (syntax-property
                                 #'(void)
                                 'disappeared-binding (disappeared-bindings-todo))
                                'disappeared-use (disappeared-use-todo))])
           ;; reconstruct the module with the extra code
           ;; use the regular %#module-begin from `racket/base' for top-level printing
           (arm #`(#%module-begin #,before-code optimized-body ... #,after-code check-syntax-help))))))]))

(define did-I-suggest-:print-type-already? #f)
(define :print-type-message " ... [Use (:print-type <expr>) to see more.]")
(define (ti-core stx init)
  (current-type-names (init-current-type-names))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ . (module . rest))
     #'(module . rest)]
    [(_ . (~and form ((~var command (static interactive-command? #f)) . _)))
     ((interactive-command-procedure (attribute command.value)) #'form init)]
    [(_ . form)
     (init)
     (tc-setup
      stx #'form 'top-level body2 void tc-toplevel-form before type
      (with-syntax*
       ([(optimized-body . _) (maybe-optimize #`(#,body2))])
       (syntax-parse body2
         ;; any of these do not produce an expression to be printed
         [(head:invis-kw . _) (arm #'optimized-body)]
         [_ (let ([ty-str (match type
                            ;; don't print results of type void
                            [(tc-result1: (== -Void type-equal?))
                             #f]
                            ;; don't print results of unknown type
                            [(tc-any-results:)
                             #f]
                            [(tc-result1: t f o)
                             ;; Don't display the whole types at the REPL. Some case-lambda types
                             ;; are just too large to print.
                             ;; Also, to avoid showing too precise types, we generalize types
                             ;; before printing them.
                             (define tc (cleanup-type t))
                             (define tg (generalize tc))
                             (format "- : ~a~a~a\n"
                                     tg
                                     (cond [(equal? tc tg) ""]
                                           [else (format " [more precisely: ~a]" tc)])
                                     (cond [(equal? tc t) ""]
                                           [did-I-suggest-:print-type-already? " ..."]
                                           [else (set! did-I-suggest-:print-type-already? #t)
                                                 :print-type-message]))]
                            [(tc-results: t)
                             (define tcs (map cleanup-type t))
                             (define tgs (map generalize tcs))
                             (format "- : ~a~a~a\n"
                                     (cons 'Values tgs)
                                     (cond [(andmap equal? tgs tcs) ""]
                                           [else (format " [more precisely: ~a]" (cons 'Values tcs))])
                                     ;; did any get pruned?
                                     (cond [(andmap equal? t tcs) ""]
                                           [did-I-suggest-:print-type-already? " ..."]
                                           [else (set! did-I-suggest-:print-type-already? #t)
                                                 :print-type-message]))]
                            [x (int-err "bad type result: ~a" x)])])
              (if ty-str
                  #`(let ([type '#,ty-str])
                      (begin0 #,(arm #'optimized-body) (display type)))
                  (arm #'optimized-body)))])))]))
