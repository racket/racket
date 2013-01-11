#lang racket/base

(require (rename-in "utils/utils.rkt")
         (for-syntax racket/base)
         (for-template racket/base)
         (private with-types type-contract parse-type)
         (except-in syntax/parse id)
         racket/match racket/syntax unstable/match racket/list syntax/stx
         (types utils abbrev generalize printer)
         (typecheck provide-handling tc-toplevel tc-app-helper)
         (rep type-rep)
         (env env-req)
         (for-template (only-in (base-env prims) :type :print-type :query-type/result))
         (utils utils tc-utils arm)
         "tc-setup.rkt" "utils/debug.rkt")

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
  (syntax-parse stx
    [(_ . ((~datum module) . rest))
     #'(module . rest)]
    [(_ . ((~literal :type) ty:expr))
     #`(display #,(format "~a\n" (parse-type #'ty)))]
    ;; Prints the _entire_ type. May be quite large.
    [(_ . ((~literal :print-type) e:expr))
     (tc-setup #'stx #'e 'top-level expanded init tc-toplevel-form before type
               #`(display
                  #,(parameterize ([print-multi-line-case-> #t])
                      (format "~a\n" (match type
                                       [(tc-result1: t f o) t]
                                       [(tc-results: t) (cons 'Values t)])))))]
    ;; given a function and input types, display the result type
    [(_ . ((~literal :query-type/args) op:expr arg-type:expr ...))
     (with-syntax ([(dummy-arg ...) (generate-temporaries #'(arg-type ...))])
       (tc-setup #'stx
                 ;; create a dummy function with the right argument types
                 #`(lambda #,(stx-map (lambda (a t)
                                        (syntax-property a 'type-label t))
                                      #'(dummy-arg ...) #'(arg-type ...))
                     (op dummy-arg ...))
                 'top-level expanded init tc-toplevel-form before type
                 #`(display
                    #,(format "~a\n"
                              (match type
                                [(tc-result1: (and t (Function: _)) f o) t])))))]
    ;; given a function and a desired return type, fill in the blanks
    [(_ . ((~literal :query-type/result) op:expr desired-type:expr))
     (let ([expected (parse-type #'desired-type)])
       (tc-setup #'stx #'op 'top-level expanded init tc-toplevel-form before type
                 (match type
                   [(tc-result1: (and t (Function: _)) f o)
                    (let ([cleaned (cleanup-type t expected)])
                      #`(display
                         #,(match cleaned
                             [(Function: '())
                              "Desired return type not in the given function's range."]
                             [(Function: arrs)
                              (format "~a\n" cleaned)])))]
                   [_ (error (format "~a: not a function" (syntax->datum #'op) ))])))]
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
                                           [else (format " [generalized from ~a]" tc)])
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
                                           [else (format " [generalized from ~a]" (cons 'Values tcs))])
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
