#lang racket/base

(require racket/match
         rackunit
         syntax/srcloc
         syntax/strip-context)

(module+ test
  (module config info
    (define timeout 180)))

(define blame-parties
  (make-immutable-hash
   (list (let ([stx #'here-syntax])
           (cons #`(quote-syntax #,stx)
                 (source-location->string stx)))
         (cons #''"here string" "here string")
         (cons #'(module-path-index-join ''somewhere #f)
               ''somewhere)
         (cons #''from-macro ''macro)
         (cons #''use-site ''use)
         (cons #''unknown "unknown"))))

(define-binary-check (check-blame-party=? actual expected)
  (match actual
    ; module names can be uninterned symbols while they’re being expanded (to represent the fact that
    ; compile-time module paths can be completely unrelated from the one a module will eventually have
    ; once declared), but for our purposes we really do want to compare symbolic names
    [`',mod-name
     (equal? `',(string->symbol (symbol->string mod-name)) expected)]
    [_
     (equal? actual expected)]))

(define (elevate-phase stx #:phase-shift phase-shift)
  (for/fold ([stx stx])
            ([i (in-range phase-shift)])
    #`(begin-for-syntax #,stx)))

(for* ([definition-phase-shift (in-range 3)]
       [expansion-phase-shift (in-range 3)]
       [party-to-blame (in-list '(positive negative))]
       [(blame-party-expr blame-party-val) (in-hash blame-parties)])
  (with-check-info (['definition-phase-shift definition-phase-shift]
                    ['expansion-phase-shift expansion-phase-shift]
                    ['party-to-blame party-to-blame]
                    ['blame-party-expr blame-party-expr]
                    ['blame-party-val blame-party-val])

    (parameterize ([current-namespace (make-base-namespace)])
      ; smuggle contract system predicates/accessors out of the relevant phase so that we can inspect
      ; the exception and the blame object inside
      (define-values [exn:fail:contract:blame?
                      exn:fail:contract:blame-object
                      blame-positive
                      blame-original?
                      blame-swapped?]
        (values #f #f #f #f #f))
      (eval (strip-context
             #`(module smuggler racket/base
                 (require racket/contract)
                 (#,(λ args (set!-values [exn:fail:contract:blame?
                                          exn:fail:contract:blame-object
                                          blame-positive
                                          blame-original?
                                          blame-swapped?]
                                         (apply values args)))
                  exn:fail:contract:blame?
                  exn:fail:contract:blame-object
                  blame-positive
                  blame-original?
                  blame-swapped?))))

      ; define a macro that uses expr/c at some phase
      (eval (strip-context
             #`(module macro racket/base
                 (require
                   ; imports for `begin-for-syntax`es in this module and in expansion
                   #,@(for/list ([phase (in-range 1 (+ definition-phase-shift
                                                       expansion-phase-shift))])
                        #`(for-meta #,phase (only-in racket/base begin-for-syntax)))
                   ; imports for macro definition
                   (for-meta #,definition-phase-shift racket/base syntax/parse/define)
                   (for-meta #,(add1 definition-phase-shift) racket/base)
                   ; imports for expressions in macro expansion
                   (for-meta #,(+ definition-phase-shift expansion-phase-shift)
                             (only-in racket/base
                                      #%expression
                                      exact-integer?)))
                 #,(elevate-phase
                    #:phase-shift definition-phase-shift
                    #`(begin
                        (provide integer-only)
                        (define-simple-macro (integer-only e)
                          #:declare e (expr/c #'exact-integer?
                                              #:phase (+ (syntax-local-phase-level)
                                                         #,expansion-phase-shift)
                                              #:arg? #,(eq? party-to-blame 'negative)
                                              #,(match party-to-blame
                                                  ['positive #'#:positive]
                                                  ['negative #'#:negative])
                                              #,blame-party-expr)
                          #,(elevate-phase
                             #:phase-shift expansion-phase-shift
                             #`(#%expression e.c))))))))

      (for ([use-phase-shift (in-range 3)])
        (with-check-info (['use-phase-shift use-phase-shift])
          ; use the macro at some phase and catch the exception
          (define exn
            (with-handlers ([(λ (exn) (exn:fail:contract:blame? exn)) values])
              (eval (strip-context
                     #`(module use racket/base
                         (require
                           ; imports for `begin-for-syntax`es in this module
                           #,@(for/list ([phase (in-range 1 (+ definition-phase-shift
                                                               use-phase-shift))])
                                #`(for-meta #,phase (only-in racket/base begin-for-syntax)))
                           ; import for expression embedded in expansion (and value smuggling)
                           (for-meta #,(+ definition-phase-shift
                                          expansion-phase-shift
                                          use-phase-shift)
                                     (only-in racket/base quote)
                                     'smuggler)
                           ; import for the macro
                           (for-meta #,use-phase-shift 'macro))
                         #,(elevate-phase
                            #:phase-shift (+ definition-phase-shift use-phase-shift)
                            #'(integer-only 'not-an-integer)))))
              (when (zero? (+ definition-phase-shift expansion-phase-shift use-phase-shift))
                (eval '(require 'use)))
              #f))

          ; make sure the right party was blamed
          (check-not-false exn "No exception raised")
          (define blame (exn:fail:contract:blame-object exn))
          (match party-to-blame
            ['positive (check-pred blame-original? blame)]
            ['negative (check-pred blame-swapped? blame)])
          (check-blame-party=? (blame-positive blame) blame-party-val))))))
