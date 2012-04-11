#lang racket/base

(require syntax/parse
         "debug.rkt"
         (for-syntax racket/base "debug.rkt" syntax/parse
                     macro-debugger/emit)
         "literals.rkt")

;; to get syntax as a literal
(require (for-template racket/base))

(provide (all-defined-out))

(define (honu->racket forms)
  (define-literal-set literals (%racket))
  forms
  #;
  (syntax-parse forms #:literal-sets (literals)
    #:literals ([literal-syntax syntax])
    [(%racket x) #'x
    #;
     (honu->racket #'x)]
    [(literal-syntax form) #'#'form]
    [(form ...)
     (datum->syntax forms
                    (map honu->racket (syntax->list #'(form ...)))
                    forms
                    forms)]
    [x #'x]
    [() forms]))

(define (strip-stops code)
  (define-syntax-class stopper #:literal-sets (cruft)
    #;
    [pattern semicolon]
    [pattern honu-comma]
    [pattern colon])
  #;
  (syntax-parse code
    [(x:stopper rest ...) (strip-stops #'(rest ...))]
    [else code])
  code
  )

(define-syntax repeat$ (lambda (stx) (raise-syntax-error 'repeat$ "dont use this")))

(define (remove-repeats input)
  (debug 2 "Remove repeats from ~a\n" (syntax->datum input))
  (debug 2 "Properties ~a\n" (syntax-property-symbol-keys input))
  (define-literal-set locals (repeat$))
  (syntax-parse input #:literal-sets ([locals #:at input])
    [(out ... ((~literal repeat$) stuff ...) rest ...)
     (debug 2 " Found a repeat\n")
     (with-syntax ([(out* ...) (map remove-repeats (syntax->list #'(out ...)))]
                   [(rest* ...) (map remove-repeats (syntax->list #'(rest ...)))])
       (remove-repeats (datum->syntax input
                                      (syntax->list #'(out* ... stuff ... rest* ...))
                                      input input)))]
    [(normal ...) (with-syntax ([(normal* ...) (map remove-repeats (syntax->list #'(normal ...)))])
                    (datum->syntax input
                                   (syntax->list #'(normal* ...))
                                   input input))]
    [x #'x]
    [else (raise-syntax-error 'repeats "unhandled case" input)]))

(define-syntax (unexpand-honu-syntax stx)
  (define (remove-repeats input)
    (debug 2 "Remove repeats from ~a\n" (syntax->datum input))
    (debug 2 "Properties ~a\n" (syntax-property-symbol-keys input))
    (define-literal-set locals (repeat$))
    (syntax-parse input #:literal-sets (locals)
      [(out ... (repeat$ stuff ...) rest ...)
       (debug 2 " Found a repeat\n")
       (with-syntax ([(out* ...) (map remove-repeats (syntax->list #'(out ...)))]
                     [(rest* ...) (map remove-repeats (syntax->list #'(rest ...)))])
         (remove-repeats (datum->syntax input
                                        (syntax->list #'(out* ... stuff ... rest* ...))
                                        input input)))]
      [(normal ...) (with-syntax ([(normal* ...) (map remove-repeats (syntax->list #'(normal ...)))])
                      (datum->syntax input
                                     (syntax->list #'(normal* ...))
                                     input input))]
      [x #'x]
      [else (raise-syntax-error 'repeats "unhandled case" input)]))

  (syntax-case stx ()
    [(_ expr)
     (begin
       (debug "Expand honu syntax at phase ~a\n" (syntax-local-phase-level))
       #;
       (debug " Is ~a expanded ~a\n" (syntax->datum #'expr) (syntax->datum #'#'expr))
       (emit-remark (format "Unexpand honu syntax at phase ~a" (syntax-local-phase-level))
                    #'expr)
       #;
       (syntax-case #'expr ()
         [(_ what) (debug "Properties on ~a are ~a\n" #'what (syntax-property-symbol-keys #'what))])
       (define removed (remove-repeats #'expr))
       (emit-local-step #'expr removed #:id #'unexpand-honu-syntax)
       (debug "Cleansed ~a\n" (syntax->datum removed))
       (debug "Syntax properties ~a\n" (syntax-property-symbol-keys removed))
       removed)]))

; (define parsed-property (gensym 'honu-parsed))
(define parsed-property 'honu-parsed)

(define (parsed-syntax syntax)
  (debug "Add parsed syntax property to ~a\n" syntax)
  (if syntax
    (syntax-property syntax parsed-property #t)
    syntax))

(define (parsed-syntax? syntax)
  (syntax-property syntax parsed-property))

(define-syntax (racket-syntax stx)
  (syntax-case stx ()
    [(_ form)
     #'(parsed-syntax #'form)]))

(begin-for-syntax
  (provide compress-dollars)
  (define (compress-dollars stx)
    (define-literal-set local-literals (honu-$ repeat$))
    (syntax-parse stx #:literal-sets (local-literals)
      [(honu-$ x ... honu-$ rest ...)
       (with-syntax ([(rest* ...) (compress-dollars #'(rest ...))])
         (datum->syntax stx (syntax->list #'((repeat$ x ...) rest* ...))
                        stx stx))]
      [(x rest ...)
       (with-syntax ([x* (compress-dollars #'x)]
                     [(rest* ...) (compress-dollars #'(rest ...))])
         (datum->syntax stx
                        (syntax->list #'(x* rest* ...))
                        stx stx))]
      [x #'x])))

