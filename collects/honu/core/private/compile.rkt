#lang racket/base

(require syntax/parse
         (for-syntax racket/base "debug.rkt" syntax/parse)
         "literals.rkt")

;; to get syntax as a literal
(require (for-template racket/base))

(provide (all-defined-out))

(define (honu->racket forms)
  (define-literal-set literals (%racket))
  (syntax-parse forms #:literal-sets (literals)
    #:literals ([literal-syntax syntax])
    [(%racket x) (honu->racket #'x)]
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
  (syntax-parse code
    [(x:stopper rest ...) (strip-stops #'(rest ...))]
    [else code]))

(define-syntax repeat$ (lambda (stx) (raise-syntax-error 'repeat$ "dont use this")))

(define-syntax (unexpand-honu-syntax stx)
  (define (remove-repeats input)
    (debug "Remove repeats from ~a\n" (syntax->datum input))
    (define-literal-set locals (repeat$))
    (syntax-parse input #:literal-sets (locals)
      [(out ... (repeat$ stuff ...) rest ...)
       (debug " Found a repeat\n")
       (with-syntax ([(out* ...) (map remove-repeats (syntax->list #'(out ...)))]
                     [(rest* ...) (map remove-repeats (syntax->list #'(rest ...)))])
         (remove-repeats #'(out* ... stuff ... rest* ...)))]
      [(normal ...) (with-syntax ([(normal* ...) (map remove-repeats (syntax->list #'(normal ...)))])
                      (datum->syntax input
                                     #'(normal* ...)
                                     input input))]
      [x #'x]
      [else (raise-syntax-error 'repeats "unhandled case" input)]))

  (syntax-case stx ()
    [(_ expr)
     (begin
       (debug "Expand honu syntax at phase ~a\n" (syntax-local-phase-level))
       (debug " Is ~a expanded ~a\n" (syntax->datum #'expr) (syntax->datum #'#'expr))
       (define removed (remove-repeats #'expr))
       (debug "Cleansed ~a\n" (syntax->datum removed))
       removed)]))
