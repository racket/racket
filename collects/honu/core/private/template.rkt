#lang racket/base

(require syntax/parse
         "literals.rkt"
         "debug.rkt"
         (for-syntax racket/base
                     "debug.rkt"
                     syntax/parse
                     macro-debugger/emit
                     ))

(provide (all-defined-out))

(define-syntax repeat$ (lambda (stx) (raise-syntax-error 'repeat$ "dont use this")))

(define (remove-repeats input)
  (debug 2 "Remove repeats from ~a\n" (syntax->datum input))
  (debug 2 "Properties ~a\n" (syntax-property-symbol-keys input))
  (define-literal-set locals (repeat$))
  (syntax-parse input #:literal-sets ([locals #:at input])
    [(out ... ((~literal repeat$) stuff ...) rest ...)
     (debug 2 " Found a repeat\n")
     (with-syntax ([(out* ...) (map remove-repeats (syntax->list #'(out ...)))]
                   [(stuff* ...) (map remove-repeats (syntax->list #'(stuff ...)))]
                   [(rest* ...) (map remove-repeats (syntax->list #'(rest ...)))])
       (remove-repeats (datum->syntax input
                                      (syntax->list #'(out* ... stuff* ... rest* ...))
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

(define (compress-dollars stx)
  (define-literal-set local-literals (honu-$ repeat$))
  (define-splicing-syntax-class not-dollar
                                #:literal-sets (local-literals)
                                [pattern x #:when (or (not (identifier? #'x))
                                                      (not (free-identifier=? #'honu-$ #'x)))
                                         #:with out #'x])
  (debug 2 "Compress dollars ~a\n" stx)
  (syntax-parse stx #:literal-sets (local-literals)
    [(honu-$ x:not-dollar ... honu-$ rest ...)
     (debug 2 "Compressing ~a\n" #'(x.out ...))
     (with-syntax ([(rest* ...) (compress-dollars #'(rest ...))]
                   [(x.out* ...) (compress-dollars #'(x.out ...))])
       (datum->syntax stx (syntax->list #'((repeat$ x.out* ...) rest* ...))
                      stx stx))]
    [(honu-$ rest ...)
     (error 'compress-dollars "unmatched $ ~a" (syntax->datum stx))]
    [(x rest ...)
     (with-syntax ([x* (compress-dollars #'x)]
                   [(rest* ...) (compress-dollars #'(rest ...))])
       (datum->syntax stx
                      (syntax->list #'(x* rest* ...))
                      stx stx))]
    [x #'x]))
