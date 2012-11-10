#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     syntax/define
                     "transformer.rkt"))

#|
(define honu-scheme-syntax 'honu-scheme-syntax)

(define (raw-scheme? stx)
  (syntax-property stx honu-scheme-syntax))

(define (apply-scheme-syntax stx)
  (syntax-property stx honu-scheme-syntax #t))
|#

(provide define-honu-syntax)
(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))

;; Do any honu-specific expansion here
(require (for-syntax
           "template.rkt" ;; for compress-dollars at phase 1
           "compile.rkt"
           "literals.rkt"
           syntax/stx
           syntax/parse)
         "template.rkt") ;; for remove-repeats at phase 0
(define-honu-syntax honu-syntax
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      #;
      [(_ (#%parens single) . rest)
       (define context #'single)
       (define compressed (compress-dollars #'single))
       (values
         (with-syntax ([stuff* (datum->syntax context compressed context context)])
           (phase1:racket-syntax #'stuff*))
         #'rest
         #f)]
      [(_ (#%parens stuff ...) . rest)
       (define context (stx-car #'(stuff ...)))
       (define compressed (compress-dollars #'(stuff ...)))
       (values
         (with-syntax ([stuff* (datum->syntax context
                                              (syntax->list compressed)
                                              context context)])
           ;; (debug "Stuff is ~a\n" (syntax->datum #'stuff*))
           ;; (debug "Stuff syntaxed is ~a\n" (syntax->datum #'#'stuff*))

           ;; stuff* will be expanded when this syntax is returned because
           ;; the whole thing will be
           ;;   (remove-repeats #'((repeat$ 1) (repeat$ 2)))
           ;; so remove-repeats will be executed later
           (racket-syntax
             (remove-repeats #'stuff*))

           #;
           (with-syntax ([(out ...) #'stuff*])
             (phase1:racket-syntax #'stuff*)))
         #; #'(%racket-expression (parse-stuff stuff ...))
         #'rest
         #f)])))
