#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax parse-system-type-case))

(define-for-syntax (parse-system-type-case stx
                                           parse-one
                                           make-one make-combined)
  (syntax-parse stx
    [(_
      (~and key (~or (~datum os) (~datum os*) (~datum arch) (~datum word)))
      [(val ...) rhs]
      ...
      [(~datum else) else-rhs])

     (for ([val-stx (in-list (syntax->list #'(val ... ...)))])
       (if (eq? (syntax-e #'key) 'word)
           (unless (memv (syntax-e val-stx) '(32 64))
             (raise-syntax-error #f "expected 32 or 64" stx val-stx))
           (unless (symbol? (syntax-e val-stx))
             (raise-syntax-error #f "expected an identifier" stx val-stx))))

     (define rhs-stxs (append (attribute rhs) (list #'else-rhs)))
     (define rhs-ts (for/list ([rhs-stx (in-list rhs-stxs)])
                      (parse-one stx rhs-stx)))
     
     (let loop ([rhs-ts rhs-ts] [valss (syntax->list #'((val ...) ...))])
       (cond
         [(null? valss) (make-one (car rhs-ts))]
         [else
          (define combined (loop (cdr rhs-ts) (cdr valss)))
          (make-combined (syntax-e #'key)
                         (car valss)
                         (make-one (car rhs-ts))
                         combined)]))]))
