#lang racket/base

(provide #%module-begin provide require rename-in rename-out prefix-in only-in all-from-out except-out except-in
         providing begin)

(require (for-syntax racket/base))

(define-for-syntax ts-mod 'typed-racket/typed-racket)

(define-syntax (providing stx)
  (syntax-case stx (libs from basics except)
    [(form (libs (except lb ex ...) ...) (basics b ...) (from spec id ...) ...)
     (datum->syntax
      stx
      (syntax->datum
       (with-syntax ([(b* ...) (generate-temporaries #'(b ...))]
                     [ts ts-mod])
         (syntax/loc
             stx
           (begin
             (require (except-in ts b ...))
             (require (only-in ts [b b*] ...))
             (require (except-in lb ex ...) ...)
             (require (only-in spec id ...) ...)
             (provide id ...) ...
             (provide (rename-out [b* b] ...))
             (provide (except-out (all-from-out ts) b* ...))
             (provide (all-from-out lb) ...))))))]))
