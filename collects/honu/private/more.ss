#lang scheme

(require "honu-typed-scheme.ss"
         "literals.ss"
         (for-syntax syntax/parse
                     syntax/stx
                     "literals.ss")
         (for-template "honu-typed-scheme.ss"
                       "literals.ss"
                       ))

(provide (all-defined-out))

(define-honu-syntax honu-syntax
  (lambda (stx ctx)
    (syntax-parse stx #:literals (semicolon #%parens)
      [(_ (#%parens expr ...) semicolon . rest)
       (values
         (lambda ()
           (define (show-pattern-variables what)
             (cond
               [(syntax-pattern-variable? what) (printf "~a is a pattern variable\n") what]
               [(stx-pair? what) (for-each show-pattern-variables (syntax->list what))]
               [else (printf "~a is *not* a pattern variable\n" what)]))

           (printf "Original code is ~a\n" (syntax->datum #'(expr ...)))
           (printf "Expanded is ~a\n" (syntax->datum (expand-syntax-once #'(expr ...))))
           (for-each show-pattern-variables (syntax->list #'(expr ...)))
           ;; outer is relative phase 1, inner is relative phase 0
           #'#'(honu-unparsed-begin expr ...)
           #;
           (with-syntax ([(out ...) (local-expand #'(expr ...) 'expression '())])
             #'(honu-unparsed-begin out ...)))
         #'rest)])))

