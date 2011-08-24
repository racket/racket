#lang scheme/base
(require (prefix-in scribble: "../reader.rkt")
         (rename-in syntax/module-reader
                    [#%module-begin #%reader-module-begin]))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base)
                     #%module-begin)
         scribble-base-info
         scribble-base-reader-info
         scribble-base-language-info)

(define-syntax-rule (module-begin lang #:wrapper1 wrapper1)
  (#%reader-module-begin
   lang

   #:read          scribble:read-inside
   #:read-syntax   scribble:read-syntax-inside
   #:whole-body-readers? #t
   #:wrapper1      wrapper1
   #:info          (scribble-base-info)
   #:language-info (scribble-base-language-info)))

;; Settings that apply just to the surface syntax:
(define (scribble-base-reader-info)
  (lambda (key defval default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
      [else (default key defval)])))

;; Settings that apply to Scribble-renderable docs:
(define (scribble-base-info)
  (lambda (key defval default)
    (case key
      [(drracket:toolbar-buttons)
       (dynamic-require 'scribble/tools/drracket-buttons 'drracket-buttons)]
      [else ((scribble-base-reader-info) key defval default)])))

(define (scribble-base-language-info)
  '#(racket/language-info get-info #f))
