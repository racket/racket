#lang racket/base

(require (except-in scribble/html/lang #%module-begin)
         "layout.rkt" "extras.rkt" "links.rkt" "utils.rkt"
         "indexes.rkt")
(provide (all-from-out scribble/html/lang
                       "layout.rkt" "extras.rkt" "links.rkt"
                       "indexes.rkt")
         basename web-path url-of ; from "utils.rkt"
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin form ...)
  (#%module-begin
   form ...
   (module main racket/base
     (require plt-web/build))))

(module reader syntax/module-reader
  plt-web/main

  ;; Similar to `#lang scribble/html', but with a plain scribble reader
  ;; (not the inside one).

  #:read        scribble:read
  #:read-syntax scribble:read-syntax
  #:info        (web-reader-info)

  (require (prefix-in scribble: scribble/reader))

  (define (web-reader-info)
    (lambda (key defval default)
      (case key
        [(color-lexer)
         (dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
        [else (default key defval)]))))
