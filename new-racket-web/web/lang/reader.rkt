#lang s-exp syntax/module-reader
meta/web/common/main

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
      [else (default key defval)])))
