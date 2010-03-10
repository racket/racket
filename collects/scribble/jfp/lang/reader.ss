#lang s-exp syntax/module-reader

scribble/jfp/lang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (cons 'doc (t)))
#:info (lambda (key defval default)
         (case key
           [(color-lexer)
            (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
           [else (default key defval)]))

(require (prefix-in scribble: "../../reader.ss"))
