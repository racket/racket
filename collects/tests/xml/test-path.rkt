#lang racket/base
(require xml/path
         tests/eli-tester)

(test
 (se-path*/list '(p) '(html (body (p "Hey") (p "Bar")))) => (list "Hey" "Bar")
 (se-path* '(p) '(html (body (p "Hey")))) => "Hey"
 (se-path* '(p #:bar) '(html (body (p ([bar "Zog"]) "Hey")))) => "Zog")