#lang racket/base
(require syntax/parse)

(syntax-parse (read-syntax 'str (open-input-string "five"))
  [_:id 5])
