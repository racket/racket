
#lang scheme/base
(require "private/sc.ss"
         "private/lib.ss")

(provide define-syntax-class
         define-basic-syntax-class
         define-basic-syntax-class*
         pattern
         basic-syntax-class

         syntax-parse
         syntax-parser
         with-patterns
         ...*

         current-expression
         current-macro-name

         (all-from-out "private/lib.ss")

         (rename-out [parse-sc syntax-class-parse]
                     [attrs-of syntax-class-attributes]))
