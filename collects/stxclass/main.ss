
#lang scheme/base
(require "private/sc.ss"
         "private/lib.ss")

(provide define-syntax-class
         pattern

         ~and
         ~or
         ...*

         syntax-parse
         syntax-parser
         with-patterns
         attribute

         this-syntax

         current-expression
         current-macro-name

         (all-from-out "private/lib.ss")

         (rename-out [parse-sc syntax-class-parse]
                     [attrs-of syntax-class-attributes]))
