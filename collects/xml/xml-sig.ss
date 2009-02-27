#lang scheme
(require "private/sig.ss")

(define-signature xml^
  ((open xml-structs^)
   (open reader^)
   (open writer^)
   (open xexpr^)
   (open space^)
   (open xml-syntax^)))

(provide xml^)
