#lang scheme
(require "private/sig.ss")

(define-signature xml-syntax^
  ((contracted
    ; XXX these should both actually return syntax? that is also xexpr/c
    [syntax:read-xml (() (input-port?) . ->* . syntax?)]
    [syntax:read-xml/element (() (input-port?) . ->* . syntax?)])))

(define-signature xml^
  ((open xml-structs^)
   (open reader^)
   (open writer^)
   (open xexpr^)
   (open space^)
   (open xml-syntax^)))

(provide xml^
         xml-syntax^)
