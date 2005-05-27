
(module xml-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (require "private/sig.ss")

  (define-signature xml^
    ((open xml-structs^) (open reader^) (open writer^) (open xexpr^) (open space^)
     syntax:read-xml syntax:read-xml/element))

  (provide xml^))



