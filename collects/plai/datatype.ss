
(module datatype mzscheme
  (require "private/datatype.ss")

  (define-type-case type-case else)
  
  (provide type-case
	   define-type
	   provide-type))

