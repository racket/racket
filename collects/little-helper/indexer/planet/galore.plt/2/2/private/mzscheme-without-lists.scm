;;; mzscheme-without-lists   --  Jens Axel Søgaard

; This language contains all mzscheme functions used in 
; skew-binary-random-access-lists. Due to naming issues,
; we neeed to avoid the names car, cdr, list? etc.

(module mzscheme-without-lists mzscheme
  (provide 
   #%app
   #%datum
   #%module-begin
   #%top
   = + - < <=
   and
   cddr
   cond
   define
   define-struct
   error
   if 
   let
   let*
   not
   null?
   or
   pair?
   provide
   quote
   quotient
   require
   sub1
   ))