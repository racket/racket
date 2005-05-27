;;;
;;; <string.ss> ---- SRFI 13 port to PLT Scheme
;;; Time-stamp: <03/04/25 18:59:09 solsona>

;;; Copyright (C) Dr. Mirko Luedde (2002). All Rights Reserved.

(module rec mzscheme
  (provide rec)

  (define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))
  )