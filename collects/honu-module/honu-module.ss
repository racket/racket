(module honu-module mzscheme
  (define-syntax m
    (syntax-rules ()
      [(_ require provide)
       (begin
	 (require "dynamic.ss")
	 (provide (all-from "dynamic.ss")))]))
  (m require provide))
    


