#lang racket/load


(module b typed/racket/base
  (provide (all-defined-out))    
  (define-struct: string-type () #:transparent))

(module c typed/racket/base
  (provide (all-defined-out))    
  (define-struct: string-type () #:transparent))

(module a typed/racket/base
  

  (require 
   (prefix-in one: 'b)
   (prefix-in two: 'c)
   )  
  
  
  (provide foo)
  
  (: foo two:string-type)
  (define foo (two:string-type)))

(require 'a)
foo
