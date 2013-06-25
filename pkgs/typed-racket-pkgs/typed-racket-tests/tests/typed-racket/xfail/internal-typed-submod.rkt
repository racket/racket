#lang typed/racket
 
(module zoo typed/racket
  (provide tiger)
  
  (: tiger String)
  (define tiger "Tony"))
 
(require 'zoo)           
 
tiger
