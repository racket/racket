#lang racket/base
(require racket/list)
(provide adjustable-chars
         up-chars
         dn-chars
         lt-chars
         rt-chars)

(define up-chars
  '(#\╬ 
    #\╩ #\╣ #\╠
    #\╝ #\╚
    #\║
    #\+ #\|))

(define dn-chars
  '(#\╬ 
    #\╦ #\╣ #\╠
    #\╗ #\╔
    #\║
    #\+ #\|))

(define lt-chars
  '(#\╬ 
    #\╩ #\╦ #\╣
    #\╝ #\╗ 
    #\═ 
    #\+ #\-))

(define rt-chars
  '(#\╬ 
    #\╩ #\╦ #\╠
    #\╔ #\╚
    #\═
    #\+ #\-))

(define adjustable-chars
  (remove-duplicates
   (append up-chars dn-chars lt-chars rt-chars)))