#lang typed/racket

(define: x : (HashTable String String) #hash())
(ann #hash() (HashTable String String))
