#lang racket/base
(require syntax/location)
(require tests/eli-tester)

(define-syntax-rule (module-source)                                                                           
 (variable-reference->module-source                                                                           
  (#%variable-reference)))   

(test 
 (quote-module-name) => (module-source)                
 (quote-module-name "..") => (list (module-source) "..")    
 (quote-module-name ".." A) => (list (module-source) ".." 'A) 
 (quote-module-name A B) => (list (module-source) 'A 'B))

(module A racket
  (require syntax/location)
  (define-syntax-rule (name) (quote-module-name))
  (define a-name (name))
  (define a-path (quote-module-path))
  (module+ C
    (require syntax/location)
    (define c-name (quote-module-name))
    (define c-name2 (quote-module-name ".."))
    (define c-path (quote-module-path))
    (define c-path2 (quote-module-path ".."))
    (provide c-name c-name2 c-path c-path2))
  (provide (all-defined-out)))
(module B racket
  (require syntax/location)
  (require (submod ".." A))
  (define b-name (name))
  (define b-path (quote-module-path))
  (provide (all-defined-out)))
(require 'B)

(require 'A)
(require (submod 'A C))
(test
  a-name => (list (module-source) 'A)
  c-name => (list (module-source) 'A 'C)
  c-name2 => (list (module-source) 'A 'C "..")
  b-name =>  (list (module-source) 'B))

(test 
 (quote-module-path) => (module-source)
 (quote-module-path "..") => (list 'submod (module-source) "..")
 (quote-module-path ".." A) => (list 'submod (module-source) ".." 'A) 
 (quote-module-path A B) => (list 'submod (module-source) 'A 'B))

(test
  a-path => (list 'submod (module-source) 'A)
  c-path => (list 'submod (module-source) 'A 'C)
  c-path2 => (list 'submod (module-source) 'A 'C "..")
  b-path =>  (list 'submod (module-source) 'B))
