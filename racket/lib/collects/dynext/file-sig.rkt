#lang racket/base
(require racket/unit)

(provide dynext:file^)

(define-signature dynext:file^
  (append-zo-suffix
   append-c-suffix
   append-constant-pool-suffix
   append-object-suffix
   append-extension-suffix
   
   extract-base-filename/ss
   extract-base-filename/c
   extract-base-filename/kp
   extract-base-filename/o
   extract-base-filename/ext))