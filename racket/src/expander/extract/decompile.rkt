#lang racket/base
(require '#%linklet
         racket/pretty
         "../run/status.rkt")

(provide compile-and-decompile)

(define (compile-and-decompile linklet-expr print-extracted-to)
  (unless compile-linklet
    (error "Host Racket does not support linklet compilation"))
  
  (log-status "Compiling and decompiling linklet to ~a" print-extracted-to)
      
  (define linklet (compile-linklet linklet-expr))

  (define o (open-output-bytes))
  (write (hash->linklet-bundle (hasheq 0 linklet)) o)

  (define i (open-input-bytes (get-output-bytes o)))

  ;; Dynamically load decompiler, so that it's not otherwise a
  ;; dependency for running the expander-flattener
  (define zo ((dynamic-require 'compiler/zo-parse 'zo-parse) i))
  (define decompiled-expr ((dynamic-require 'compiler/decompile 'decompile) zo))
  
  (call-with-output-file*
   print-extracted-to
   #:exists 'truncate/replace
   (lambda (o)
     (pretty-write decompiled-expr o))))

(define compile-linklet
  (hash-ref (primitive-table '#%linklet) 'compile-linklet #f))

(define hash->linklet-bundle
  (hash-ref (primitive-table '#%linklet) 'hash->linklet-bundle #f))
