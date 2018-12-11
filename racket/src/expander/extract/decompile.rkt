#lang racket/base
(require '#%linklet
         racket/pretty
         "../run/status.rkt")

(provide compile-and-decompile)

(define (compile-and-decompile linklet-expr print-extracted-to #:as-bytecode? as-bytecode?)
  (log-status "Compiling and decompiling linklet to ~a" print-extracted-to)

  (define linklet (compile-linklet linklet-expr))

  (define out (open-output-bytes))
  (write (hash->linklet-bundle (hasheq 0 linklet)) out)
  
  (call-with-output-file*
   print-extracted-to
   #:exists 'truncate/replace
   (lambda (o)
     (if as-bytecode?
         (write-bytes (get-output-bytes out) o)
         (let* ([i (open-input-bytes (get-output-bytes out))]
                ;; Dynamically load decompiler, so that it's not otherwise a
                ;; dependency for running the expander-flattener
                [zo ((dynamic-require 'compiler/zo-parse 'zo-parse) i)]
                [decompiled-expr ((dynamic-require 'compiler/decompile 'decompile) zo)])
           (pretty-write decompiled-expr o))))))
