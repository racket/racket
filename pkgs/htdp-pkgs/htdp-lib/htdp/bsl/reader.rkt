#lang scheme/base
(require (rename-in syntax/module-reader
                    [#%module-begin #%reader-module-begin]))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base)
                     #%module-begin))

(define-syntax-rule (module-begin lang opts)
  (#%reader-module-begin
   lang

   #:read (wrap-reader read options)
   #:read-syntax (wrap-reader read-syntax options)
   #:info (make-info options)
   #:language-info (make-language-info options)

   (provide options)
   (define options opts)))

(define (wrap-reader read-proc options)
  (lambda args
    (parameterize ([read-decimal-as-inexact #f]
                   [read-accept-dot #f]
                   [read-accept-quasiquote (memq 'read-accept-quasiquote options)])
      (apply read-proc args))))

(define ((make-info options) key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (list (dynamic-require 'stepper/drracket-button 'stepper-drracket-button)
           (dynamic-require 'drracket/syncheck-drracket-button 'syncheck-drracket-button))]

    [(drscheme:opt-out-toolbar-buttons)
     ;; opt-out of all of the extra buttons b/c 
     ;; we don't want anything to confuse in the teaching languages.
     #f]
    
    [(drracket:show-big-defs/ints-labels) #t]
    
    [else (use-default key default)]))

(define (make-language-info options)
  `#(htdp/bsl/language-info get-info ,options))
