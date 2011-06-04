#lang racket/unit

#|

This file sets up the right lexical environment to invoke the tools that want to use the drscheme: names.

|#

(require racket/class
         racket/list
         racket/runtime-path
         racket/contract
         setup/getinfo
         mred
         framework
         framework/splash
         "drsig.rkt"
         "language-object-contract.rkt"
         mrlib/switchable-button
         string-constants)

(require (for-syntax racket/base racket/match
                     compiler/cm-accomplice
                     syntax/modread))

(import [prefix drscheme:frame: drracket:frame^]
        [prefix drscheme:unit: drracket:unit^]
        [prefix drscheme:rep: drracket:rep^]
        [prefix drscheme:get/extend: drracket:get/extend^]
        [prefix drscheme:language: drracket:language^]
        [prefix drscheme:language-configuration: drracket:language-configuration^]
        [prefix drscheme:help-desk: drracket:help-desk^]
        [prefix drscheme:init: drracket:init^]
        [prefix drscheme:debug: drracket:debug^]
        [prefix drscheme:eval: drracket:eval^]
        [prefix drscheme:modes: drracket:modes^]
        [prefix drscheme:tracing: drracket:tracing^]
        [prefix drscheme:module-language: drracket:module-language^]
        [prefix drscheme:module-language-tools: drracket:module-language-tools^]
        [prefix drscheme: drracket:interface^])
(export drracket:tools-drs^)

(define-syntax (wrap-tool-inputs stx)
  (syntax-case stx ()
    [(_ body tool-name)
     (let ()
       (define tool-lib-src (collection-file-path "tool-lib.rkt" "drracket"))
       (define full-sexp
         (call-with-input-file tool-lib-src
           (λ (port)
              (with-module-reading-parameterization
               (lambda ()
                 (read port))))))
       
       (register-external-file tool-lib-src)

       (let loop ([sexp full-sexp])
         (match sexp
           [`((#%module-begin ,body ...))
            (loop body)]
           [`((provide/dr/doc (,x ,name ,ctc ,other ...) ...) ,rest ...)
            #`(let #,(map (λ (orig-name ctc) 
                            (define (rewrite obj)
                              (cond
                                [(symbol? obj)
                                 (string->symbol (regexp-replace #rx"^drracket:" (symbol->string obj) "drscheme:"))]
                                [(pair? obj)
                                 (cons (rewrite (car obj))
                                       (rewrite (cdr obj)))]
                                [else obj]))
                            (with-syntax ([name (datum->syntax #'tool-name (rewrite orig-name))]
                                          [ctc (datum->syntax #'tool-name (rewrite ctc))])
                              #`[name
                                 (contract (let ([name ctc]) name)  ;; need to replace the names in 'ctc'
                                           name 
                                           'drracket 
                                           tool-name
                                           (quote name)
                                           (quote-syntax name))]))
                          name
                          ctc)
                body)]
           [`(,a . ,b) 
            (loop b)]
           [`()
            (error 'tools-drs.rkt "did not find provide/dr/doc: ~a" full-sexp)])))]))

;; these two definitions are a hack. They give bindings for the drracket: based names that
;; appear in the source of language-object-contract.rkt.
(define (drracket:language:capability-registered? . args) (apply drscheme:language:capability-registered? args))
(define (drracket:language:get-capability-contract . args) (apply drscheme:language:get-capability-contract args))

;; invoke-drs-tool : unit/sig string -> (values (-> void) (-> void))
;; invokes the tools and returns the two phase thunks.
;; this is the same as the invoke-tool function in tools.rkt, but 
;; supplies names prefixed with `drscheme:'
(define (invoke-drs-tool unit tool-name)
  (define-unit-binding unit@ unit (import drscheme:tool^) (export drracket:tool-exports^))
  (language-object-abstraction drscheme:language:object/c #f)
  (wrap-tool-inputs 
   (let ()
     (define-values/invoke-unit unit@
       (import drscheme:tool^) (export drracket:tool-exports^))
     (values phase1 phase2))
   tool-name))

