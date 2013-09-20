#lang racket/unit

#|

This file sets up the right lexical environment to
invoke the tools that want to use the drscheme: names.

|#

(require racket/class
         racket/list
         racket/runtime-path
         racket/contract
         racket/struct-info
         setup/getinfo
         mred
         framework
         framework/splash
         drracket/private/drsig
         "language-object-contract.rkt"
         "wrap-tool-inputs.rkt"
         mrlib/switchable-button
         string-constants
         net/url)

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



;; these two definitions are a hack. They give bindings for the drracket: based names that
;; appear in the source of language-object-contract.rkt.
(define (drracket:language:capability-registered? . args)
  (apply drscheme:language:capability-registered? args))
(define (drracket:language:get-capability-contract . args)
  (apply drscheme:language:get-capability-contract args))

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
   tool-name
   #t))

