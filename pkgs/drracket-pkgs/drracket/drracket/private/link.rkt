#lang racket/base
(require racket/unit
         "modes.rkt"
         "font.rkt"
         "eval.rkt"
         "module-browser.rkt"
         "multi-file-search.rkt"
         "debug.rkt"
         "module-language.rkt"
         "tools.rkt"
         "tools-drs.rkt"
         "language.rkt"
         "language-configuration.rkt"
         drracket/private/drsig
         "init.rkt"
         "text.rkt"
         "app.rkt"
         "main.rkt"
         "rep.rkt"
         "frame.rkt"
         "unit.rkt"
         "tracing.rkt"
         "get-extend.rkt"
         "help-desk.rkt"
         "module-language-tools.rkt"
         "interface.rkt")

(provide drracket@ drscheme/drracket:tool^)

(define-compound-unit/infer drracket-unit@
  (import)
  (export drracket:debug^
          drracket:unit^
          drracket:rep^
          drracket:frame^
          drracket:get/extend^
          drracket:language-configuration^
          drracket:language^
          drracket:help-desk^
          drracket:eval^
          drracket:modes^
          drracket:tracing^
          drracket:module-language/int^
          drracket:module-language-tools^
          drracket:interface^)
  (link interface@ init@ tools@ tools-drs@ modes@ text@ eval@ frame@ rep@ language@
        module-overview@ module-language@ unit@ debug@ multi-file-search@ get-extend@
        language-configuration@ font@ module-language-tools@
        help-desk@ tracing@ app@
        main@))

(define-signature drscheme/drracket:tool^
  ((open drracket:tool^)
   (open drscheme:tool^)))

(define-unit/new-import-export drracket@
  (import) 
  (export drscheme/drracket:tool^)
  (((prefix drracket:debug: drracket:debug^)
    (prefix drracket:unit: drracket:unit^)
    (prefix drracket:rep: drracket:rep^)
    (prefix drracket:frame: drracket:frame^)
    (prefix drracket:get/extend: drracket:get/extend^)
    (prefix drracket:language-configuration: drracket:language-configuration^)
    (prefix drracket:language: drracket:language^)
    (prefix drracket:help-desk: drracket:help-desk^)
    (prefix drracket:eval: drracket:eval^)
    (prefix drracket:modes: drracket:modes^)
    (prefix drracket:tracing: drracket:tracing^)
    (prefix drracket:module-language: drracket:module-language/int^)
    (prefix drracket:module-language-tools: drracket:module-language-tools^)
    (prefix drracket: drracket:interface^)
    
    (prefix drscheme:debug: drracket:debug^)
    (prefix drscheme:unit: drracket:unit^)
    (prefix drscheme:rep: drracket:rep^)
    (prefix drscheme:frame: drracket:frame^)
    (prefix drscheme:get/extend: drracket:get/extend^)
    (prefix drscheme:language-configuration: drracket:language-configuration^)
    (prefix drscheme:language: drracket:language^)
    (prefix drscheme:help-desk: drracket:help-desk^)
    (prefix drscheme:eval: drracket:eval^)
    (prefix drscheme:modes: drracket:modes^)
    (prefix drscheme:tracing: drracket:tracing^)
    (prefix drscheme:module-language: drracket:module-language/int^)
    (prefix drscheme:module-language-tools: drracket:module-language-tools^)
    (prefix drscheme: drracket:interface^))
   drracket-unit@))
