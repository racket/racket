#lang racket/base
(require scheme/unit
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
         "drsig.rkt"
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
         "module-language-tools.rkt")

(provide drracket@)

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
          drracket:module-language^
          drracket:module-language-tools^)
  (link init@ tools@ tools-drs@ modes@ text@ eval@ frame@ rep@ language@
        module-overview@ unit@ debug@ multi-file-search@ get-extend@
        language-configuration@ font@ module-language@ module-language-tools@
        help-desk@ tracing@ app@
        main@))

(define-unit/new-import-export drracket@
  (import) (export drracket:tool^)
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
    (prefix drracket:module-language: drracket:module-language^)
    (prefix drracket:module-language-tools: drracket:module-language-tools^))
   drracket-unit@))
