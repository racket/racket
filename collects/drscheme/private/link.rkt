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

(provide drscheme@)

(define-compound-unit/infer drscheme-unit@
  (import)
  (export drscheme:debug^
          drscheme:unit^
          drscheme:rep^
          drscheme:frame^
          drscheme:get/extend^
          drscheme:language-configuration^
          drscheme:language^
          drscheme:help-desk^
          drscheme:eval^
          drscheme:modes^
          drscheme:tracing^
          drscheme:module-language^
          drscheme:module-language-tools^)
  (link init@ tools@ modes@ text@ eval@ frame@ rep@ language@
        module-overview@ unit@ debug@ multi-file-search@ get-extend@
        language-configuration@ font@ module-language@ module-language-tools@
        help-desk@ tracing@ app@
        main@))

(define-unit/new-import-export drscheme@
  (import) (export drscheme:tool^)
  (((prefix drscheme:debug: drscheme:debug^)
    (prefix drscheme:unit: drscheme:unit^)
    (prefix drscheme:rep: drscheme:rep^)
    (prefix drscheme:frame: drscheme:frame^)
    (prefix drscheme:get/extend: drscheme:get/extend^)
    (prefix drscheme:language-configuration: drscheme:language-configuration^)
    (prefix drscheme:language: drscheme:language^)
    (prefix drscheme:help-desk: drscheme:help-desk^)
    (prefix drscheme:eval: drscheme:eval^)
    (prefix drscheme:modes: drscheme:modes^)
    (prefix drscheme:tracing: drscheme:tracing^)
    (prefix drscheme:module-language: drscheme:module-language^)
    (prefix drscheme:module-language-tools: drscheme:module-language-tools^))
   drscheme-unit@))
