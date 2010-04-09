#lang scheme/base
(require scheme/unit
         "modes.ss"
         "font.ss"
         "eval.ss"
         "module-browser.ss"
         "multi-file-search.ss"
         "debug.ss"
         "module-language.ss"
         "tools.ss"
         "language.ss"
         "language-configuration.ss"
         "drsig.ss"
         "init.ss"
         "text.ss"
         "app.ss"
         "main.ss"
         "rep.ss"
         "frame.ss"
         "unit.ss"
         "tracing.ss"
         "get-extend.ss"
         "help-desk.ss"
         "module-language-tools.ss")

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
          drscheme:module-language^)
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
    (prefix drscheme:module-language: drscheme:module-language^))
   drscheme-unit@))
