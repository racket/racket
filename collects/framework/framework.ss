#lang scheme/base

(require mzlib/unit
         (lib "mred-unit.ss" "mred")
         (lib "mred-sig.ss" "mred")
         mred
         scheme/class

         "private/framework-exports.ss"
         "preferences.ss"
         "test.ss"
         "gui-utils.ss"
         "decorated-editor-snip.ss"
         
         "framework-unit.ss"
         "private/sig.ss"
         
         scheme/contract
         
         (for-syntax scheme/base))

(provide-signature-elements
 (prefix application: framework:application-class^)
 (prefix version: framework:version-class^)
 (prefix color-model: framework:color-model-class^)
 (prefix mode: framework:mode-class^)
 (prefix exit: framework:exit-class^)
 (prefix menu: framework:menu-class^)
 (prefix preferences: framework:preferences-class^)
 (prefix number-snip: framework:number-snip-class^)
 (prefix autosave: framework:autosave-class^)
 (prefix path-utils: framework:path-utils-class^)
 (prefix icon: framework:icon-class^)
 (prefix keymap: framework:keymap-class^)
 (prefix editor: framework:editor-class^)
 (prefix pasteboard: framework:pasteboard-class^)
 (prefix text: framework:text-class^)
 (prefix color: framework:color-class^)
 (prefix color-prefs: framework:color-prefs-class^)
 (prefix comment-box: framework:comment-box-class^)
 (prefix finder: framework:finder-class^)
 (prefix group: framework:group-class^)
 (prefix canvas: framework:canvas-class^)
 (prefix panel: framework:panel-class^)
 (prefix frame: framework:frame-class^)
 (prefix handler: framework:handler-class^)
 (prefix scheme: framework:scheme-class^)
 (prefix main: framework:main-class^))

(provide (all-from-out "test.ss")
         (all-from-out "gui-utils.ss")
         (all-from-out "preferences.ss")
         (all-from-out "decorated-editor-snip.ss"))

(define-syntax (provide/contract/docs stx)
  (syntax-case stx ()
    [(me (name contract docs ...) ...)
     (let ([args (syntax->datum #'((name contract) ...))])
       #`(provide/contract #,@(datum->syntax #'me args)))]))

(define-compound-unit/infer framework+mred@
  (import)
  (export framework^)
  (link standard-mred@ framework@))


(define-values/invoke-unit/infer framework+mred@)

(framework-exports provide/contract/docs)
