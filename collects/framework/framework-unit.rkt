#lang scheme/base

(require mzlib/unit
         mred/mred-sig)

(require "private/sig.ss"
         "private/number-snip.ss"
         "private/comment-box.ss"
         "private/application.ss"
         "private/version.ss"
         "private/color-model.ss"
         "private/exit.ss"
         "private/menu.ss"
         "private/preferences.ss"
         "private/autosave.ss"
         "private/color.ss"
         "private/color-prefs.ss"
         "private/handler.ss" 
         "private/keymap.ss"
         "private/path-utils.ss"
         "private/icon.ss"
         "private/editor.ss"
         "private/pasteboard.ss"
         "private/text.ss"
         "private/finder.ss"
         "private/group.ss"
         "private/canvas.ss"
         "private/panel.ss"
         "private/frame.ss"
         "private/scheme.ss"
         "private/main.ss"
         "private/mode.ss")

(provide framework-separate@ framework@)

(define-compound-unit/infer framework-separate@
  (import mred^)
  (export framework:application^ 
          framework:version^
          framework:color-model^
          framework:mode^
          framework:exit^
          framework:menu^
          framework:preferences^
          framework:number-snip^
          framework:autosave^
          framework:path-utils^
          framework:icon^
          framework:keymap^
          framework:editor^
          framework:pasteboard^
          framework:text^
          framework:color^
          framework:color-prefs^
          framework:comment-box^
          framework:finder^
          framework:group^ 
          framework:canvas^
          framework:panel^
          framework:frame^ 
          framework:handler^
          framework:scheme^ 
          framework:main^)
  (link
   application@ version@ color-model@ mode@ exit@ menu@
   preferences@ number-snip@ autosave@ path-utils@ icon@ keymap@
   editor@ pasteboard@ text@ color@ color-prefs@ comment-box@ 
   finder@ group@ canvas@ panel@ frame@ handler@ scheme@ main@))

(define-unit/new-import-export framework@ (import mred^) (export framework^)
  (((prefix application: framework:application^)
    (prefix version: framework:version^)
    (prefix color-model: framework:color-model^)
    (prefix mode: framework:mode^)
    (prefix exit: framework:exit^)
    (prefix menu: framework:menu^)
    (prefix preferences: framework:preferences^)
    (prefix number-snip: framework:number-snip^)
    (prefix autosave: framework:autosave^)
    (prefix path-utils: framework:path-utils^)
    (prefix icon: framework:icon^)
    (prefix keymap: framework:keymap^)
    (prefix editor: framework:editor^)
    (prefix pasteboard: framework:pasteboard^)
    (prefix text: framework:text^)
    (prefix color: framework:color^)
    (prefix color-prefs: framework:color-prefs^)
    (prefix comment-box: framework:comment-box^)
    (prefix finder: framework:finder^)
    (prefix group: framework:group^)
    (prefix canvas: framework:canvas^)
    (prefix panel: framework:panel^)
    (prefix frame: framework:frame^)
    (prefix handler: framework:handler^)
    (prefix scheme: framework:scheme^)
    (prefix main: framework:main^))
   framework-separate@ mred^))
