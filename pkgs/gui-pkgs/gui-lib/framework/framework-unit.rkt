#lang racket/base

(require racket/unit
         mred/mred-sig)

(require "private/sig.rkt"
         "private/number-snip.rkt"
         "private/comment-box.rkt"
         "private/application.rkt"
         "private/version.rkt"
         "private/color-model.rkt"
         "private/exit.rkt"
         "private/menu.rkt"
         "private/preferences.rkt"
         "private/autosave.rkt"
         "private/color.rkt"
         "private/color-prefs.rkt"
         "private/handler.rkt" 
         "private/keymap.rkt"
         "private/path-utils.rkt"
         "private/icon.rkt"
         "private/editor.rkt"
         "private/pasteboard.rkt"
         "private/text.rkt"
         "private/finder.rkt"
         "private/group.rkt"
         "private/canvas.rkt"
         "private/panel.rkt"
         "private/frame.rkt"
         "private/racket.rkt"
         "private/main.rkt"
         "private/mode.rkt"
         "private/early-init.rkt")

(provide framework@)

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
          framework:racket^ 
          framework:main^)
  (link
   preferences@ early-init@
   application@ version@ color-model@ mode@ exit@ menu@
   number-snip@ autosave@ path-utils@ icon@ keymap@
   editor@ pasteboard@ text@ color@ color-prefs@ comment-box@ 
   finder@ group@ canvas@ panel@ frame@ handler@ racket@ main@))

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
    (prefix racket: framework:racket^)
    (prefix main: framework:main^))
   framework-separate@ mred^))
