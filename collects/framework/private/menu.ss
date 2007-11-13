#lang scheme/unit
  (require (lib "class.ss")
           "sig.ss"
           "../preferences.ss"
           (lib "mred-sig.ss" "mred"))
  
  (import mred^)
  (export framework:menu^)
  
  (define can-restore<%>
    (interface (selectable-menu-item<%>)
      restore-keybinding))
  
  (define can-restore-mixin
    (mixin (selectable-menu-item<%>) (can-restore<%>)
      (inherit set-shortcut get-shortcut)
      (define saved-shortcut 'not-yet)
      (define/public (restore-keybinding)
        (unless (eq? saved-shortcut 'not-yet)
          (set-shortcut saved-shortcut)))
      
      (super-new)
      (set! saved-shortcut (get-shortcut))
      (unless (preferences:get 'framework:menu-bindings)
        (set-shortcut #f))))
  
  (define can-restore-underscore<%>
    (interface (labelled-menu-item<%>)
      erase-underscores
      restore-underscores))
  
  (define can-restore-underscore-mixin
    (mixin (labelled-menu-item<%>) (can-restore-underscore<%>)
      (inherit get-label get-plain-label set-label)
      (define/public (erase-underscores)
        (set-label (get-plain-label)))
      (define/public (restore-underscores)
        (unless (eq? saved-label 'not-yet-saved-label)
          (set-label saved-label)))
      (define saved-label 'not-yet-saved-label)
      (super-new)
      (set! saved-label (get-label))
      (unless (preferences:get 'framework:menu-bindings)
        (erase-underscores))))
  
  (define can-restore-menu-item% (can-restore-mixin menu-item%))
  (define can-restore-checkable-menu-item% (can-restore-mixin checkable-menu-item%))
  (define can-restore-underscore-menu% (can-restore-underscore-mixin menu%))
