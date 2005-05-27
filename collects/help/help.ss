#|

This file contains all of the initialization of the Help Desk application.
It is only loaded when Help Desk is run by itself (outside DrScheme).

|#

(module help mzscheme 
  (require "bug-report.ss" ;; load now to init the preferences early
           (lib "cmdline.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "external.ss" "browser")
           "private/link.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "mred.ss" "mred"))
  
  (command-line
   "help-desk"
   (current-command-line-arguments))
  
  (preferences:add-panel
   (list (string-constant font-prefs-panel-title))
   (lambda (panel)
     (let* ([hp (new horizontal-panel% (parent panel))]
            [size (make-object slider% 
                    (string-constant font-size)
                    1
                    72
                    hp
                    (lambda (size evt)
                      (preferences:set 'framework:standard-style-list:font-size (send size get-value)))
                    (preferences:get 'framework:standard-style-list:font-size))])
       (preferences:add-callback
        'framework:standard-style-list:font-size
        (lambda (p v) (send size set-value v)))
       hp)))
  (color-prefs:add-background-preferences-panel)
  (preferences:add-warnings-checkbox-panel)
  (install-help-browser-preference-panel)
  
   ;; for use by the bug report frame.
  ;(namespace-set-variable-value! 'help-desk:frame-mixin (make-bug-report/help-desk-mixin 'the-hd-cookie))
  
  (handler:current-create-new-window 
   (lambda (filename) 
     (let ([browser-frame '((hd-cookie-new-browser the-hd-cookie))])
       (when (and filename
                  (file-exists? filename))
         (send (send (send browser-frame get-hyper-panel) get-canvas) goto-url
               (string-append "file://" filename)
               #f))
       browser-frame)))

  (new-help-desk))
  