(module main mzscheme
  (require (lib "unitsig.ss")
           (lib "sig.ss" "web-server")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "external.ss" "browser")
           (lib "string-constant.ss" "string-constants")
           "sig.ss")
  
  (provide main@)
  
  (define main@
    (unit/sig ()
      (import)
      
      ;; where should the pref stuff really go?
      (preferences:set-default 'drscheme:help-desk:last-url-string "" string?)
      (preferences:set-default 'drscheme:help-desk:frame-width 350 number?)
      (preferences:set-default 'drscheme:help-desk:frame-height 400 number?)
      (preferences:set-default 'drscheme:help-desk:search-how 1 (lambda (x) (member x '(0 1 2))))
      (preferences:set-default 'drscheme:help-desk:search-where 1 (lambda (x) (member x '(0 1 2))))
      
      (preferences:set-default 'drscheme:help-desk:separate-browser #t boolean?)
      (preferences:set-default 'drscheme:help-desk:ask-about-external-urls #t boolean?)
      
      (add-to-browser-prefs-panel
       (lambda (panel)
         (let* ([cbp (instantiate group-box-panel% ()
                       (parent panel)
                       (label (string-constant plt:hd:external-link-in-help))
                       (alignment '(left center))
                       (stretchable-height #f)
                       (style '(deleted)))]
                [cb (instantiate check-box% ()
                      (label (string-constant plt:hd:use-homebrew-browser))
                      (parent cbp)
                      (value (preferences:get 'drscheme:help-desk:separate-browser))
                      (callback
                       (lambda (cb evt)
                         (preferences:set 'drscheme:help-desk:separate-browser
                                          (not (send cb get-value))))))])
           ;; Put checkbox panel at the top:
           (send panel change-children (lambda (l) (cons cbp l)))
           (preferences:add-callback 
            'drscheme:help-desk:separate-browser
            (lambda (p v) (send cb set-value (not v))))
           (void)))))))