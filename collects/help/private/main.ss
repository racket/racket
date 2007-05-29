(module main (lib "a-unit.ss")
  (require (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "external.ss" "browser")
           (lib "string-constant.ss" "string-constants")
           (lib "xml.ss" "xml")
           (lib "htmltext.ss" "browser")
           (prefix home: "../servlets/home.ss")
           "sig.ss")
  
  (import)
  (export main^)
  
      ;; where should the pref stuff really go?
      (preferences:set-default 'drscheme:help-desk:last-url-string "" string?)
      (preferences:set-default 'drscheme:help-desk:frame-width 350 number?)
      (preferences:set-default 'drscheme:help-desk:frame-height 400 number?)
      (preferences:set-default 'drscheme:help-desk:search-how 1 (lambda (x) (member x '(0 1 2))))
      (preferences:set-default 'drscheme:help-desk:search-where 1 (lambda (x) (member x '(0 1 2))))
      
      (preferences:set-default 'drscheme:help-desk:separate-browser #t boolean?)
      (preferences:set-default 'drscheme:help-desk:ask-about-external-urls #t boolean?)
      
      (preferences:set-default 'drscheme:help-desk:font-size 
                               (cons #f 
                                     (let* ([txt (make-object text%)]
                                            [stl (send txt get-style-list)]
                                            [bcs (send stl basic-style)])
                                       (send bcs get-size)))
                               (λ (x) 
                                 (and (pair? x)
                                      (boolean? (car x))
                                      (and (integer? (cdr x))
                                           (<= 0 (cdr x) 255)))))
      
      ;; create "Html Standard" style to be able to
      ;; adjust its size in the preferences dialog
      (let* ([sl (editor:get-standard-style-list)]
             [html-standard-style-delta (make-object style-delta% 'change-nothing)]
             [html-standard-style 
              (send sl find-or-create-style 
                    (send sl find-named-style "Standard")
                    html-standard-style-delta)])
        (send sl new-named-style "Html Standard" html-standard-style))
      
      (preferences:add-callback
       'drscheme:help-desk:font-size
       (λ (k v) (update-font-size v)))
      
      (define (update-font-size v)
        (let ([style (send (editor:get-standard-style-list) find-named-style "Html Standard")])
          (send style set-delta
                (if (car v)
                    (make-object style-delta% 'change-size (cdr v))
                    (make-object style-delta% 'change-nothing)))))
      (update-font-size (preferences:get 'drscheme:help-desk:font-size))
      
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
                      (value (not (preferences:get 'drscheme:help-desk:separate-browser)))
                      (callback
                       (lambda (cb evt)
                         (preferences:set 'drscheme:help-desk:separate-browser
                                          (not (send cb get-value))))))])
           ;; Put checkbox panel at the top:
           (send panel change-children (lambda (l) (cons cbp l)))
           (preferences:add-callback 
            'drscheme:help-desk:separate-browser
            (lambda (p v) (send cb set-value (not v))))
           (void))))
      
      (define (add-help-desk-font-prefs show-example?)
        (preferences:add-panel
         (list (string-constant font-prefs-panel-title) 
               (string-constant help-desk))
         (lambda (panel)
           (let* ([vp (new vertical-panel% (parent panel) (alignment '(left top)))]
                  [use-drs (new check-box%
                                (label (string-constant use-drscheme-font-size))
                                (parent vp)
                                (value (not (car (preferences:get 'drscheme:help-desk:font-size))))
                                (callback
                                 (λ (cb y)
                                   (preferences:set 'drscheme:help-desk:font-size 
                                                    (cons (not (send cb get-value))
                                                          (cdr (preferences:get
                                                                'drscheme:help-desk:font-size)))))))]
                  [size (new slider% 
                             (label (string-constant font-size))
                             (min-value 1)
                             (max-value 255)
                             (parent vp)
                             (callback
                              (λ (size evt)
                                (preferences:set 'drscheme:help-desk:font-size
                                                 (cons 
                                                  #t
                                                  (send size get-value)))))
                             (init-value
                              (cdr (preferences:get 'drscheme:help-desk:font-size))))]
                  [hp (new horizontal-panel% 
                           (alignment '(center center))
                           (stretchable-height #f)
                           (parent vp))]
                  [mk-button
                   (λ (label func)
                     (new button% 
                          (parent hp)
                          (label label)
                          (callback
                           (λ (k v) 
                             (let ([old (preferences:get 'drscheme:help-desk:font-size)])
                               (preferences:set 'drscheme:help-desk:font-size 
                                                (cons (car old) 
                                                      (func (cdr old)))))))))]
                  [sub1-button (mk-button "-1" sub1)]
                  [add1-button (mk-button "+1" add1)]
                  [enable/disable
                   (λ (v)
                     (send size enable (car v))
                     (send sub1-button enable (car v))
                     (send add1-button enable (car v))
                     (send size set-value (cdr v)))])
             (preferences:add-callback
              'drscheme:help-desk:font-size 
              (λ (k v)
                (enable/disable v)))
             (enable/disable (preferences:get 'drscheme:help-desk:font-size))
             
             (when show-example?
               (let* ([show-message
                       (λ ()
                         (message-box 
                          (string-constant help-desk)
                          (string-constant help-desk-this-is-just-example-text)))]
                      [mix
                       (λ (%)
                         (class %
                           (inherit set-clickback)
                           (define/override (add-link p1 p2 s)
                             (set-clickback p1 p2 (lambda (e x y) (show-message))))
                           (define/override (add-thunk-callback p1 p2 thunk)
                             (set-clickback p1 p2 (lambda (e p1 p2) (show-message))))
                           (define/override (add-scheme-callback p1 p2 scheme) 
                             (set-clickback p1 p2 (lambda (e p1 p2) (show-message))))
                           (super-new)))]
                      [text (new (mix (html-text-mixin (text:hide-caret/selection-mixin 
                                                        text:standard-style-list%))))]
                      [msg (new message% (parent vp) (label (string-constant example-text)))]
                      [ec (new editor-canvas% (parent vp) (editor text))])
                 (let-values ([(in out) (make-pipe)])
                   (thread
                    (λ ()
                      (write-xml/content (xexpr->xml (home:start #f)) out)
                      (close-output-port out)))
                   (render-html-to-text in text #f #t))))
                    
             vp)))))