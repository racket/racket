(module time-keystrokes mzscheme
  
  (require drscheme/tool
           mzlib/list
           mzlib/unit
           mzlib/class
           mzlib/etc
           mred
           framework)
  
  (provide tool@)
  
  (define short-str "(abc)")
  (define chars-to-test (build-string
                         400
                         (λ (i) (string-ref short-str (modulo i (string-length short-str))))))
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define (tool-mixin super%)
        (class super%
          (inherit get-button-panel)
          (super-new)
          (let ([button (new button%
                             (label "Time Keystrokes")
                             (parent (get-button-panel))
                             (callback
                              (lambda (button evt) 
                                (time-keystrokes this))))])
            (send (get-button-panel) change-children
                  (lambda (l)
                    (cons button (remq button l)))))))
      
      (define (time-keystrokes frame)
        (let loop ([n 10])
          (when (zero? n)
            (error 'time-keystrokes "could not find drscheme frame"))
          (let ([front-frame (test:get-active-top-level-window)])
            (unless (eq? front-frame frame)
              (sleep 1/10)
              (loop (- n 1)))))
        (let ([win (send frame get-definitions-canvas)])
          (send win focus)
          (time (send-key-events win chars-to-test))))
      
      (define (send-key-events window chars)
        (for-each (λ (char)
                    (send-key-event window (new key-event% (key-code char))))
                  (string->list chars)))
      
      
      ;; copied from framework/test.rkt
      (define (send-key-event window event)
        (let loop ([l (ancestor-list window #t)])
          (cond [(null? l)
                 (cond
                   [(method-in-interface? 'on-char (object-interface window))
                    (send window on-char event)]
                   [(is-a? window text-field%)
                    (send (send window get-editor) on-char event)]
                   [else 
                    (error
                     'send-key-event
                     "focused window is not a text-field% and does not have on-char: ~s" window)])]
                [(send (car l) on-subwindow-char window event) #f]
                [else (loop (cdr l))])))
      
      ;; copied from framework/test.rkt
      (define (ancestor-list window stop-at-top-level-window?)
        (let loop ([w window] [l null])
          (if (or (not w)
                  (and stop-at-top-level-window?
                       (is-a? w top-level-window<%>)))
              l
              (loop (send w get-parent) (cons w l)))))
      
      (when (getenv "PLTDRKEYS")
        (printf "PLTDRKEYS: installing unit frame mixin\n")
        (drscheme:get/extend:extend-unit-frame tool-mixin)))))

