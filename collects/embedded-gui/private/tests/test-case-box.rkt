(require
 mzlib/class
 mzlib/list
 mred
 mzlib/unitsig
 drscheme/tool
 mzlib/etc
 mzlib/match
 framework
 syntax/readerr
 string-constants
 embedded-gui
 string-constants)

(define test-case:program-editor% text%)

(define test-case-box%
  (class editor-snip%
    (inherit get-admin)
          
          (init-field
           [enabled? true]
           [actual-show? true]
           [collapsed? false]
           [to-test (new test-case:program-editor%)]
           [expected (new test-case:program-editor%)])
          
          (field
           [actual (new actual-text%)]
           [result (new result-snip%
                        (status (if enabled?
                                    'unknown
                                    'disabled)))])
    

          (define (show-actual show?)
            (set! actual-show? show?)
            (send show-actual-button set-state
                  (boolean->show-actual-btn-state show?))
            (send actual-pane show show?))
          
          (define (collapse bool)
            (set! collapsed? bool)
            (send collapse-button set-state
                  (boolean->collapse-btn-state bool))
            (send pb lock-alignment true)
            (send left show (not bool))
            (send right show (not bool))
            (send pb lock-alignment false))
            
          (define (boolean->collapse-btn-state bool)
            (if bool 'on 'off))
          
          (define (boolean->show-actual-btn-state bool)
            (if bool 'off 'on))
          
          (field
           [pb (new aligned-pasteboard%)]
           [main (new horizontal-alignment% (parent pb))]
           [left (new vertical-alignment%
                      (parent main)
                      (show? (not collapsed?)))]
           [right (new vertical-alignment%
                       (parent main)
                       (show? (not collapsed?)))]
           [button-pane (new vertical-alignment% (parent main))]
           [to-test-pane (new vertical-alignment% (parent left))]
           [expected-pane (new vertical-alignment% (parent right))]
           [actual-pane (new vertical-alignment%
                             (parent right)
                             (show? actual-show?))]
           [collapse-button
            (new turn-button-snip%
                 (state (boolean->collapse-btn-state collapsed?))
                 (turn-off
                  (lambda (b e) (collapse true)))
                 (turn-on
                  (lambda (b e) (collapse false))))]
           [show-actual-button
            (new turn-button-snip%
                 (state (boolean->show-actual-btn-state actual-show?))
                 (turn-off
                  (lambda (b e) (show-actual false)))
                 (turn-on
                  (lambda (b e) (show-actual true))))])
    
    (super-new (editor pb))
          
          (define (labeled-field alignment label text)
            ;; I string-append here to give space after the label
            ;; They look a lot better without something right after them.
            (new snip-wrapper%
                 (snip (make-object string-snip% (string-append label "     ")))
                 (parent alignment))
            (new snip-wrapper%
                 (snip (new stretchable-editor-snip%
                            (editor text)
                            (stretchable-height false)))
                 (parent alignment)))
          
          (labeled-field to-test-pane (string-constant test-case-to-test) to-test)
          (labeled-field expected-pane (string-constant test-case-expected) expected)
          
          (new snip-wrapper%
               (snip (make-object string-snip% (string-constant test-case-actual)))
               (parent actual-pane))
          (new snip-wrapper%
               (snip (new (grey-editor-snip-mixin stretchable-editor-snip%)
                          (editor actual)
                          (stretchable-height false)))
               (parent actual-pane))
          
          (new snip-wrapper%
               (snip result)
               (parent button-pane))
          ;; NOTE: When you add the collapse feature, be sure that
          ;; error-reporting on collapsed test-cases highlight the
          ;; test-case. (PR6955)
          (new snip-wrapper%
               (snip collapse-button)
               (parent button-pane))
          (new snip-wrapper%
               (snip show-actual-button)
               (parent button-pane))
    ))
  
  #;((-> void?) (-> void?) (symbols 'up 'down) . -> . snip%)
  ;; a snip which acts as a toggle button for rolling a window up and down
  (define turn-button-snip%
    (class toggle-button-snip%
      (super-new
       (images-off (cons (icon "turn-down.png") (icon "turn-down-click.png")))
       (images-on (cons (icon "turn-up.png") (icon "turn-up-click.png"))))))
  
  ;; a snip which will display a pass/fail result
  (define result-snip%
    (class image-snip%
      (inherit load-file)
      (init-field [status 'unknown])
      ;; ((symbols 'pass 'fail 'unknown 'disabled) . -> . void?)
      ;; updates the image with the icon representing one of three results
      (define/public (update value)
        (load-file
         (test-icon
          (case value
            [(pass) "small-check-mark.jpeg"]
            [(fail) "small-cross.jpeg"]
            [(unknown) "small-empty.gif"]
            [(disabled) "small-no.gif"]))))
      
      (super-new)
      (update status)))
      
  
  (define (icon str)
    (build-path (collection-path "icons") str))
  
  (define (test-icon str)
    (build-path (collection-path "test-suite") "private" "icons" str))
  
  ;; a locked text hightlighted to show that it is inactive
  (define actual-text%
    (class (grey-editor-mixin
            (text:hide-caret/selection-mixin racket:text%))
      (inherit hide-caret lock)
      (super-new)
      (hide-caret true)
      (lock true)))
  
(define f (new frame% (label "f") (width 400) (height 400)))
(define e (new text%))
(define c (new editor-canvas% (parent f) (editor e)))
(define t (new test-case-box%))
(send f show #t)
(send e insert t)
