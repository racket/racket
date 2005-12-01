#| Note: Test cases have a lot of state like the to-test, predicate, etc. I need to find a way
   to not have to maintain this list of state in many places. It's not as simple as a global
   list, however, because they need to be instantiation variables, etc.
|#
(module test-case-box mzscheme
  
  (provide test-case-box^ test-case-box@)
  
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "etc.ss")
   (lib "match.ss")
   (lib "framework.ss" "framework")
   (lib "readerr.ss" "syntax")
   (lib "string-constant.ss" "string-constants")
   (lib "embedded-gui.ss" "embedded-gui")
   "make-snipclass.ss"
   "convert-to-string.ss"
   "text-syntax-object.ss"
   "print-to-text.ss"
   "test-case.ss"
   (only (lib  "teachprims.ss" "lang" "private") beginner-equal?))
  
  (define-signature test-case-box^ (test-case-box% phase1 phase2))
  (define test-case-box@
    (unit/sig test-case-box^
      (import drscheme:tool^ text->syntax-object^ print-to-text^)
      
      (define test-case:program-editor% false)
      
      (define (phase1) (void))
      (define (phase2)
        (set! test-case:program-editor%
              (init-text-mixin
               (tabbable-text-mixin
                ((drscheme:unit:get-program-editor-mixin)
                 scheme:text%)))))
      
      ;; The test case box that is inserted into the drscheme frame
      (define test-case-box%
         (class* (on-show-editor-snip-mixin
                  (convert-to-string-mixin
                   (decorated-editor-snip-mixin editor-snip%))) (readable-snip<%>)
           (inherit get-admin convert-to-string)
           
           ;; A text that will uncollapse the test case when it is highlighted for an error
           (define error-alert-text%
             (class test-case:program-editor%
               (define/override highlight-range
                 (opt-lambda (start end color (bitmap false) (caret-space false) (priority 'low))
                   (when collapsed? (collapse false))
                   (super highlight-range start end color bitmap caret-space priority)))
               (super-new)))
           
           (init-field
            [enabled? true]
            [actual-show? false]
            [collapsed? false]
            [show-right-pane? false]
            [error-box? false]
            [to-test (new error-alert-text%)]
            [expected (new error-alert-text%)]
            [predicate (new error-alert-text% (text ""))]
            [should-raise (new error-alert-text% (text ""))]
            [error-message (new error-alert-text%)])
           
           #;(any? (union integer? false?) (union integer? false?) (union integer? false?) . -> . any?)
           ;; Called by the execution to get a syntax object that represents this box.
           (define/public read-special
             (opt-lambda (source (line false) (column false) (position false))
               #;((is-a?/c text%) . -> . syntax-object?)
               ;; Creates a single syntax object out of the text or raises read-error
               (define (text->syntax-object text default-content)
                 (match (text->syntax-objects text default-content)
                   [() (raise-read-error (string-constant test-case-empty-error)
                                         source line false position 1)]
                   [(stx) stx]
                   [(stx next rest-stx ...)
                    (raise-read-error (string-constant test-case-too-many-expressions-error)
                                      text
                                      (syntax-line next)
                                      (syntax-column next)
                                      (syntax-position next)
                                      (syntax-span next))]))
	       (syntax-property
		(if enabled?
		    (with-syntax ([to-test-stx (syntax-property (text->syntax-object to-test #f)
								'stepper-test-suite-hint
								true)]
				  [update-stx (lambda (x) (update x))] ; eta public method
				  [set-actuals-stx set-actuals]
				  [w printf])
		      (if error-box?
			  (with-syntax ([exn-pred-stx (text->syntax-object should-raise #'exn:fail?)]
					[exn-handler-stx
					 (if (empty-text? error-message)
					     #'(lambda (v) true)
					     #`(lambda (v)
						 (equal? (exn-message v)
							 #,(text->syntax-object
							    error-message
							    #f))))])
			    (syntax/loc (datum->syntax-object
					 false 'ignored (list source line column position 1))
			      (test-error-case to-test-stx
					       exn-pred-stx
					       exn-handler-stx
					       update-stx
					       set-actuals-stx)))
			  (with-syntax ([exp-stx (text->syntax-object expected #f)]
					[pred-stx (text->syntax-object predicate beginner-equal?)])
			    (syntax/loc (datum->syntax-object
					 false 'ignored (list source line column position 1))
			      (test-case pred-stx
					 to-test-stx
					 exp-stx
					 update-stx
					 set-actuals-stx)))))
		    (syntax-property #'(define-values () (values))
				     'stepper-skip-completely
				     true))
		'test-case-box #t)))
           
           #;(boolean? . -> . void?)
           ;; sets the test case to the proper result bassed on if it was correct
           (define/public (update pass?)
             (send result update (if pass? 'pass 'fail)))
           
           #;(-> void?)
           ;; resets the state of the test case
           (define/public (reset)
             (send pb lock-alignment true)
             (send* actual
               (lock false)
               (erase)
               (lock true))
             (when enabled?
               (send result update 'unknown))
             (send pb lock-alignment false))
           
           #;(boolean? . -> . void?)
           ;; enables or disables the test case
           (define/public (enable enable?)
             (unless (boolean=? enabled? enable?)
               (if enable?
                   (begin (set! enabled? true)
                          (send result update 'unknown))
                   (begin (set! enabled? false)
                          (reset)
                          (send result update 'disabled)))))
           
           #;(-> void)
           ;; tells the test-box to take the caret
           (define/public (take-caret)
             (send pb set-caret-owner
                   (send (send to-test get-admin) get-snip)
                   'display))
           
           #;(-> string)
           ;; The textual representation of this test-case
           ;; STATUS: Begginner language doesn't have with-handlers
           ;; STATUS: Pretty printing not here yet.
           (define (get-string)
             (if error-box?
                 "Not yet implemented. What to do in beginner?"
                 (format "(~a ~a ~a)"
                         (send predicate get-text)
                         (send to-test get-text)
                         (send expected get-text))))
           
           #;((is-a?/c expand-program%) (listof any?) . -> . void?)
           ;; set the text in the actual field to the value given
           ;; STATUS: Ensure the edit-sequence is needed.
           (define (set-actuals vals)
             (send (send (get-admin) get-editor) begin-edit-sequence)
             (send actual lock false)
             (print-to-text actual vals)
             (send actual lock true)
             (send (send (get-admin) get-editor) end-edit-sequence))
           
           ;;;;;;;;;;
           ;; Saving and Copying
           
           #;(-> (is-a?/c test-case-box%))
           ;; Called by drscheme to copy and paste this test-case
           (define/override (copy)
             (let ([new-to-test (new error-alert-text%)]
                   [new-expected (new error-alert-text%)]
                   [new-predicate (new error-alert-text%)]
                   [new-should-raise (new error-alert-text%)]
                   [new-error-message (new error-alert-text%)])
               (send to-test copy-self-to new-to-test)
               (send expected copy-self-to new-expected)
               (send predicate copy-self-to new-predicate)
               (send should-raise copy-self-to new-should-raise)
               (send error-message copy-self-to new-error-message)
               (new test-case-box%
                    (enabled? enabled?)
                    (actual-show? actual-show?)
                    (collapsed? collapsed?)
                    (show-right-pane? show-right-pane?)
                    (error-box? error-box?)
                    (to-test new-to-test)
                    (expected new-expected)
                    (predicate predicate)
                    (should-raise should-raise)
                    (error-message error-message))))
           
           #;((is-a?/c editor-stream-out%) . -> . void?)
           ;; Writes this test case box to the given file.
           (define/override (write f)
             (send to-test write-to-file f)
             (send expected write-to-file f)
             (send predicate write-to-file f)
             (send should-raise write-to-file f)
             (send error-message write-to-file f)
             (send f put (if enabled? 1 0))
             (send f put (if collapsed? 1 0))
             (send f put (if error-box? 1 0)))
           
           
           #;((is-a?/c editor-stream-in%) . -> . void?)
           ;; Reads the state of the box in from the given stream
           (define/public (read-from-file f)
             (let ([enabled?-box (box 0)]
                   [collapsed?-box (box 0)]
                   [error-box?-box (box 0)])
	       (let ([vers (send tcb-sc reading-version f)])
		 (case vers
		   [(1)
		    ;; Discard comment:
		    (send (new text%) read-from-file f)
		    (send* to-test (erase) (read-from-file f))
		    (send* expected (erase) (read-from-file f))
		    ;; Nothing else is in the stream in version 1,
		    ;;  so leave the defaults
		    ]
		   [(2)
		    (send* to-test (erase) (read-from-file f))
		    (send* expected (erase) (read-from-file f))
		    (send* predicate (erase) (read-from-file f))
		    (send* should-raise (erase) (read-from-file f))
		    (send* error-message (erase) (read-from-file f))
		    (send f get enabled?-box)
		    (send f get collapsed?-box)
		    (send f get error-box?-box)
		    (enable (= (unbox enabled?-box) 1))
		    ;; Presently this is poking a bug in the embedded-gui.
		    ;; I'll leaving it commented til I reduce the bug.
		    #;(collapse (= (unbox collapsed?-box) 1))
		    (toggle-error-box (= (unbox error-box?-box) 1))]))))
           
           ;;;;;;;;;;
           ;; Layout
           
           #;(-> (is-a?/c bitmap%))
           ;; The bitmap to use for the top corner of the box.
           (define/override (get-corner-bitmap)
             (if error-box?
                 (make-object bitmap% (icon "scheme-box.jpg"))
                 (make-object bitmap% (icon "scheme-box.jpg"))))
           
           #;(-> (symbols 'top-right 'top-left 'bottom-left 'bottom-right))
           ;; The location of the corner bitmap
           (define/override (get-position) 'top-right)
           
           #;(-> (union string? (is-a?/c color%)))
           ;; The color of the border of this box
           (define/override (get-color) (if error-box? "red" "purple"))
           
           #;(-> (is-a?/c popup-menu%))
           ;; The popup menu used for the top corner of this box
           (define/override (get-menu)
             (let ([the-menu (new popup-menu% (title (string-constant test-case-menu-title)))])
               (define (make-toggle label f init)
                 (letrec ([item (new checkable-menu-item%
                                     (parent the-menu)
                                     (label label)
                                     (checked init)
                                     (callback (lambda (m e)
                                                 (f (send item is-checked?)))))])
                   item))
               (new menu-item%
                    (label (if error-box?
                               (string-constant test-case-switch-to-nonerror-box)
                               (string-constant test-case-switch-to-error-box)))
                    (parent the-menu)
                    (callback (lambda (m e)
                                (toggle-error-box (not error-box?)))))
               (make-toggle
                (string-constant test-case-collapse)
                collapse collapsed?)
               (make-toggle
                (string-constant test-case-show-actual)
                show-actual actual-show?)
               (make-toggle
                (string-constant test-case-enable)
                (lambda (b) (enable b)) enabled?) ; eta public method
               (make-toggle
                (if error-box?
                    (string-constant test-case-show-error-message)
                    (string-constant test-case-show-predicate))
                show-right-pane show-right-pane?)
               (new menu-item%
                    (label (string-constant test-case-convert-to-text))
                    (parent the-menu)
                    (callback
                     (lambda (m e)
                       (convert-to-string (get-string)))))
               the-menu))
           
           #;(-> void)
           ;; Hide and show the boxes that differ between error and now and
           ;; poke the snip-parent to display the new boarder color
           (define (toggle-error-box bool)
             (set! error-box? bool)
             (send pb lock-alignment true)
             (send should-be-pane show (not error-box?))
             (send should-raise-pane show error-box?)
             (send predicate-pane show (not error-box?))
             (send error-message-pane show error-box?)
             (send pb lock-alignment false)
             (if error-box?
                 (set-tabbing to-test should-raise)
                 (set-tabbing to-test expected))
             (>>= (snip-parent this)
                  (lambda (admin)
                    (send admin resized this true))))
           
           #;(boolean? . -> . void)
           ;; Shows or hides the actual box
           (define (show-actual show?)
             (set! actual-show? show?)
             (send pb lock-alignment true)
             (send show-actual-button set-state
                   (boolean->show-actual-btn-state show?))
             (send to-test-pane stretchable-height show?)
             (send actual-pane show show?)
             (send pb lock-alignment false))
           
           #;(boolean? . -> . void)
           ;; Toggles the test-case between a collapsed minimal state and one with entry boxes.
           (define (collapse bool)
             (set! collapsed? bool)
             (send collapse-button set-state
                   (boolean->collapse-btn-state bool))
             (send body show (not bool)))
           
           #;(booean? . -> . void)
           ;; Shows or hides the predicate box
           (define (show-right-pane show?)
             (set! show-right-pane? show?)
             (send right-pane show show-right-pane?))
           
           ;;;;;;;;;;
           ;; Box layout
           
           (field
            [pb (new aligned-pasteboard%)]
            [main (new horizontal-alignment% (parent pb))])
           
           ;;;;;;;;;;
           ;; The button bar w/ result check mark box
           
           (field
            [button-pane (new vertical-alignment% (parent main))]
            [result (new result-snip% (status (if enabled? 'unknown 'disabled)))])
           (snip-wrap button-pane result)
           (field
            [collapse-button
             (new turn-button%
                  (parent button-pane)
                  (state (boolean->collapse-btn-state collapsed?))
                  (turn-off (lambda (b e) (collapse true)))
                  (turn-on (lambda (b e) (collapse false))))]
            [show-actual-button
             (new turn-button%
                  (parent button-pane)
                  (state (boolean->show-actual-btn-state actual-show?))
                  (turn-off (lambda (b e) (show-actual false)))
                  (turn-on (lambda (b e) (show-actual true))))])
           
           ;;;;;;;;;;
           ;; The text boxes
           
           (field
            [body (new horizontal-alignment% (parent main) (show? (not collapsed?)))]
            [to-test-pane
             (new labeled-text-field%
                  (parent body)
                  (label (string-constant test-case-to-test))
                  (text to-test))]
            
            [result-pane (new vertical-alignment% (parent body))]
            [should-be-pane
             (new labeled-text-field%
                  (parent result-pane)
                  (show? (not error-box?))
                  (label (string-constant test-case-expected))
                  (text expected))]
            [should-raise-pane
             (new labeled-text-field%
                  (parent result-pane)
                  (show? error-box?)
                  (label (string-constant test-case-should-raise))
                  (text should-raise))]
            [actual (new actual-text%)]
            [actual-pane
             (new labeled-text-field%
                  (parent result-pane)
                  (label (string-constant test-case-actual))
                  (show? actual-show?)
                  (snip-class (grey-editor-snip-mixin stretchable-editor-snip%))
                  (text actual))]
            
            [right-pane (new vertical-alignment% (parent body) (show? show-right-pane?))]
            [predicate-pane
             (new labeled-text-field%
                  (parent right-pane)
                  (show? (not error-box?))
                  (label (string-constant test-case-predicate))
                  (text predicate))]
            [error-message-pane
             (new labeled-text-field%
                  (parent right-pane)
                  (show? error-box?)
                  (label (string-constant test-case-error-message))
                  (text error-message))])
           
           (super-new (editor pb))
           
           (set-tabbing to-test expected predicate)
           (set-tabbing should-raise error-message)
           
           ;;;;;;;;;;
           ;; Snip class
           
           (inherit set-snipclass)
           (set-snipclass tcb-sc)))
        
      ;;;;;;;;;;
      ;; Snip class
      
      ;; A snip-class for the test case box
      (define tcb-sc
        (make-snipclass
         test-case-box%
         "test-case-box%"
         #;
	 (lambda (class% f)
           (let ([enabled?-box (box 0)]
                 [collapsed?-box (box 0)]
                 [error-box?-box (box 0)]
                 [to-test (new test-case:program-editor%)]
                 [expected (new test-case:program-editor%)]
                 [predicate (new test-case:program-editor%)]
                 [should-raise (new test-case:program-editor%)]
                 [error-message (new test-case:program-editor%)])
             (send to-test read-from-file f)
             (send expected read-from-file f)
             (send predicate read-from-file f)
             (send should-raise read-from-file f)
             (send error-message read-from-file f)
             (send f get enabled?-box)
             (send f get collapsed?-box)
             (send f get error-box?-box)
             (new class%
                  (enabled? (= (unbox enabled?-box) 1))
                  (collapsed? (= (unbox collapsed?-box) 1))
                  (error-box? (= (unbox error-box?-box) 1))
                  (to-test to-test)
                  (expected expected)
                  (predicate predicate)
                  (should-raise should-raise)
                  (error-message error-message))))))
      ))
  
  #;((-> void?) (-> void?) (symbols 'up 'down) . -> . snip%)
  ;; a snip which acts as a toggle button for rolling a window up and down
  ;; STATUS : Change this to derive embedded-toggle-button%
  (define turn-button%
    (class embedded-toggle-button%
      (super-new
       (images-off (cons (icon "turn-down.png") (icon "turn-down-click.png")))
       (images-on (cons (icon "turn-up.png") (icon "turn-up-click.png"))))))
  
  ;; a snip which will display a pass/fail result
  (define result-snip%
    (class image-snip%
      (inherit set-bitmap)
      (init-field [status 'unknown])
      ;; ((symbols 'pass 'fail 'unknown 'disabled) . -> . void?)
      ;; updates the image with the icon representing one of three results
      (define/public (update value)
	(set-bitmap
	 (memoize value
		  (lambda ()
		    (make-object bitmap%
				 (test-icon
				  (case value
				    [(pass) "small-check-mark.jpeg"]
				    [(fail) "small-cross.jpeg"]
				    [(unknown) "small-empty.gif"]
				    [(disabled) "small-no.gif"])))))))
      
      (super-new)
      (update status)))

  (define memory (make-hash-table 'equal))
  (define (memoize k thunk)
    (hash-table-get memory k (lambda ()
			       (let ([v (thunk)]) 
				 (hash-table-put! memory k v)
				 v))))
  
  #;(string? . -> . string?)
  ;; A path to the icon given a file name
  (define (icon str)
    (build-path (collection-path "icons") str))
  
  #;(string? . -> . string?)
  ;; A path to the icon in the test-suite given a file name
  (define (test-icon str)
    (build-path (collection-path "test-suite") "private" "icons" str))
  
  ;; a locked text hightlighted to show that it is inactive
  (define actual-text%
    (class (grey-editor-mixin
            (text:hide-caret/selection-mixin scheme:text%))
      (inherit hide-caret lock)
      (super-new)
      (hide-caret true)
      (lock true)))

  ;; a text mixin that gives the text an init arg of an initial contents
  (define init-text-mixin
    (mixin ((class->interface text%)) ()
      (inherit insert)
      (init [text ""])
      (super-new)
      (insert text)))
           
  #;(boolean? . -> . (symbols 'on 'off))
  ;; converts a boolean to the value expected by the collapse button
  (define (boolean->collapse-btn-state bool)
    (if bool 'on 'off))
  
  #;(boolean? . -> . (symbols 'on 'off))
  ;; converts a boolean to the value expected by the show actual button
  (define (boolean->show-actual-btn-state bool)
    (if bool 'off 'on))
  
  #;((is-a?/c text%) . -> . boolean?)
  ;; True if the given text is empty
  (define (empty-text? t)
    (let ([str (send t get-text)])
      (string=? str "")))
  
  ;;;;;;;;;;
  ;; Eaiser syntax for embedded-gui
  (define (snip-wrap p snip)
    (new snip-wrapper% (parent p) (snip snip)))
  
  ;; Inserts a label and a text field into the given alignment
  (define labeled-text-field%
    (class vertical-alignment%
      (init label text (snip-class stretchable-editor-snip%))
      (super-new (stretchable-height false))
      (new embedded-message% (parent this) (label label))
      (new snip-wrapper%
           (parent this)
           (snip (new snip-class
                      (editor text)
                      (min-width 80))))))
  
  #;((union any? false?) (any? . -> . any?) . -> . (union any? false?))
  ;; Send the value to the function unless it 'fails' by returning false. Like haskell's bind operator.
  (define (>>= value function)
    (cond
      [value => function]
      [else false]))
  )
