(module tool mzscheme
  
  (provide tool@)
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unit.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "string-constant.ss" "string-constants")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard")
   "private/test-case-box.ss"
   "private/find-scheme-menu.ss"
   "private/text-syntax-object.ss"
   "private/print-to-text.ss")
  
  (define-signature menu-extentions^ ())
  (define-unit menu-extentions@
    (import drscheme:tool^ test-case-box^)
    (export menu-extentions^;drscheme:tool-exports^
             )
      ;; This delay is set up because reset-highlighting is called immediately
      ;; after execution where I don't want the test-cases to be cleared.
      ;; STATUS: It appears that the problem this flag was created to fix has been
      ;; fixed and is now delaying unecessarily. I have commented out the place where
      ;; it is set to true, effectively turning off the feature. I'll remove the code
      ;; if no bugs creap in after sufficient usage.
      (define delay? false)
      
      ;; This flag ensures that the test case boxes are only reset when the need
      ;; to be, which is only once after each execution of the program.
      (define needs-reset? false)
      
      ;; Adds the test suite tool menu to the Dr. Scheme frame
      ;; Updates the needs-reset? when the the program is executed
      (define test-case-mixin
        (mixin (drscheme:unit:frame<%> top-level-window<%> (class->interface frame%)) ()
          (inherit get-definitions-text get-edit-target-object get-menu-bar
                   get-special-menu)
          
          #;(-> void)
          ;; Called when the program is executed
          ;; Overriden to rest the test-cases.
          (define/override (execute-callback)
            (send (get-definitions-text) for-each-test-case
                  (lambda (case) (send case reset)))
            (super execute-callback)
            ;(set! delay? true)
            (set! needs-reset? true))
          
          #;(boolean . -> . void)
          ;; Enable or disable all of the test-cases
          (define (enable enable?)
            (send (get-definitions-text) for-each-test-case
                  (lambda (case) (send case enable enable?))))
          
          #;((is-a?/c menu-item%) . -> . void)
          ;; NOTE: This function is COPIED from the drscheme/private/unit.ss file
          (define (has-editor-on-demand menu-item)
            (let ([edit (get-edit-target-object)])
              (send menu-item enable (and edit (is-a? edit editor<%>)))))
          
          (super-new)
          
          ;; Create the new menu items.
          (field
           [test-cases-enabled? true]
           [insert-menu-item
            (new menu-item%
                 (label (string-constant test-case-insert))
                 (parent (get-special-menu))
                 (callback
                  (lambda (menu event)
                    (let ([test-box (new test-case-box% (enabled? test-cases-enabled?))]
                          [text (get-edit-target-object)])
                      (when text
                        (send text begin-edit-sequence)
                        (send text insert test-box)
                        (send test-box take-caret)
                        (send text end-edit-sequence)))))
                 (demand-callback has-editor-on-demand))])
          (let ([parent (find-scheme-menu (get-special-menu))])
            (and parent
                 (new menu-item%
                      (parent parent)
                      (label (string-constant test-case-disable-all))
                      (callback
                       (lambda (menu event)
                         (set! test-cases-enabled? (not test-cases-enabled?))
                         (if test-cases-enabled?
                             (send menu set-label (string-constant test-case-disable-all))
                             (send menu set-label (string-constant test-case-enable-all)))
                         (send (get-definitions-text) for-each-test-case
                               (lambda (tc) (send tc enable test-cases-enabled?))))))))))

      (drscheme:get/extend:extend-unit-frame test-case-mixin)
      
      ;; Adds a hook in the reset-highlighting to clear all of the test-case results when
      ;; the appropriate
      ;; STATUS: It's better to override reset-highlighting but this after-insert/delete works
      ;; for now.
      (define clear-results-mixin
        (mixin (editor<%>) ()
          (inherit find-first-snip)

          ;#;(case-> (-> boolean?) (boolean? . -> . void))
          ;;; Get or set the delay-reset field
          ;(define/public delay-reset
          ;  (case-lambda
          ;    [() delay?]
          ;    [(v) (set! delay? v)]))
          
          #;(-> void)
          ;; set all of the test-case-boxes in the definitions text to an unevaluated state
          (define/public (reset-test-case-boxes)
            (when needs-reset?
              (set! needs-reset? false)
              (for-each-test-case (lambda (snip) (send snip reset)))))
          
          #;(((is-a?/c test-case-box%) . -> . void) . -> . void)
          ;; executes the given function on each test-case-box
          (define/public (for-each-test-case f)
            (for-each-snip
               (lambda (snip)
                 (when (is-a? snip test-case-box%)
                   (f snip)))
               (find-first-snip)))
          
          (super-new)))
      
      (drscheme:get/extend:extend-definitions-text clear-results-mixin)

      ;; Require the test-case macro into every new namespace when a program is run.
      (define require-macro-mixin
        (mixin ((class->interface drscheme:rep:text%)) ()
          (inherit get-user-namespace get-definitions-text)
          
          #;((is-a?/c area<%>) . -> . (is-a?/c frame%))
          ;; The frame containing the given area
          (define (find-frame area)
            (let ([parent (send area get-parent)])
              (if parent
                  (find-frame parent)
                  area)))
          
          #;(-> void)
          ;; Called to indicate that the program annotations should be cleared.
          ;; Overriden to reset test case boxes
          (define/override (reset-highlighting)
            (super reset-highlighting)
            (let ([defs-text (get-definitions-text)])
              ;(if (send text delay-reset)
              ;    (send text delay-reset false)
              ;    (send text reset-test-case-boxes)))) 
              (send defs-text reset-test-case-boxes)))
          
          #;(-> void)
          ;; Called when the program is execute to reset the rep:text
          ;; Overriden to require the test case macro into any program that is executed.
          (define/override (reset-console)
            (super reset-console)
            (parameterize ([current-namespace (get-user-namespace)])
              (namespace-require '(lib "test-case.ss" "test-suite" "private"))))
          (super-new)))
      
      (drscheme:get/extend:extend-interactions-text require-macro-mixin))
  
  (define tool@
    (compound-unit/infer
     (import drscheme:tool^)
     (export drscheme:tool-exports^)
     (link menu-extentions@ test-case-box@ text->syntax-object@ print-to-text@)))
  )
