(module tester mzscheme
  
  (require (lib "mred.ss" "mred")
           (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           (lib "class.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "etc.ss"))
           
  (require "ast.ss" "display-java.ss" "parameters.ss")
  
  (provide test-info% test-display% test-tool@)

;                                              #              *##$          *#*    
;    #                       #                                #*            # #    
;   #####    $##$   *###$#  #####            :##    ##*##*  @#####   &##&   *#*    
;    #      $   -$  #$  -#   #                 #     #+ *#    #     &+  +&   -+$#  
;    #      ######  *###$    #      ######     #     #   #    #     #    #  +$&:   
;    #      $           +#   #                 #     #   #    #     #    #    *#*  
;    #* :$  +*      #   *#   #* :$             #     #   #    #     &+  +&    # #  
;    *##$    +##$+  @*###*   *##$            #####  ### ### :#####   &##&     *#*
  
  
  #;(make-single-test string (listof testcase) (listof string) 
                      int (listof failed-check) (listof src))
  (define-struct single-test (name testcases not-tested 
                                   num-checks failed-checks covered-exprs
                                   covered-methods))
  
  ;(make-failed-check src (listof (U string snip%)) (listof src))
  (define-struct failed-check (src msg covers))
  
  ;(make-testcase string boolean (listof src))
  (define-struct testcase (name passed? covers))
  
  (define-local-member-name provide-test-results provide-covered)
  
  (define test-info%
    (class* object% ()
            
      (define tested-classes null);------ (listof single-test)
      (define covered null);------------- (listof src)
      (define nearly-tested-classes null);(listof string)
      
      (define current-class (make-single-test "" null null 0 null null null))
      (define current-testcoverage null)
      
      (define total-tests 0)
      (define failed-tests 0)
      (define total-checks 0)
      (define failed-checks 0)
      
      (define current-test-obj null)
        
      (define/public (add-check)
        (set-single-test-num-checks! current-class 
                                     (add1 (single-test-num-checks current-class)))
        (set! total-checks (add1 total-checks)))
            
      ;check-failed: (list (U string snip%)) src -> void
      (define/public (check-failed msg src)
        (set-single-test-failed-checks! current-class
                                      (cons 
                                       (make-failed-check src msg null)
                                       (single-test-failed-checks current-class)))
        (set! failed-checks (add1 failed-checks)))
      
      (define/public (format-value value)
        (make-java-snip value (make-format-style #t 'field #f)))
      
      (define/public (covered-position src)
        (set! covered (cons src covered))
        (set! current-testcoverage (cons src current-testcoverage))
        (set-single-test-covered-exprs! 
         current-class 
         (cons src (single-test-covered-exprs current-class)))
        (when (and (testcase-ext?) src)
          (send current-test-obj testCoverage-boolean-int #f (src-pos src))))
      
      (define/public (provide-test-results)
        (values tested-classes covered nearly-tested-classes total-tests 
                failed-tests total-checks failed-checks))
  
      (define/public (provide-covered) covered)
      
      ;run-tests: (listof (list string class)) (listof string) -> (listof object)
      (define/public (run-tests tests close-names)
        (let ((objects
               (map 
                (lambda (name/class)
                  (set! current-class (make-single-test (car name/class) null null 0 null null null))
                  (let ((obj (make-object (cadr name/class))))
                    (when (testcase-ext?) (set! current-test-obj obj))
                    (with-handlers ((exn? (lambda (e) (raise e))))
                      ((current-eval)
                       #`(send #,obj #,(string->symbol (string-append (car name/class)
                                                                      "-constructor")))))
                    (if (testcase-ext?)
                        (run-testcases obj)
                        (run-methods obj))
                    (set! tested-classes (cons current-class tested-classes))
                    (when (testcase-ext?)
                      (set-single-test-covered-methods! current-class (send obj testCoverage-boolean-int #t 1)))
                    (list (car name/class) obj)))
                tests)))
          (set! nearly-tested-classes close-names)
          (map cadr objects)))
      
      (define/private (run-testcases object)
        (let loop ([methods (send object testMethods)])
          (cond
            [(null? methods) (void)]
            [else 
             (set! total-tests (add1 total-tests))
             (set! current-testcoverage null)
             (let ((res ((cadr (car methods)))))
               (set-single-test-testcases!
                current-class
                (cons (make-testcase (car (car methods)) res current-testcoverage)
                      (single-test-testcases current-class)))
               (unless res (set! failed-tests (add1 failed-tests))))
             (loop (cdr methods))])))            
      
      (define/private (run-methods object)
        (let loop ([methods (reverse (interface->method-names (object-interface object)))])
          (cond
            ((null? methods) (void))
            ((test-method? (car methods))
             (set! total-tests (add1 total-tests))
             (set! current-testcoverage null)
             (let ((res ((current-eval)
                         #`(send #,object #,(car methods)))))
               (set-single-test-testcases!
                current-class
                (cons (make-testcase (car methods) res current-testcoverage)
                      (single-test-testcases current-class)))
               (unless res (set! failed-tests (add1 failed-tests))))
             (loop (cdr methods)))
            ((test-method-name? (car methods))
             (set-single-test-not-tested!
              current-class
              (cons (format "Method ~a could not run due to requiring arguments."
                            (car methods))
                    (single-test-not-tested current-class)))
             (loop (cdr methods)))
            ((close-to-test-name? (car methods))
             (set-single-test-not-tested!
              current-class
              (cons (format "Method ~a has a name similar to a test, but does not begin with 'test'."
                            (car methods))
                    (single-test-not-tested current-class)))
             (loop (cdr methods)))
            (else (loop (cdr methods))))))
            
      (define (test-method? name)
        (and (test-method-name? name) (no-args? name)))
      
      (define (test-method-name? name)
        (regexp-match "^test" (symbol->string name)))
      
      (define (no-args? name)
        (not (regexp-match "-" (symbol->string name))))
      
      (define (close-to-test-name? name)
        (let ((n (symbol->string name)))
          (or (regexp-match "^tst" n)
              (regexp-match "^tet" n)
              (regexp-match "^Test" n)
              (regexp-match "^tes" n))))
      
      (super-instantiate ())
      ))

;                                               ##     #                      ##                    *#*    
;    #                       #                   #                             #                    # #    
;   #####    $##$   *###$#  #####            $#@ #   :##    *###$#  ##:#@      #     $@#$:  ##: :## *#*    
;    #      $   -$  #$  -#   #              $+  +#     #    #$  -#   #* -$     #        -#  -$   $   -+$#  
;    #      ######  *###$    #      ######  #    #     #    *###$    #   #     #     $##$#   $- *$  +$&:   
;    #      $           +#   #              #    #     #        +#   #   #     #    @+   #    @ @     *#*  
;    #* :$  +*      #   *#   #* :$          $+  +#     #    #   *#   #: -$     #    #-  +#    $$$     # #  
;    *##$    +##$+  @*###*   *##$            $#@ ##  #####  @*###*   # #@    #####  *##$ ##    #      *#*  
;                                                                    #                        ++           
;                                                                   ###                      ###           

  (define test-display%
    (class object% ()
      
      (init-field (drscheme-frame #f))
      (init-field (current-tab #f))
      
      (define/public (pop-up-window test-results)
        (let* ((curr-win (send current-tab get-test-window))
               (window 
                (if curr-win
                    curr-win
                    (make-object test-window%)))
               (content (make-object (editor:standard-style-list-mixin text%))))
          (fill-in content test-results)
          (send content lock #t)
          (send window update-editor content)
          (send current-tab current-test-editor content)
          (unless curr-win
            (send current-tab current-test-window window)
            (send drscheme-frame register-test-window window)
            (send window update-switch 
                  (lambda () (send drscheme-frame dock-tests)))
            (send window update-disable
                  (lambda () (send current-tab update-test-preference #f)))
            (send window update-closer
                  (lambda()
                    (send drscheme-frame deregister-test-window window)
                    (send current-tab current-test-window #f)
                    (send current-tab current-test-editor #f))))
          (if (get-preference 'profj:test-window:docked? 
                              (lambda () (put-preferences '(profj:test-window:docked?) '(#f)) #f))
              (send drscheme-frame display-test-panel content)
              (send window show #t))))
      
      (define/private (fill-in editor test-results)
        (let-values (((tested-classes covered nearly-tested-classes total-tests 
                                      failed-tests total-checks failed-checks)
                      (send test-results provide-test-results)))
          (letrec ((insert-content
                    (lambda (source nextline?) 
                      (let loop ((contents source))
                        (unless (null? contents)
                          (send editor insert (car contents))
                          (when nextline? (next-line))
                          (loop (cdr contents))))))
                   (next-line (lambda ()
                                (send editor insert "\n   "))))
            
            (unless (= 0 total-tests)
              (send editor insert (format "Ran ~a total tests\n" total-tests))
              (if (= 0 failed-tests)
                  (send editor insert "All tests passed!\n\n")
                  (send editor insert (format "~a of ~a tests failed. See below for details.\n\n"
                                              failed-tests total-tests))))
            (unless (= 0 total-checks)
              (send editor insert (format "Ran ~a total checks\n" total-checks))
              (if (= 0 failed-checks)
                  (send editor insert "All checks passed!\n\n")
                  (send editor insert (format "~a of ~a checks failed. See below for details.\n\n"
                                              failed-checks total-checks))))
            (unless (null? covered)
              (make-covered-button covered editor #f)
              (send editor insert "\n"))
            
            (if (testcase-ext?)
                (send editor insert "Ran the following tests:\n")
                (send editor insert "Tested the following Example classes:\n"))
            (for-each
             (lambda (test-info)
               (send editor insert "\n")
               (send editor insert (single-test-name test-info))
               (unless (null? (single-test-covered-exprs test-info))
                 (make-covered-button (single-test-covered-exprs test-info) editor #t))
               (unless (null? (single-test-testcases test-info))
                 (let ((num-tests (length (single-test-testcases test-info)))
                       (failed-tests (filter (compose not testcase-passed?)
                                             (single-test-testcases test-info))))
                   (next-line)
                   (send editor insert (format "Ran ~a test methods." num-tests))
                   (next-line)
                   (if (null? failed-tests)
                       (send editor insert "All tests passed!")
                       (send editor insert (format "~a of ~a tests failed:"
                                                   (length failed-tests) num-tests)))
                   (next-line)
                   (for-each (lambda (test)
                               (send editor insert 
                                     (format "~a ~a" (testcase-name test)
                                             (if (testcase-passed? test) "succeeded!" "failed.")))
                               (unless (null? (testcase-covers test))
                                 (make-covered-button (testcase-covers test) editor #f))
                               (next-line))
                             (reverse (single-test-testcases test-info)))))
               (unless (null? (single-test-covered-methods test-info))
                 (next-line)
                 (send editor insert "Tested the following classes:")
                 (next-line)
                 (for-each (lambda (class)
                             (let ((num-methods (length (car (cdr class))))
                                   (uncovered-methods (filter (lambda (m) (not (car (cdr m)))) (car (cdr class)))))
                               (send editor insert (format "class ~a with ~a of its methods covered."
                                                           (car class)
                                                           (cond
                                                             ((null? uncovered-methods) "all")
                                                             ((= (length uncovered-methods) num-methods) "none")
                                                             (else
                                                              (- num-methods (length uncovered-methods))))))
                               (next-line)
                               (let loop ((methods uncovered-methods))
                                 (unless (null? methods)
                                   (send editor insert (format "Method ~a was not fully covered." 
                                                               (car (car methods))))
                                   (next-line)
                                   (loop (cdr methods))))))
                           (single-test-covered-methods test-info)))
                                                                
               (when (> (single-test-num-checks test-info) 0)
                 (next-line)
                 (send editor insert (format "Ran ~a checks." (single-test-num-checks test-info)))
                 (next-line)
                 (if (null? (single-test-failed-checks test-info))
                     (send editor insert "All checks succeeded!\n")
                     (begin
                       (send editor insert (format "~a of ~a checks failed:"
                                                   (length (single-test-failed-checks test-info))
                                                   (single-test-num-checks test-info)))
                       (next-line)
                       (for-each (lambda (check)
                                   (make-link editor (failed-check-msg check) 
                                              (failed-check-src check))
                                   (next-line))
                                 (reverse (single-test-failed-checks test-info)))))
                 ))
             tested-classes)
            (unless (null? nearly-tested-classes)
              (send editor insert "\n")
              (send editor insert "The following classes were not run, but are similar to example classes:\n")
              (insert-content nearly-tested-classes #f)))))
        (super-instantiate ())))
  
  (define test-window%
    (class frame% ()
      
      (super-instantiate 
          ((string-constant profj-test-results-window-title) #f 400 350))
      
      (define editor #f)
      (define switch-func void)
      (define disable-func void)
      (define close-cleanup void)
      
      (define content
        (make-object editor-canvas% this #f '(auto-vscroll)))
      
      (define button-panel (make-object horizontal-panel% this
                             '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))
      
      (define buttons
        (list (make-object button% 
                (string-constant close)
                button-panel
                (lambda (b c)
                  (when (eq? 'button (send c get-event-type))
                    (close-cleanup)
                    (send this show #f))))
              (make-object button%
                (string-constant profj-test-results-close-and-disable)
                button-panel
                (lambda (b c)
                  (when (eq? 'button (send c get-event-type))
                    (disable-func)
                    (close-cleanup)
                    (send this show #f))))
              (make-object button%
                (string-constant dock)
                button-panel
                (lambda (b c)
                  (when (eq? 'button (send c get-event-type))
                    (send this show #f)
                    (put-preferences '(profj:test-window:docked?) '(#t))
                    (switch-func))))
              (make-object grow-box-spacer-pane% button-panel)))
              
      
      (define/public (update-editor e)
        (set! editor e)
        (send content set-editor editor))
      
      (define/public (update-switch thunk)
        (set! switch-func thunk))
      (define/public (update-closer thunk)
        (set! close-cleanup thunk))
      (define/public (update-disable thunk)
        (set! disable-func thunk))
      ))
  
  (define test-panel%
    (class vertical-panel% ()

      (inherit get-parent)
      
      (super-instantiate () )

      (define content (make-object editor-canvas% this #f '()))
      (define button-panel (make-object horizontal-panel% this
                             '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))
      (define (hide)
        (let ((current-tab (send frame get-current-tab)))
          (send frame deregister-test-window 
                (send current-tab get-test-window))
          (send current-tab current-test-window #f)
          (send current-tab current-test-editor #f))
        (remove))

      (make-object button%
        (string-constant hide)
        button-panel
        (lambda (b c)
          (when (eq? 'button (send c get-event-type))
            (hide))))
      (make-object button%
        (string-constant profj-test-results-hide-and-disable)
        button-panel
        (lambda (b c)
          (when (eq? 'button (send c get-event-type))
            (hide)
            (send (send frame get-current-tab) update-test-preference #f))))
      (make-object button%
        (string-constant undock)
        button-panel
        (lambda (b c)
          (when (eq? 'button (send c get-event-type))
            (put-preferences '(profj:test-window:docked?) '(#f))
            (send frame undock-tests)
            )))
      
      (define/public (update-editor e)
        (send content set-editor e))
      
      (define frame #f)
      (define/public (update-frame f)
        (set! frame f))
      
      (define/public (remove)
        (let ((parent (get-parent)))
          (put-preferences '(profj:test-dock-size) (list (send parent get-percentages)))
          (send parent delete-child this)))
      ))
      
  
;   ##@ $##    #                   
;    #@ ##                         
;    #$&$#   :##    *###$#   $##*# 
;    #*@+#     #    #$  -#  $$  :# 
;    # # #     #    *###$   #      
;    #   #     #        +#  #      
;    #   #     #    #   *#  $+  :$ 
;   ### ###  #####  @*###*   $##$  
  
  ;make-link: text% (listof (U string snip%)) src -> void
  (define (make-link text msg dest)
    (for-each (lambda (m) 
                (when (is-a? m snip%)
                  (send m set-style (send (send text get-style-list)
                                          find-named-style "Standard")))
                (send text insert m)) msg)
    (let ((start (send text get-end-position)))
      (send text insert (format-src dest))
      (send text set-clickback 
            start (send text get-end-position)
            (lambda (t s e)
              (open-and-highlight-in-file dest))
            #f #f)
      (let ((end (send text get-end-position))
            (c (new style-delta%)))
        (send text insert " ")
        (send text change-style (make-object style-delta% 'change-underline #t)
              start end #f)
        (send c set-delta-foreground "royalblue")
        (send text change-style c start end #f))))
    
  (define (open-and-highlight-in-file srcloc)
    (let* ([position (src-pos srcloc)]
           [span (src-span srcloc)]
           [rep/ed (get-editor srcloc #t)])
      (when rep/ed
        (cond
          [(is-a? (cadr rep/ed) text:basic<%>)
           (let ((highlight
                  (lambda ()
                    (send (car rep/ed) highlight-error (cadr rep/ed) position (+ position span)))))
             (queue-callback highlight))]))))
  
  (define (make-covered-button covered dest partial?)
    (send dest insert " ")
    (let* ((editor (new (editor:standard-style-list-mixin text%)
                        [auto-wrap #t]))
           (snip (new editor-snip% (editor editor)
                      (with-border? #t)))
           (start (send dest get-end-position)))
      (send snip set-style
            (send (send dest get-style-list) find-named-style "Standard"))
      (if partial?
          (send editor insert "Show covered expressions")
          (send editor insert "Show all covered expressions"))
      (send dest insert snip)
      (send dest insert " ")
      (send editor set-clickback
            0 (send editor get-end-position)
            (lambda (t s e)
              (color-covered covered))
            #f #f)
      (let ((c (new style-delta%)))
        (send c set-delta-foreground "royalblue")
        (send dest change-style c start (sub1 (send dest get-end-position)) #f))
      ))
  
  (define (color-covered covered)
    (unless (null? covered)
      (let* ([editor (get-editor (car covered) #f)]
             [style-list (editor:get-standard-style-list)]
             [uncover-color (send style-list find-named-style "profj:syntax-coloring:scheme:uncovered")]
             [cover-color (send style-list find-named-style "profj:syntax-coloring:scheme:covered")])
        (when editor
          ;(send cover-color set-delta-foreground "darkmagenta")
          ;(send uncover-color set-delta-foreground "black")
          (letrec ((color-buff
                    (lambda ()
                      (cond
                        ((or (send editor is-locked?) (send editor in-edit-sequence?))
                         (queue-callback color-buff))
                        (else
                         (unless (send editor test-froze-colorer?)
                           (send editor freeze-colorer)
                           (send editor toggle-test-status))
                         (send editor begin-test-color)
                         (send editor change-style uncover-color 0 (send editor last-position) #f)
                         (let loop ((srcs covered))
                           (unless (null? srcs)
                             (send editor change-style cover-color (sub1 (src-pos (car srcs)))
                                   (sub1 (+ (src-pos (car srcs))
                                            (src-span (car srcs)))) #f)
                             (loop (cdr srcs))))
                         (send editor end-test-color))))))
            (queue-callback color-buff))))))
  
  (define (get-editor src rep?)
    (let* ([source (src-file src)]
           [frame (cond
                    [(path? source) (handler:edit-file source)]
                    [(is-a? source editor<%>)
                     (let ([canvas (send source get-canvas)])
                       (and canvas
                            (send canvas get-top-level-window)))])]
           [editor (cond
                     [(path? source)
                      (cond
                        [(and frame (is-a? frame #;drscheme:unit:frame<%>))
                         (send frame get-definitions-text)]
                        [(and frame (is-a? frame frame:editor<%>))
                         (send frame get-editor)]
                        [else #f])]
                     [(is-a? source editor<%>) source])]
           [rep (and frame 
                     #;(is-a? frame drscheme:unit:frame%)
                     (send frame get-interactions-text))])
      (when frame 
        (unless (send frame is-shown?) (send frame show #t)))
      (if (and rep? rep editor) 
          (list rep editor)
          (and rep editor))))
  
  (define (format-src src)
    (string-append (cond
                     ((path? (src-file src)) (string-append "in " (src-file src) " at "))
                     ((is-a? (src-file src) editor<%>) "at "))
                   "line " (number->string (src-line src))
                   " column " (number->string (src-col src))))
;                                                                  
;   ####*            $#@*#          ######                         
;    # -#*          @   :#           #   #           #             
;    #   #  ## $#$  @+               # #    ##  ##  #####   *###$# 
;    #   #   #$* :   $@##            ###     $  $    #      #$  -# 
;    #   #   #          +$           # #      $$     #      *###$  
;    #   #   #           #           #        $$     #          +# 
;    #  @*   #      #$+ :$           #   #   $  $    #* :$  #   *# 
;   ####*   #####   #*@#$           ######  ##  ##   *##$   @*###* 
;                                                                  
  
  (define-local-member-name toggle-test-status test-froze-colorer? begin-test-color end-test-color)
  
  (define test-tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define (test-definitions-text%-mixin %)
        (class % ()        
          (inherit begin-edit-sequence end-edit-sequence)

          (define colorer-frozen-by-test? #f)
          (define/public (test-froze-colorer?) colorer-frozen-by-test?)
          (define/public (toggle-test-status)
            (set! colorer-frozen-by-test?
                  (not colorer-frozen-by-test?)))
          
          (define/public (begin-test-color)
            (begin-edit-sequence #f))
          (define/public (end-test-color)
            (end-edit-sequence))
          
          (define/augment (on-delete start len)
            (begin-edit-sequence)
            (inner (void) on-delete start len))
          (define/augment (after-delete start len)
            (inner (void) after-delete start len)
            (when colorer-frozen-by-test?
              (send this thaw-colorer)
              (send this toggle-test-status))
            (end-edit-sequence))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (inner (void) after-insert start len)
            (when colorer-frozen-by-test?
              (send this thaw-colorer)
              (send this toggle-test-status))
            (end-edit-sequence))
          
          (super-instantiate ())))
      
      (define (test-frame-mixin %)
        (class % ()

          (inherit get-current-tab)
                    
          (define/public (display-test-panel editor)
            (send test-panel update-editor editor)
            (unless (send test-panel is-shown?)
              (send test-frame add-child test-panel)
              (let ((test-box-size
                     (get-preference 'profj:test-dock-size (lambda () '(2/3 1/3)))))
                (send test-frame set-percentages test-box-size))
              ))
          (define test-panel null)
          (define test-frame null)
          
          (define test-windows null)
          (define/public (register-test-window t)
            (set! test-windows (cons t test-windows)))
          (define/public (deregister-test-window t)
            (set! test-windows (remq t test-windows)))
          
          (define/public (dock-tests)
            (for-each (lambda (t) (send t show #f)) test-windows)
            (let ((ed (send (get-current-tab) get-test-editor)))
              (when ed (display-test-panel ed))))
          (define/public (undock-tests)
            (send test-panel remove)
            (for-each (lambda (t) (send t show #t)) test-windows))
          
          (define/override (make-root-area-container cls parent)
            (let* ([outer-p (super make-root-area-container panel:vertical-dragable% parent)]
                   [louter-panel (make-object vertical-panel% outer-p)]
                   [test-p (make-object test-panel% outer-p '(deleted))]
                   [root (make-object cls louter-panel)])
              (set! test-panel test-p)
              (send test-panel update-frame this)
              (set! test-frame outer-p)
              root))
          
          (define/augment (on-tab-change from-tab to-tab)
            (let ((test-editor (send to-tab get-test-editor))
                  (panel-shown? (send test-panel is-shown?))
                  (dock? (get-preference 'profj:test-window:docked? (lambda () #f))))
              (cond
                ((and test-editor panel-shown? dock?)
                 (send test-panel update-editor test-editor))
                ((and test-editor dock?)
                 (display-test-panel test-editor))
                ((and panel-shown? (not dock?))
                 (undock-tests))
                (panel-shown? (send test-panel remove)))
              (inner (void) on-tab-change from-tab to-tab)))
                
          (super-instantiate () )))
      
      (define (test-tab%-mixin %)
        (class % ()
          
          (inherit get-frame get-defs)
          
          (define test-editor #f)
          (define/public (get-test-editor) test-editor)
          (define/public (current-test-editor ed)
            (set! test-editor ed))
          
          (define test-window #f)
          (define/public (get-test-window) test-window)
          (define/public (current-test-window w)
            (set! test-window w))
          
          (define/public (update-test-preference test?)
            (let* ([language-settings
                    (preferences:get
                     (drscheme:language-configuration:get-settings-preferences-symbol))]
                   [language
                    (drscheme:language-configuration:language-settings-language
                     language-settings)]
                   [settings
                    (drscheme:language-configuration:language-settings-settings
                     language-settings)])
              (when (object-method-arity-includes? language 'update-test-setting 2)
                (let ((next-setting (drscheme:language-configuration:make-language-settings 
                                     language
                                     (send language update-test-setting settings test?))))
                  (preferences:set
                   (drscheme:language-configuration:get-settings-preferences-symbol)
                   next-setting)
                  (send (get-defs) set-next-settings next-setting)))))
          
          (define/augment (on-close)
            (when test-window
              (when (send test-window is-shown?)
                (send test-window show #f))
              (send (get-frame) deregister-test-window test-window))
            (inner (void) on-close))
          
          (super-instantiate () )))
      
      (drscheme:get/extend:extend-definitions-text test-definitions-text%-mixin)
      (drscheme:get/extend:extend-unit-frame test-frame-mixin)
      (drscheme:get/extend:extend-tab test-tab%-mixin)
          
      ))

  )
