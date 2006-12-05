
(module gui mzscheme
  
  (require
   (lib "tool.ss" "drscheme")
   (lib "unit.ss")
   (lib "list.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix fw: (lib "framework.ss" "framework"))
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   (prefix bit: (lib "bitmap-label.ss" "mrlib"))
   
   (prefix cst: "constants.ss")
   (prefix sba: "constraints-gen-and-prop.ss")
   (prefix err: "sba-errors.ss")
   (prefix saa: "snips-and-arrows.ss")
   )
  
  (provide tool@)
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^) 
      ; INTERFACE WITH LANGUAGES
      (define mrflow-language-extension-interface<%>
        (interface ()
          render-value-set
          get-mrflow-primitives-filename))
      
      (define (mrflow-default-implementation-mixin super%)
        (class* super% (mrflow-language-extension-interface<%>)
          
          ; type -> string
          ; Language implementors are responsible for providing a type pretty-printer.
          ; XXX NOT CURRENTLY USED
          (define/public (render-value-set val) "render-value-set-mixin not implemented")
          
          ; -> string
          ; Language implementors are responsible for providing the name of the file
          ; that contains the types of the primitives for their language. If they don't,
          ; we give a warning, use R5RS, and hope for the best.
          (define/public (get-mrflow-primitives-filename)
            (message-box (strcst:string-constant mrflow-using-default-language-title)
                         (strcst:string-constant mrflow-using-default-language)
                         #f '(ok))
            (build-path (collection-path "mrflow")
                        "primitives"
                        "r5rs.ss"))
          
          (super-instantiate ())))
      
      (define (phase1) 
        (drscheme:language:extend-language-interface mrflow-language-extension-interface<%>
                                                     mrflow-default-implementation-mixin)
        (drscheme:unit:add-to-program-editor-mixin saa:extend-all-editors-mixin))
      
      (define (phase2) cst:void)
      
      
      (define mrflow-bitmap
        (bit:bitmap-label-maker
         (strcst:string-constant mrflow-button-title)
         (build-path (collection-path "icons") "mrflow-small.bmp")))
      
      
      ; TERM AND SNIP STYLES
      (define can-click-style-delta (make-object style-delta% 'change-weight 'bold))
      (send can-click-style-delta set-delta-foreground "purple")
      
      (define green-style-delta (make-object style-delta% 'change-weight 'bold))
      (send green-style-delta set-delta-foreground "green")
      (send green-style-delta set-underlined-on #t)
      
      (define orange-style-delta (make-object style-delta% 'change-weight 'bold))
      (send orange-style-delta set-delta-foreground "orange")
      (send orange-style-delta set-underlined-on #t)
      
      (define red-style-delta (make-object style-delta% 'change-weight 'bold))
      (send red-style-delta set-delta-foreground "red")
      (send red-style-delta set-underlined-on #t)
      
      ; symbol style-delta% -> style-delta%
      ; compares two style-deltas (one represented as a color/severity name, the other one as
      ; an actual style-delta) and returns the most "urgent" one.
      ; red > orange > green
      (define (max-style-delta-by-name style-delta-name style-delta)
        (case style-delta-name
          [(red) red-style-delta]
          [(orange) (if (eq? style-delta red-style-delta) style-delta orange-style-delta)]
          [(green) style-delta]
          [else (error 'max-style-delta-by-name
                       "MrFlow internal error; incomparable style-delta ~a"
                       style-delta-name)]))
      
      ; sba-state  label -> style-delta%
      ; If the label has errors associated with it, we color the term with the color
      ; of the worst error, otherwise we color it with the normal clickable style-delta.
      (define (get-style-delta-from-label sba-state label)
        (let ([errors (sba:get-errors-from-label sba-state label)])
          (if (null? errors)
              can-click-style-delta
              (foldl (lambda (sba-error current-max-style-delta)
                       (max-style-delta-by-name (err:sba-error-gravity sba-error) current-max-style-delta))
                     green-style-delta
                     errors))))
      
      ; sba-state label -> exact-non-negative-integer
      ; span conversation: for all graphical purposes, the span of a compound expression is 1,
      ; to highlight only the opening parenthesis. Otherwise we might highlight subexpressions
      ; with the wrong color.
      (define (get-span-from-label sba-state label)
        (if (or (sba:is-label-atom? label)
                );(not (null? (sba:get-errors-from-label sba-state label))))
            (sba:get-span-from-label label)
            1))
      
      ; (listof (cons symbol string))
      ; Lists the possible snip types and their corresponding colors.
      ; For a given term that has snips of several different types, the snips will be
      ; ordered from left to right in the editor in the same order as their types appear
      ; in this list.
      (define snip-types-and-colors
        '((type . "blue")
          (error . "red")))
      
      
      ; INTERFACE FOR MENUS
      ; symbol symbol -> string
      ; given a snip type and a menu action for snips (show/hide), return the corresponding
      ; menu text
      (define (get-menu-text-from-snip-type type action)
        (case type
          [(type)
           (case action
             [(show) (strcst:string-constant mrflow-popup-menu-show-type)]
             [(hide) (strcst:string-constant mrflow-popup-menu-hide-type)]
             [else (error 'get-menu-text-from-type "MrFlow internal error; unknown type action: ~a" action)])]
          [(error)
           (case action
             [(show) (strcst:string-constant mrflow-popup-menu-show-errors)]
             [(hide) (strcst:string-constant mrflow-popup-menu-hide-errors)]
             [else (error 'get-menu-text-from-type "MrFlow internal error; unknown error action: ~a" action)])]
          [else (error 'get-menu-text-from-type "MrFlow internal error; unknown type: ~a" type)]))
      
      ; sba-state symbol label -> (listof string)
      ; given a snip type and a lable, returns the content of the snips to be
      ; added for that type and label.
      (define (get-snip-text-from-snip-type sba-state type label)
        (case type
          [(type) (list (sba:pp-type sba-state (sba:get-type-from-label sba-state label) 'gui))]
          [(error) (map err:sba-error-message (sba:get-errors-from-label sba-state label))]))
      
      
      ; DEFINITION WINDOW MIXIN
      (drscheme:get/extend:extend-definitions-text saa:extend-top-editor-mixin)
      
      (drscheme:get/extend:extend-tab
       (mixin (drscheme:rep:context<%> drscheme:unit:tab<%>) ()
         (inherit get-defs)
         (define/augment (clear-annotations)
           (inner (void) clear-annotations)
           (send (get-defs) remove-all-snips-and-arrows-and-colors))
         (super-new)))
      
      ; UNIT FRAME MIXIN
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super%
           ; -> void
           (define/augment (enable-evaluation)
             (inner cst:void enable-evaluation)
             (send analyze-button enable #t))
           
           ; -> void
           (define/augment (disable-evaluation)
             (inner cst:void disable-evaluation)
             (send analyze-button enable #f))
           
           (super-instantiate ())
           
           
           (inherit get-button-panel get-definitions-text get-interactions-text get-current-tab)
           (define analyze-button
             (instantiate
                 button% ()
               (label (mrflow-bitmap this))
               (parent (get-button-panel))
               (callback
                (lambda (button event)
                  (let ([start-time (current-milliseconds)]
                        [definitions-text (get-definitions-text)]
                        [current-tab (get-current-tab)]
                        [drs-eventspace (current-eventspace)]
                        [interactions-text (get-interactions-text)]
                        [language-settings
                         (fw:preferences:get
                          (drscheme:language-configuration:get-settings-preferences-symbol))])
                    (letrec-values
                        ([(user-change-terms register-label-with-gui)
                          (saa:init-snips-and-arrows-gui
                           definitions-text
                           sba:get-source-from-label
                           sba:get-mzscheme-position-from-label
                           (lambda (label) (get-span-from-label sba-state label))
                           sba:get-arrows-from-labels
                           (lambda (label) (get-style-delta-from-label sba-state label))
                           (lambda (menu labels) cst:void)
                           get-menu-text-from-snip-type
                           (lambda (type label) (get-snip-text-from-snip-type sba-state type label))
                           snip-types-and-colors
                           #t)
                          ; snips-and-arrows library testing...
;                          (saa:init-snips-and-arrows-gui
;                           definitions-text
;                           sba:get-source-from-label
;                           sba:get-mzscheme-position-from-label
;                           (lambda (label) (get-span-from-label sba-state label))
;                           sba:get-arrows-from-label
;                           (lambda (label) (get-style-delta-from-label sba-state label))
;                           (lambda (menu labels)
;                             (let* ([new-name-callback
;                                     (lambda (item event)
;                                       (let ([new-name
;                                              (fw:keymap:call/text-keymap-initializer
;                                               (lambda ()
;                                                 (get-text-from-user
;                                                  "rename"
;                                                  "rename")))]
;                                             [terms (append
;                                                     (map
;                                                      (lambda (arrow-info)
;                                                        (cons (car arrow-info) "foo"))
;                                                      (sba:get-arrows-from-labels labels))
;                                                     (map
;                                                      (lambda (arrow-info)
;                                                        (cons (cadr arrow-info) "foo"))
;                                                      (sba:get-arrows-from-labels labels))
;                                                     )])
;                                         (user-change-terms terms)))])
;                               (make-object menu-item%
;                                 (strcst:string-constant cs-rename-id)
;                                 menu
;                                 new-name-callback)))
;                           get-menu-text-from-snip-type
;                           (lambda (type label) (get-snip-text-from-snip-type sba-state type label))
;                           snip-types-and-colors
;                           #f)
                          ]
                         [(sba-state) (sba:make-sba-state register-label-with-gui)])
                      ; disable-evaluation will lock the editor, so hopefully all the other tools
                      ; unlock the editor to clear their crap (note that the second call below
                      ; is a call to the superclass, so remove-all-snips-and-arrows-and-colors
                      ; is not called here, but is called internally inside
                      ; init-snips-and-arrows-gui
                      (disable-evaluation)
                      ;(send current-tab clear-annotations)
                      
                      ; note: we have to do this each time, because the user might have changed
                      ; the language between analyses.
                      (let* ([language-object (drscheme:language-configuration:language-settings-language
                                               language-settings)]
                             [primitive-table-file (send language-object get-mrflow-primitives-filename)])
                        (if (file-exists? primitive-table-file)
                            (begin
                              (sba:initialize-primitive-type-schemes sba-state primitive-table-file)
                              (drscheme:eval:expand-program
                               (drscheme:language:make-text/pos definitions-text
                                                                0
                                                                (send definitions-text last-position))
                               language-settings
                               #t
                               ; set current-directory and current-load-relative-directory before expansion
                               (lambda ()
                                 (let* ([tmp-b (box #f)]
                                        [fn (send definitions-text get-filename tmp-b)])
                                   (unless (unbox tmp-b)
                                     (when fn
                                       (let-values ([(base name dir?) (split-path fn)])
                                         (current-directory base)
                                         (current-load-relative-directory base))))))
                               void
                               (lambda (syntax-object-or-eof iter)
                                 (if (eof-object? syntax-object-or-eof)
                                     (parameterize ([current-eventspace drs-eventspace])
                                       (queue-callback
                                        (lambda () ; =drs=
                                          (let ([sba-end-time (current-milliseconds)])
                                            ;(printf "sba time: ~a ms~n" (- (current-milliseconds) start-time))
                                            (sba:check-primitive-types sba-state)
                                            ;(printf "check time: ~a ms~n" (- (current-milliseconds) sba-end-time))
                                            )
                                          ; color everything right before re-enabling buttons
                                          (send definitions-text color-registered-labels)
                                          (enable-evaluation)
                                          )))
                                     (begin
                                       (parameterize ([current-eventspace drs-eventspace])
                                         (queue-callback
                                          (lambda () ; =drs=
                                            ;(printf "syntax: ~a~n" (syntax-object->datum syntax-object-or-eof))
                                            (sba:create-label-from-term sba-state syntax-object-or-eof '() #f))))
                                       ; must be outside the parameterize so the next expansion occurs
                                       ; in the right eventspace...
                                       (iter))))))
                            ; get-mrflow-primitives-filename defaults to R5RS
                            ; (see mrflow-default-implementation-mixin above), so if we arrive here,
                            ; we know we are in trouble because it means no primitive table is
                            ; defined for the current language and we couldn't even find the table
                            ; for the R5RS primitives.
                            (error 'analyze-button-callback
                                   "MrFlow internal error; R5RS primitive types file ~a doesn't exist."
                                   primitive-table-file)))))))))
           
           (send (get-button-panel) change-children
                 (lambda (button-list)
                   (cons analyze-button (remq analyze-button button-list))))
           ) ; class
         )) ; drscheme:get/extend:extend-unit-frame
      
      )) ; tool@ unit/sig
  ); module
