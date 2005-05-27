
(module snips-and-arrows (lib "mrflow.ss" "mrflow")
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   
   (prefix cst: "constants.ss")
   (prefix saav: "snips-and-arrows-view.ss")
   "labels.ss"
   )
  
  (provide/contract
   (extend-all-editors-mixin mixin-contract)
   (extend-top-editor-mixin mixin-contract)
   (init-snips-and-arrows-gui (text%?
                               (label? . -> . text%?)
                               (label? . -> . non-negative-exact-integer?)
                               (label? . -> . non-negative-exact-integer?)
                               ((listof label?) . -> . (listof (list/c label? label? string?)))
                               (label? . -> . style-delta%?)
                               ((is-a?/c popup-menu%) (listof label?) . -> . void?)
                               (symbol? symbol? . -> . string?)
                               (symbol? label? . -> . (listof string?))
                               (listof (cons/c symbol? string?))
                               boolean?
                               . -> .
                               (values ((listof (cons/c label? string?)) . -> . void?)
                                       (label? . -> . void?))))
   (init-snips-and-arrows-gui-for-syntax-objects (text%?
                                                  ((listof syntax?) . -> . (listof (list/c syntax? syntax? string?)))
                                                  (syntax? . -> . style-delta%?)
                                                  ((is-a?/c popup-menu%) (listof syntax?) . -> . void?)
                                                  (symbol? symbol? . -> . string?)
                                                  (symbol? syntax? . -> . (listof string?))
                                                  (listof (cons/c symbol? string?))
                                                  boolean?
                                                  . -> .
                                                  (values ((listof (cons/c syntax? string?)) . -> . void?)
                                                          (syntax? . -> . void?))))
   )
  
  (define-struct gui-state (; gui-view-state
                            gui-view-state
                            ; ((listof label) -> (listof (list label label string)))
                            get-arrows-from-labels
                            ; (symbol symbol -> string)
                            get-menu-text-from-snip-type
                            ; (symbol label -> (listof string))
                            get-snip-text-from-snip-type-and-label
                            ; (popup-menu% (listof label) -> void)
                            extend-menu-for-labels
                            ; (union #f (listof label))
                            previous-labels
                            ; boolean
                            ; we need this one to prevent arrows and menus to show up
                            ; before the real analysis part is over, because as long as
                            ; the analysis is not finished we might not have all arrows
                            ; and not all errors (so wrong menus).
                            term-analysis-done?
                            ))
  
  ; MENUS
  ; gui-state menu% (listof labels) symbol text% -> void
  ; creates a menu entry for a given snip type
  ; all labels correspond to the same term (because of macros)
  (define (create-snips-menu-item-by-type gui-state menu labels type editor)
    (let ([gui-view-state (gui-state-gui-view-state gui-state)]
          [get-menu-text-from-snip-type (gui-state-get-menu-text-from-snip-type gui-state)]
          [get-snip-text-from-snip-type-and-label
           (gui-state-get-snip-text-from-snip-type-and-label gui-state)])
      (if (ormap (lambda (label)
                   (saav:label-has-snips-of-this-type? gui-view-state label type))
                 labels)
          ; at least one label has snips displayed => delete menu entry
          (make-object menu-item%
            (get-menu-text-from-snip-type type 'hide)
            menu
            (lambda (item event)
              (for-each (lambda (label)
                          (when (saav:label-has-snips-of-this-type? gui-view-state label type)
                            (saav:remove-inserted-snips gui-view-state label type editor)))
                        labels)))
          ; no label has snips displayed => show menu entry if one of them has snips associated
          ; with it
          (unless (andmap (lambda (label)
                            (null? (get-snip-text-from-snip-type-and-label type label)))
                          labels)
            (make-object menu-item%
              (get-menu-text-from-snip-type type 'show)
              menu
              (lambda (item event)
                (for-each (lambda (label)
                            (saav:add-snips gui-view-state label type editor))
                          labels))))))
    cst:void)
  
  ; gui-state menu% (listof label) -> menu-item%
  ; create menu entries for arrows
  ; all labels correspond to the same term (because of macros)
  (define (create-arrow-menu-items gui-state menu labels)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [arrows-info ((gui-state-get-arrows-from-labels gui-state) labels)]
           [max-arrows (length arrows-info)]
           [tacked-arrows (apply + (map (lambda (label)
                                          (saav:get-tacked-arrows-from-label gui-view-state label))
                                        labels))])
      (when (< tacked-arrows max-arrows)
        (make-object menu-item%
          (strcst:string-constant snips-and-arrows-popup-menu-tack-all-arrows)
          menu
          (lambda (item event)
            ; remove all (possibly untacked) arrows and add all arrows, tacked.
            ; we could just add the untacked ones, but what we do here is simple
            ; and efficient enough
            (for-each (lambda (label)
                        (saav:remove-arrows gui-view-state label 'all #t))
                      labels)
            (for-each
             (lambda (arrow-info)
               (saav:add-arrow gui-view-state arrow-info #t))
             arrows-info)
            (saav:invalidate-bitmap-cache gui-view-state))))
      (when (> tacked-arrows 0)
        (make-object menu-item%
          (strcst:string-constant snips-and-arrows-popup-menu-untack-all-arrows)
          menu
          (lambda (item event)
            (for-each (lambda (label)
                        (saav:remove-arrows gui-view-state label 'all #t))
                      labels)
            (saav:invalidate-bitmap-cache gui-view-state))))))
  
  
  ; gui-view-state -> boolean
  ; User insertions cause problems: a user might insert something while our snips
  ; are present.  That would force us to remove all the snips, since as soon as
  ; the user changes the program the results of the analysis become invalid.  So
  ; we would have to keep track of the user insertion (which is possible), update
  ; the position of all our snips accordingly (which is possible too), then delete
  ; all the snips because they would not be valid anymore (which is very possible).
  ; In fact we used to do all that.  The reason we got rid of it is because it does
  ; not interact well with the undo feature:  if, after the user insertion and the
  ; automatic removal of snips, the user does an undo, the undo might delete random
  ; stuff at the position where the user insertion initially occured, but that might
  ; not be the position where that user-inserted stuff currently is, because removing
  ; the snips between the insertion and the undo might have moved around the stuff
  ; that was inserted...
  ;
  ; Note that it's not possible to delete our snips just right before the user action
  ; is effected in the window (e.g. during a call to the on-insert method),
  ; because the editor is locked at that time (and with reason: if we were to remove
  ; the snips right after the user acts (which is the thing that decides we must
  ; get rid of all our snips) but just before the action actually takes place in the
  ; editor, then after removing the snips the user action would actually be effected
  ; at the wrong position in the editor - i.e. we can't sweep the rug under DrScheme's
  ; own insertion mechanism, and I don't think Matthew would be willing to add a mechanism
  ; whereby one could notify DrScheme that the rug is being swept...)
  ;
  ; Same problem with trying to remove the snips inside can-insert? : the editor is
  ; locked.
  ;
  ; Note also that things get even worse if the user tries to delete stuff instead of
  ; inserting stuff: the user might try to delete one of our own snips!  We could
  ; check the stuff the user wants to delete and only allow the delete if the stuff
  ; didn't contain one of our snips, but this still wouldn't solve the undo problem
  ; (which exists in reverse: deleting and undoing would re-insert the deleted stuff
  ; at the wrong place - I tried it!).
  ;
  ; Conclusion: it's impossible to solve the problem of user insertion and deletion
  ; while snips are present, because the undo then becomes buggy.  So we simply
  ; completely disallow user insertions and deletions while snips are present (in
  ; this editor - there's no problem with undo if the user action happens in another
  ; editor that doesn't contain snips, and then we just use that as a signal to delete
  ; all snips in all editors using the after-user-action fucntion).
  ;
  ; So this is what this function is doing: disallow user modifications to an editor
  ; when the editor contains snips (or while the analysis is still running).
  (define (is-action-allowed? gui-view-state editor)
    (or (saav:analysis-currently-modifying? gui-view-state)
        (if (saav:snips-currently-displayed-in-editor? gui-view-state editor)
            (begin
              (message-box (strcst:string-constant snips-and-arrows-user-action-disallowed-title)
                           (strcst:string-constant snips-and-arrows-user-action-disallowed)
                           #f '(ok caution))
              #f)
            #t)))
  
  
  ; MIXINS
  ; to be applied to all editors and sub-editors containing registered labels
  (define extend-all-editors-mixin
    (lambda (super%)
      (class super%
        
        ; State initialization and resetting
        ; The state is created by the call to init-snips-and-arrows-gui in the callback
        ; of the tool's button.  The state is hidden inside the register-label-with-gui function
        ; returned by the call.  That means a new instance of the state is created each time
        ; the user uses the tool.  Then, each time the user uses register-label-with-gui,
        ; the function checks whether the editor has been seen before or not, and if it hasn't
        ; it calls the editor's initialize-snips-and-arrows-gui-state method to initialize the
        ; editor's state.  That ensures that all editors where coloring has to happen share the
        ; same state.  Note that the top editor has both extend-all-editors-mixin and
        ; extend-top-editor-mixin applied to it, so the initialize-snips-and-arrows-gui-state
        ; method is define/public in one case and define/override in the other case.
        ; Note also that the initialization of the top editor is always done
        ; as a special case inside init-snips-and-arrows-gui (see this function below)
        ; because that editor still needs to have access to the state to redraw arrows even if
        ; no label is registered for it.
        ;
        ; The state is reset in two cases:
        ; - the user inserts or deletes something in an editor (see the comment for
        ;   is-action-allowed? above for details about when this is allowed), and
        ;   clear-colors-immediately? is true
        ; - the gui makes a direct call to remove-all-snips-and-arrows-and-colors (probably inside
        ;   the clear-annotations method for the unit frame)
        ; The state is reseted by calling the reset-snips-and-arrows-state method of each editor
        ; for which a label has been registred.  Since the unit frame has no direct reference to
        ; the state but only through the register-label-with-gui function, and since the editors
        ; don't have any reference to the state after their reset-snips-and-arrows-state method
        ; is called, the state can be garbage collected as soon as the register-label-with-gui
        ; function is not referenced by the unit frame anymore.
        ; Note that it would be possible for the unit frame to re-use the state (and indeed that's
        ; how it was working for a while) but it makes testing whether the analysis is currently
        ; running a bit more difficult and doesn't make anything else any simpler.  Besides, it
        ; might also be a source of subtle errors if everything is not correctly reseted from one
        ; run of the analysis to the next one.
        
        ; (union gui-state symbol)
        (define gui-state 'uninitialized-gui-state-in-extend-all-editors-mixin)
        
        ; (union gui-view-state 'symbol)
        (define gui-view-state 'uninitialized-gui-view-state-in-extend-all-editors-mixin)
        
        ; gui-state -> void
        ; see the same method below for explanation
        (define/public (initialize-snips-and-arrows-gui-state new-gui-state)
          (set! gui-state new-gui-state)
          (set! gui-view-state (gui-state-gui-view-state new-gui-state)))
        
        ; -> void
        (define/public (reset-snips-and-arrows-state)
          (set! gui-state 'reinitialized-gui-state-in-extend-all-editors-mixin)
          (set! gui-view-state 'reinitialized-gui-view-state-in-extend-all-editors-mixin))
        
        ; exact-non-negative-integer exact-non-negative-integer -> boolean
        (define/augment (can-insert? start len)
          (and (or (symbol? gui-state)
                   (and (gui-state-term-analysis-done? gui-state)
                        (is-action-allowed? gui-view-state this)))
               (inner #t can-insert? start len)))
        
        ; exact-non-negative-integer exact-non-negative-integer -> boolean
        (define/augment (can-delete? start len)
          (and (or (symbol? gui-state)
                   (and (gui-state-term-analysis-done? gui-state)
                        (is-action-allowed? gui-view-state this)))
               (inner #t can-delete? start len)))
        
        ; exact-non-negative-integer exact-non-negative-integer -> void
        (define/augment (after-insert start len)
          (unless (or (symbol? gui-state)
                      (saav:analysis-currently-modifying? gui-view-state))
            (saav:after-user-action gui-view-state))
          (inner cst:void after-insert start len))
        
        ; exact-non-negative-integer exact-non-negative-integer -> void
        (define/augment (after-delete start len)
          (unless (or (symbol? gui-state)
                      (saav:analysis-currently-modifying? gui-view-state))
            (saav:after-user-action gui-view-state))
          (inner cst:void after-delete start len))
        
        (super-instantiate ()))))
  
  ; to apply to the top editor
  (define extend-top-editor-mixin
    (lambda (super%)
      (class super%
        
        ; (union gui-state symbol)
        (define gui-state 'uninitialized-gui-state-in-extend-top-editor-mixin)
        
        ; (union gui-view-state symbol)
        (define gui-view-state 'uninitialized-gui-view-state-in-extend-top-editor-mixin)
        
        ; (box (listof text%))
        (define known-editors (box '()))
        
        ; gui-state -> void
        ; init-snips-and-arrows-gui creates register-label-with-gui, which will call
        ; saav:register-label-with-gui, which will in turn find the editor for the label
        ; and call this method (if necessary) to initialize the editor's state, thereby
        ; allowing all the editors for a single analysis to share the same state (see
        ; the same method above too).
        (define/override (initialize-snips-and-arrows-gui-state new-gui-state)
          (super initialize-snips-and-arrows-gui-state new-gui-state)
          (set! gui-state new-gui-state)
          (set! gui-view-state (gui-state-gui-view-state new-gui-state)))
        
        ; -> void
        (define/override (reset-snips-and-arrows-state)
          (super reset-snips-and-arrows-state)
          (set! gui-state 'reinitialized-gui-state-in-extend-top-editor-mixin)
          (set! gui-view-state 'reinitialized-gui-view-state-in-extend-top-editor-mixin))
        
        ; string symbol -> boolean
        ; We forbid saving if the analysis is in the middle of running or in the middle
        ; of modifying the content of the editor
        (define/augment (can-save-file? filename format)
          (if (symbol? gui-state)
              (inner #t can-save-file? filename format)
              (if (and (gui-state-term-analysis-done? gui-state)
                       (not (saav:analysis-currently-modifying? gui-view-state)))
                  (inner #t can-save-file? filename format)
                  #f)))
        
        (define/override (save-file . args)
          (if (symbol? gui-state)
              (super save-file . args)
              (saav:run-thunk-without-snips gui-view-state
               (lambda () (super save-file . args)))))
                  
        ; -> void
        ; colors all registered labels
        ; The analysis proper is only officially done after we've colored everything, otherwise
        ; user insertions might occur before we have time to finish coloring and we will color
        ; the wrong stuff...
        (define/public (color-registered-labels)
          (unless (symbol? gui-view-state)
            (saav:color-registered-labels gui-view-state known-editors)
            (set-gui-state-term-analysis-done?! gui-state #t)))
        
        ; -> void
        ; remove all snips and arrows, and resets text style in all editors
        (define/public (remove-all-snips-and-arrows-and-colors)
          (if (symbol? gui-view-state)
              (saav:remove-all-colors known-editors)
              (saav:remove-all-snips-and-arrows-and-colors gui-view-state)))
        
        ; boolean dc% real real real real real real symbol -> void
        (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
          (super on-paint before? dc left top right bottom dx dy draw-caret)
          (when (and (not (symbol? gui-state))
                     (not before?)
                     (gui-state-term-analysis-done? gui-state))
            (saav:redraw-arrows gui-view-state dc dx dy)))
        
        (inherit find-position dc-location-to-editor-location)
        ; mouse-event% -> (values (union #f exact-non-negative-integer) (union #f text%))
        ; finds the editor in which a mouse-event% has occured, going down recursively
        ; if there are embedded editors, but not going down the embedded editors when they
        ; have been introduced by the analysis itself (e.g. type snips).
        (define (get-drscheme-pos-and-editor event)
          (let ([dc-x (send event get-x)]
                [dc-y (send event get-y)]
                [on-it? (box #f)])
            (let loop ([previous-pos #f]
                       [previous-editor #f]
                       [editor this])
              (let-values ([(ed-x ed-y) (send editor dc-location-to-editor-location dc-x dc-y)])
                (let ([pos (send editor find-position ed-x ed-y #f on-it?)])
                  (if (not (unbox on-it?))
                      (values #f #f)
                      (let ([snip (send editor find-snip pos 'after-or-none)])
                        (if (and snip (is-a? snip editor-snip%))
                            (let ([sub-editor (send snip get-editor)])
                              (if (saav:is-editor-registered? gui-view-state sub-editor)
                                  (loop pos editor sub-editor)
                                  (values pos editor)))
                            (values pos editor)))))))))

        (inherit get-admin)
        ; mouse-event% -> void
        (define/override (on-event event)
          (cond
            [(or (symbol? gui-state)
                 (not (gui-state-term-analysis-done? gui-state)))
             (super on-event event)]
            [(and (send event button-down? 'right)
                  (let-values ([(pos editor) (get-drscheme-pos-and-editor event)])
                    (if pos
                        (let ([labels (saav:get-related-labels-from-drscheme-pos-and-editor
                                       gui-view-state pos editor)])
                          (if (null? labels)
                              #f
                              (cons labels editor))) ; no "=>-values" so use cons...
                        #f)))
             =>
             (lambda (labels&editor)
               (let ([menu (make-object popup-menu%)]
                     [labels (car labels&editor)]
                     [editor (cdr labels&editor)])
                 ; SNIPS
                 (let ([create-snips-menu-item
                        (lambda (snip-type)
                          (create-snips-menu-item-by-type gui-state menu labels snip-type editor))])
                   (saav:for-each-snip-type gui-view-state create-snips-menu-item))
                 ; ARROWS
                 (create-arrow-menu-items gui-state menu labels)
                 ; HIDE ALL SNIPS
                 (when (saav:snips-currently-displayed-in-editor? gui-view-state editor)
                   (make-object menu-item%
                     (strcst:string-constant snips-and-arrows-hide-all-snips-in-editor)
                     menu
                     (lambda (item event)
                       (saav:remove-all-snips-in-editor gui-view-state editor))))
                 ; OTHER
                 ((gui-state-extend-menu-for-labels gui-state) menu labels)
                 
                 (when (not (null? (send menu get-items)))
                   (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
                     (send (get-admin) popup-menu menu x y)))
                 ))]
            [(and (send event button-down? 'middle)
                  (let-values ([(pos editor) (get-drscheme-pos-and-editor event)])
                    (if pos
                        (let ([labels (saav:get-related-labels-from-drscheme-pos-and-editor
                                       gui-view-state pos editor)])
                          (if (null? labels)
                              #f
                              (cons labels editor))) ; no "=>-values" so use cons...
                        #f)))
             =>
             (lambda (labels&editor)
               (let ([menu (make-object popup-menu%)]
                     [labels (car labels&editor)]
                     [editor (cdr labels&editor)]
                     [get-snip-text-from-snip-type-and-label
                      (gui-state-get-snip-text-from-snip-type-and-label gui-state)])
                 (saav:for-each-snip-type
                  gui-view-state
                  (lambda (snip-type)
                    (unless (andmap (lambda (label)
                                      (null? (get-snip-text-from-snip-type-and-label snip-type label)))
                                    labels)
                      ; at least one label has snips of this type
                      (for-each (lambda (label)
                                  (let ([snip-strings (get-snip-text-from-snip-type-and-label snip-type label)])
                                    (unless (null? snip-strings)
                                      (for-each
                                       (lambda (snip-string)
                                         (make-object menu-item%
                                           (if (<= (string-length snip-string) 200)
                                               snip-string
                                               (string-append
                                                (substring snip-string 0 197)
                                                "..."))
                                           menu
                                           (lambda (item event) cst:void)))
                                       snip-strings))))
                                labels)
                      (make-object separator-menu-item% menu))))
                 (when (not (null? (send menu get-items)))
                   (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
                     (send (get-admin) popup-menu menu x y)))
                 ))]
            [(send event leaving?)
             (let ([previous-labels (gui-state-previous-labels gui-state)])
               (when previous-labels
                 (for-each (lambda (previous-label)
                             (saav:remove-arrows gui-view-state previous-label #f #f))
                           previous-labels)
                 (set-gui-state-previous-labels! gui-state #f)
                 (saav:invalidate-bitmap-cache gui-view-state)))]
            [(or (send event moving?)
                 (send event entering?))
             (if (or (send event get-left-down)
                     (send event get-middle-down)
                     (send event get-right-down))
                 (super on-event event)
                 (let*-values ([(pos editor) (get-drscheme-pos-and-editor event)]
                               [(labels)
                                (if pos
                                    (saav:get-related-labels-from-drscheme-pos-and-editor
                                     gui-view-state pos editor)
                                    #f)]
                               [(previous-labels) (gui-state-previous-labels gui-state)]
                               [(not-same-labels) (not (equal? labels previous-labels))])
                   (when (and previous-labels not-same-labels)
                     (for-each (lambda (previous-label)
                                 (saav:remove-arrows gui-view-state previous-label #f #f))
                               previous-labels))
                   (when (and labels not-same-labels)
                     (for-each (lambda (arrow-info)
                                 (saav:add-arrow gui-view-state arrow-info #f))
                               ((gui-state-get-arrows-from-labels gui-state) labels)))
                   (when not-same-labels
                     (when (or (not (null? previous-labels))
                               (not (null? labels)))
                       ; something has changed, and we might have either removed some arrows or
                       ; added some (or both), so we redraw
                       (saav:invalidate-bitmap-cache gui-view-state))
                     (set-gui-state-previous-labels! gui-state labels))))]
            [else (super on-event event)]))
        
        (super-instantiate ()))))
  
  
  ; ... see below ... -> (label -> void)
  ; Ouch...  The returned function can be used to register labels with this gui
  (define (init-snips-and-arrows-gui
           ; % text%
           top-editor
           ; (label -> text%)
           get-editor-from-label
           ; (label -> non-negative-exact-integer)
           get-mzscheme-position-from-label
           ; (label -> non-negative-exact-integer)
           get-span-from-label
           ; ((listof label) -> (listof (list label label string)))
           get-arrows-from-labels
           ; (label -> style-delta%)
           get-style-delta-from-label
           ; (popup-menu% (listof label) -> void)
           extend-menu-for-labels
           ; (symbol symbol -> string)
           get-menu-text-from-snip-type
           ; (symbol label -> (listof string))
           get-snip-text-from-snip-type-and-label
           ; (listof (cons symbol string))
           snip-types-and-colors
           ; boolean
           clear-colors-immediately?)
    (let* ([gui-view-state (saav:make-gui-view-state
                            top-editor
                            get-editor-from-label
                            get-mzscheme-position-from-label
                            get-span-from-label
                            get-snip-text-from-snip-type-and-label
                            get-style-delta-from-label
                            snip-types-and-colors
                            clear-colors-immediately?)]
           [gui-state (make-gui-state
                       gui-view-state
                       get-arrows-from-labels
                       get-menu-text-from-snip-type
                       get-snip-text-from-snip-type-and-label
                       extend-menu-for-labels
                       #f
                       #f)])
      ; just make sure everything is clear before assigning a new state
      (send top-editor remove-all-snips-and-arrows-and-colors)
      
      ; we need this to force the registration of the top editor, to make sure
      ; on-paint and on-event work correctly even when no label has been registered for
      ; the top editor itself.
      (saav:register-editor-with-gui
       gui-view-state top-editor
       (lambda (editor)
         (send editor initialize-snips-and-arrows-gui-state gui-state)))
      
      (values
       ; (listof (cons label string)) -> void
       (lambda (labels-and-new-terms)
         (saav:user-change-terms gui-view-state labels-and-new-terms))
       
       ; label -> void
       ; to register a label with the gui
       (lambda (label)
         (saav:register-label-with-gui
          gui-view-state label
          (lambda (editor)
            (send editor initialize-snips-and-arrows-gui-state gui-state))))
       )))
  
  ; SIMPLIFIED INTERFACE
  ; symbol -> void
  ; default function for snip handling
  (define error-no-snips
    (case-lambda
      [(_) (error-no-snips 'dummy 'dummy)]
      [(_1 _2) (error 'snips-and-arrows "no snip info was provided when snips-and-arrows library was initialized")]))
  
  ; ... see below ... -> (values gui-state (label -> void))
  ; simplified version of make-snips-and-arrows-state, specialized for syntax objects,
  ; and with default handling of snips
  (define init-snips-and-arrows-gui-for-syntax-objects
    (opt-lambda (; text%
                 top-editor
                 ; ((listof syntax-object) -> (listof (list syntax-object syntax-object string)))
                 get-arrows-from-syntax-objects
                 ; (syntax-object -> style-delta%)
                 get-style-delta-from-syntax-object
                 
                 ; OPTIONAL menu stuff
                 ; (popup-menu% (listof syntax-object) -> void)
                 (extand-menu-for-syntax-objects (lambda (menu stxs) cst:void))
                 
                 ; OPTIONAL snip stuff
                 ; (symbol symbol -> string)
                 (get-menu-text-from-snip-type error-no-snips)
                 ; (symbol syntax-object -> (listof string))
                 (get-snip-text-from-snip-type-and-syntax-object error-no-snips)
                 ; (listof (cons symbol string))
                 (snip-types-and-colors '())
                 
                 ; boolean
                 (clear-colors-immediately? #f))
      (init-snips-and-arrows-gui
       top-editor
       syntax-source
       syntax-position
       syntax-span
       get-arrows-from-syntax-objects
       get-style-delta-from-syntax-object
       extand-menu-for-syntax-objects
       get-menu-text-from-snip-type
       get-snip-text-from-snip-type-and-syntax-object
       snip-types-and-colors
       clear-colors-immediately?)))
  
  )
