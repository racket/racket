
(module snips-and-arrows-view (lib "mrflow.ss" "mrflow")
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix arrow: (lib "arrow.ss" "drscheme"))
   (only (lib "list.ss") sort)
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   
   (prefix cst: "constants.ss")
   (prefix saam: "snips-and-arrows-model.ss")
   ;"set-list.ss"
   "set-hash.ss"
   ;"assoc-set-list.ss"
   "assoc-set-hash.ss"
   "labels.ss"
   )
  
  (define-struct gui-view-state (; gui-model-state
                                 gui-model-state
                                 ; test%
                                 top-editor
                                 ; (label -> text%)
                                 get-editor-from-label
                                 ; boolean
                                 ; so we can differenciate between actions done by the analysis and actions
                                 ; done by the user. Also prevents an infinite loop when deleting: if the user
                                 ; deletes something, it triggers a call to after-delete, which deletes all the
                                 ; snips, which triggers calls to after-delete, etc... so after-delete needs to
                                 ; be wrapped to prevent an infinite loop.
                                 analysis-currently-modifying?
                                 ; (symbol label -> (listof string))
                                 get-snip-text-from-snip-type-and-label
                                 ; (label -> style-delta%)
                                 get-style-delta-from-label
                                 ; (listof (cons symbol style-delta%))
                                 snip-types-and-colors
                                 ; boolean
                                 clear-colors-immediately?
                                 ))
  
  (provide/contract
   (make-gui-view-state (text%?
                         (label? . -> . text%?)
                         (label? . -> . non-negative-exact-integer?)
                         (label? . -> . non-negative-exact-integer?)
                         (symbol? label? . -> . (listof string?))
                         (label? . -> . style-delta%?)
                         (listof (cons/c symbol? string?))
                         boolean?
                         . -> . gui-view-state?))
   
   (rename gui-view-state-analysis-currently-modifying?
           analysis-currently-modifying?
           (gui-view-state? . -> . boolean?))
   (color-registered-labels (gui-view-state? (box/c (listof text%?)) . -> . void?))
   (after-user-action (gui-view-state? . -> . void?))
   
   (register-label-with-gui (gui-view-state? label? (text%? . -> . void?) . -> . void?))
   (register-editor-with-gui (gui-view-state? text%? (text%? . -> . void?) . -> . void?))
   (is-editor-registered? (gui-view-state? text%? . -> . boolean?))
   (get-related-labels-from-drscheme-pos-and-editor (gui-view-state? non-negative-exact-integer? text%? . -> . (listof label?)))
   (user-change-terms (gui-view-state? (listof (cons/c label? string?)) . -> . void?))
   
   (add-arrow (gui-view-state? (list/c label? label? string?) boolean? . -> . void?))
   (get-tacked-arrows-from-label (gui-view-state? label? . -> . non-negative-exact-integer?))
   (remove-arrows (gui-view-state? label? (or/c symbol? boolean?) boolean? . -> . void?))
   (redraw-arrows (gui-view-state? (is-a?/c dc<%>) real? real? . -> . void?))
   
   (invalidate-bitmap-cache (gui-view-state? . -> . void?))
   
   (label-has-snips-of-this-type? (gui-view-state? label? symbol? . -> . boolean?))
   (snips-currently-displayed-in-editor? (gui-view-state? text%? . -> . boolean?))
   (for-each-snip-type (gui-view-state? (symbol? . -> . void?) . -> . void?))
   (run-thunk-without-snips (gui-view-state? (-> any) . -> . any))
   (add-snips (gui-view-state? label? symbol? text%? . -> . void?))
   (remove-inserted-snips (gui-view-state? label? symbol? text%? . -> . void?))
   (remove-all-snips-in-editor (gui-view-state? text%? . -> . void?))
   (remove-all-snips-in-all-editors (gui-view-state? . -> . void?))
   (remove-all-colors ((box/c (listof text%?)) . -> . void?))
   (remove-all-snips-and-arrows-and-colors (gui-view-state? . -> . void?))
   )
  
  ; text%
  ; (label -> text%)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; (symbol label -> (listof string))
  ; (label -> style-delta%)
  ; (listof (cons symbol style-delta%))
  ; boolean
  ; -> gui-view-state
  (set! make-gui-view-state
        (let ([real-make-gui-view-state make-gui-view-state])
          (lambda (top-editor
                   get-editor-from-label
                   get-mzscheme-position-from-label
                   get-span-from-label
                   get-snip-text-from-snip-type-and-label
                   get-style-delta-from-label
                   snip-types-and-colors
                   clear-colors-immediately?)
            (real-make-gui-view-state (saam:make-gui-model-state get-editor-from-label
                                                                 get-mzscheme-position-from-label
                                                                 get-span-from-label
                                                                 (map car snip-types-and-colors))
                                      top-editor
                                      get-editor-from-label
                                      #f
                                      get-snip-text-from-snip-type-and-label
                                      get-style-delta-from-label
                                      (map (lambda (snip-type-and-color)
                                             (cons (car snip-type-and-color)
                                                   (send (make-object style-delta%) set-delta-foreground (cdr snip-type-and-color))))
                                           snip-types-and-colors)
                                      clear-colors-immediately?))))
  
  ; INTERFACE BETWEEN MODEL AND TOP MODULE
  ; gui-view-state non-negative-exact-integer text% -> (listof label)
  (define (get-related-labels-from-drscheme-pos-and-editor gui-view-state pos editor)
    (saam:get-related-labels-from-drscheme-pos-and-source
     (gui-view-state-gui-model-state gui-view-state) pos editor))
  
  ; gui-view-state label (text% -> void) -> void
  ; registers a label with the gui. We also need to initialize the editor's state the first time
  ; we see that editor, to make sure all editors are sharing the same state.
  ; Note that we could color the label as we go, thereby having incremental coloring as we
  ; analyze terms, but that turns out to be *very* slow, because the editor has to be unlocked
  ; (because of disable-evalution), the style changed, the editor re-lock and the bitmap cache
  ; invalidated for each label in turn.  It would also possibly not show all the arrows for a
  ; given label while the analysis is still going on.
  (define (register-label-with-gui gui-view-state label init-editor)
    (let ([editor (saam:register-label-with-gui (gui-view-state-gui-model-state gui-view-state) label)])
      (when editor (init-editor editor)))
    cst:void)
  
  ; gui-view-state text% (text% -> void) -> void
  ; Same as above, except that we register an editor instead of a label.  We use this to always
  ; register the top editor (see comment in make-register-label-with-gui in
  ; snips-and-arrows.ss).
  (define (register-editor-with-gui gui-view-state editor init-editor)
    (let ([editor (saam:register-source-with-gui (gui-view-state-gui-model-state gui-view-state) editor)])
      (when editor (init-editor editor)))
    cst:void)
  
  ; gui-view-state text% -> boolean
  (define (is-editor-registered? gui-view-state editor)
    (saam:is-source-registered? (gui-view-state-gui-model-state gui-view-state) editor))
  
  ; gui-view-state (symbol -> void) -> void
  (define (for-each-snip-type gui-view-state f)
    (saam:for-each-snip-type (gui-view-state-gui-model-state gui-view-state) f))
  
  ; gui-view-state label symbol -> boolean
  (define (label-has-snips-of-this-type? gui-view-state label type)
    (saam:label-has-snips-of-this-type? (gui-view-state-gui-model-state gui-view-state) label type))
  
  ; gui-view-state text% -> boolean
  (define (snips-currently-displayed-in-editor? gui-view-state editor)
    (saam:snips-currently-displayed-in-source? (gui-view-state-gui-model-state gui-view-state) editor))
  
  ; gui-view-state label -> non-negative-exact-integer
  (define (get-tacked-arrows-from-label gui-view-state label)
    (saam:get-tacked-arrows-from-label (gui-view-state-gui-model-state gui-view-state) label))
  
  ; gui-view-state (list label label string) boolean -> void
  (define (add-arrow gui-view-state arrow-info tacked?)
    (saam:add-arrow (gui-view-state-gui-model-state gui-view-state) arrow-info tacked?))
  
  ; gui-view-state label (or/c symbol boolean) boolean -> void
  (define (remove-arrows gui-view-state start-label tacked? exn?)
    (saam:remove-arrows (gui-view-state-gui-model-state gui-view-state) start-label tacked? exn?))
  
  
  ; COLORING / CLEARING
  ; gui-view-state (box (listof text%)) -> void
  ; Color all registered labels.  Note that we know that no user modifications will be
  ; possible while we color (snips-and-arrows.ss takes care of that through can-insert?
  ; can-delete?) so there's no need to lock the editors.
  ; We remember all the editors in known-editors, because we might need that later, once
  ; the state has been resetted, to correctly clear the colors in all editors.
  (define (color-registered-labels gui-view-state known-editors)
    (let* ([gui-model-state (gui-view-state-gui-model-state gui-view-state)]
           [get-span-from-label (saam:make-get-span-from-label-from-model-state gui-model-state)]
           [get-style-delta-from-label (gui-view-state-get-style-delta-from-label gui-view-state)])
      (saam:for-each-source
       gui-model-state
       (lambda (editor)
         (when editor
           (set-box! known-editors (cons editor (unbox known-editors)))
           (let ([locked? (send editor is-locked?)])
             (send editor begin-edit-sequence #f)
             (send editor lock #f)
             (saam:for-each-label-in-source
              gui-model-state
              editor
              (lambda (label)
                (let ([label-left-pos (saam:get-position-from-label gui-model-state label)])
                  (send editor change-style (get-style-delta-from-label label)
                        label-left-pos (+ label-left-pos (get-span-from-label label)) #f))))
             (send editor lock locked?)
             (send editor end-edit-sequence)))))
      (invalidate-bitmap-cache gui-view-state)))
  
  ; text% -> void
  ; resets all colors to original style
  (define (reset-editor-style editor)
    (when editor
      (let ([locked? (send editor is-locked?)])
        (send editor begin-edit-sequence #f)
        (send editor lock #f)
        ; comment this out if you want to keep all the pretty colors
        (let* ([style-list (send editor get-style-list)]
               [standard-style (send style-list find-named-style "Standard")])
          (when standard-style
            (send editor change-style
                  standard-style
                  0 (send editor last-position) #f)))
        (send editor lock locked?)
        (send editor end-edit-sequence))))
  
  ; (box (listof text%)) -> void
  (define (remove-all-colors known-editors)
    (for-each reset-editor-style (unbox known-editors))
    (set-box! known-editors '()))
  
  ; gui-view-state -> void
  (define (remove-all-colors-using-state gui-view-state)
    (saam:for-each-source (gui-view-state-gui-model-state gui-view-state) reset-editor-style))
  
  ; gui-view-state -> void
  ; remove arrows and all snips, editor by editor.
  (define (remove-all-snips-and-arrows gui-view-state)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
    (saam:remove-all-arrows (gui-view-state-gui-model-state gui-view-state))
    (invalidate-bitmap-cache gui-view-state)
    (remove-all-snips-in-all-editors gui-view-state)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f))
  
  ; gui-view-state text% -> void
  ; Remove all snips in a given editor.  We loop over each label and then loop over each
  ; snip type and remove the corresponding snip group.  It would probably be much faster
  ; to first get the positions of the groups of all snips for each label (since for a given
  ; label all the groups of snips of different types are next to each other), sort them
  ; by decreasing position (so that removing a group of snip doesn't require recomputing
  ; the positions of the remaining groups), then remove them in that order.  I might do
  ; that one day if people complain of slowness...
  (define (remove-all-snips-in-editor gui-view-state editor)
    (let ([gui-model-state (gui-view-state-gui-model-state gui-view-state)])
      (saam:for-each-label-in-source
       gui-model-state
       editor
       (lambda (label)
         (saam:for-each-snip-type
          gui-model-state
          (lambda (type)
            (when (saam:label-has-snips-of-this-type? gui-model-state label type)
              (remove-inserted-snips gui-view-state label type editor))))))))
  
  ; gui-view-state -> void
  ; remove all snips
  (define (remove-all-snips-in-all-editors gui-view-state)
    (saam:for-each-source (gui-view-state-gui-model-state gui-view-state)
                          (lambda (editor)
                            (remove-all-snips-in-editor gui-view-state editor))))
  
  ; gui-view-state -> void
  ; clear all and reset all
  (define (remove-all-snips-and-arrows-and-colors gui-view-state)
    (remove-all-snips-and-arrows gui-view-state)
    (remove-all-colors-using-state gui-view-state)
    (reset-all-editors-state gui-view-state))
  
  ; gui-view-state -> void
  ; invalidates the bitmap cache of the top editor, which will call the overridden
  ; on-paint method of the top editor and redraw the arrows.
  (define (invalidate-bitmap-cache gui-view-state)
    (send (gui-view-state-top-editor gui-view-state) invalidate-bitmap-cache))
  
  ; gui-view-state -> void
  ; Resets the state of all editors we know about.  Last nail in the coffin for
  ; this analysis round.
  (define (reset-all-editors-state gui-view-state)
    (saam:for-each-source (gui-view-state-gui-model-state gui-view-state)
                          (lambda (editor)
                            (send editor reset-snips-and-arrows-state))))  
  
  
  ; EDITOR EVENTS INTERACTION
  ; gui-view-state -> void
  ; the user has started modifying stuff, so we just remove all snips (in other editors only,
  ; since we know a user modification is only allowed if the current editor doesn't have
  ; any snips - the current editor is currently locked anyway) and all arrows (in all editors),
  (define (after-user-action gui-view-state)
    (remove-all-snips-and-arrows gui-view-state)
    (when (gui-view-state-clear-colors-immediately? gui-view-state)
      (remove-all-colors-using-state gui-view-state))
    (reset-all-editors-state gui-view-state))
  
  ; gui-view-state dc% real real -> void
  ; redraws arrows during on-paint
  (define (redraw-arrows gui-view-state dc dx dy)
    (let ([top-editor (gui-view-state-top-editor gui-view-state)]
          [untacked-arrow-brush (send the-brush-list find-or-create-brush "white" 'solid)]
          [old-pen (send dc get-pen)]
          [old-brush (send dc get-brush)])
      (saam:for-each-arrow (gui-view-state-gui-model-state gui-view-state)
                           (lambda (start-label-pos-left end-label-pos-left
                                                         start-label-span end-label-span
                                                         start-editor end-editor
                                                         tacked? color)
                             (send dc set-pen (send the-pen-list find-or-create-pen color 1 'solid))
                             (if tacked?
                                 (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
                                 (send dc set-brush untacked-arrow-brush))
                             (draw-arrow start-label-pos-left
                                         (+ start-label-pos-left start-label-span)
                                         end-label-pos-left
                                         (+ end-label-pos-left end-label-span)
                                         top-editor
                                         start-editor
                                         end-editor
                                         dc dx dy)))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)))
  
  ; TEXT
  ; gui-view-state (listof (cons label string)) -> void
  ; Resize and re-color the terms corresponding to all the labels.
  ; We know there's at least one label in the list for each term to be changed,
  ; but there might be several labels in the list for the same term.  We need
  ; to update *all* known labels for all term to be changed, and modify the
  ; corresponding term only once.  So we do it in two steps:
  ; - we sort the new terms by editor and position, throwing away all the labels
  ;   (we only needed them to get the positions)
  ; - from the positions and the editors, get all the labels (sounds redundant?
  ;   the idea is that we then know that we have *all* the labels for all the
  ;   terms to be changed, and we know that we have each label only once) and
  ;   actually do the changes, modifying all the labels for a given term and
  ;   modifying the content of the corresponding editor only once for a given
  ;   term, for all terms, by decreasing position in each editor.
  ; At least we know that all labels for a given term have the same editor (unless
  ; the user of this library really screwed up get-editor-from-label but then it's
  ; not our problem if the user can't read the docs...)
  (define (user-change-terms gui-view-state labels-and-new-terms)
    (if (null? labels-and-new-terms)
        (error 'user-change-terms "internal error: can't resize no labels~n")
        (let ([get-editor-from-label (gui-view-state-get-editor-from-label gui-view-state)]
              [get-style-delta-from-label (gui-view-state-get-style-delta-from-label gui-view-state)]
              [new-terms-by-positions-by-editor (assoc-set-make)]
              [gui-model-state (gui-view-state-gui-model-state gui-view-state)])
          (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
          ; first we sort the terms to be modified by editor and by position
          ; at the end we throw away the labels, because we don't know whether we have
          ; all of them, so since we'll have to get all of them ourselves, we might just
          ; as well throw away all the onces the user gave us, at least we won't have to
          ; do any sorting to make sure we don't have duplicates.
          (for-each
           (lambda (label-and-new-term)
             (let* ([label (car label-and-new-term)]
                    [new-term (cdr label-and-new-term)]
                    [editor (get-editor-from-label label)]
                    [new-terms-by-position
                     (assoc-set-get new-terms-by-positions-by-editor
                                    editor
                                    (lambda ()
                                      (let ([new-terms-by-position (assoc-set-make)])
                                        (assoc-set-set new-terms-by-positions-by-editor
                                                       editor
                                                       new-terms-by-position)
                                        new-terms-by-position)))]
                    [position (saam:get-position-from-label gui-model-state label)]
                    [current-new-term
                     (assoc-set-get new-terms-by-position
                                    position
                                    (lambda ()
                                      (assoc-set-set new-terms-by-position
                                                     position
                                                     new-term)
                                      new-term))])
               (unless (string=? new-term current-new-term)
                 (error 'user-change-terms "two different terms specified for same position: ~a and ~a"
                        new-term current-new-term))))
           labels-and-new-terms)
          ; then for each editor and each position we have found, we update all the labels
          ; by changing their span in the model, and modify the editor at the right place (note
          ; that we need to sort the positions of the labels in decreasing order for a given
          ; editor, otherwise modifying one term would change the actual positions of the
          ; remaining terms to change...)
          ;
          ; These changes can be undone only when the editor doesn't contain any snips,
          ; otherwise the undo will undo at the wrong place.  Even if we were to force
          ; the change without undo, it would still not work because any previous action
          ; could later be undone at the wrong place.  The only way out it to put the
          ; whole thing inside run-thunk-without-snips (which will make it undoable
          ; from DrScheme's point of view) and provide our own undoer to undo the change.
          ; XXX to be done later... same thing with user modifications (insert / delete):
          ; use run-thunk-without-snips and provide our own undoer with add-undo.
          ; In the meantime we just forbid the change.  Note that we must test all the editors
          ; for snips before doing any change, because otherwise we might change terms in one
          ; editor and not in another and break the semantics of the change.
          (let ([abort? #f])
            (assoc-set-for-each
             new-terms-by-positions-by-editor
             (lambda (editor new-terms-by-positions)
               (when (snips-currently-displayed-in-editor? gui-view-state editor)
                 (set! abort? #t))))
            (if abort?
                (message-box (strcst:string-constant snips-and-arrows-user-action-disallowed-title)
                             (strcst:string-constant snips-and-arrows-user-action-disallowed)
                             #f '(ok caution))
                ; the "save" button will show up...
                (assoc-set-for-each
                 new-terms-by-positions-by-editor
                 (lambda (editor new-terms-by-positions)
                   (when editor
                     (let ([locked? (send editor is-locked?)])
                       (send editor begin-edit-sequence #t)
                       (send editor lock #f)
                       (for-each
                        (lambda (position-and-new-term-pair)
                          (let* ([position (car position-and-new-term-pair)]
                                 [new-term (cdr position-and-new-term-pair)]
                                 [labels (get-related-labels-from-drscheme-pos-and-editor gui-view-state position editor)])
                            (let-values ([(old-ending-pos new-ending-pos)
                                          (saam:user-change-terms gui-model-state
                                                                  labels editor
                                                                  (string-length new-term))])
                              (send editor insert new-term position old-ending-pos)
                              ; the styles for the different labels are hopefully the same...
                              (send editor change-style
                                    (get-style-delta-from-label (car labels))
                                    position new-ending-pos #f))))
                        (sort (assoc-set-map new-terms-by-positions cons)
                              (lambda (pos&term-pair1 pos&term-pair2)
                                (> (car pos&term-pair1) (car pos&term-pair2)))))
                       (send editor lock locked?)
                       (send editor end-edit-sequence)))))))
          (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f))))
  
  
  ; SNIPS
  ; gui-view-state label symbol text% -> void
  ; Adds snips of given type to given label.
  ; We could get the editor from the label, but there's no reason to bother...
  (define (add-snips gui-view-state label type editor)
    (when editor
      (let ([snips-content
             ((gui-view-state-get-snip-text-from-snip-type-and-label gui-view-state) type label)])
        (unless (null? snips-content)
          (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
          (let ([snip-style
                 (cdr (assq type (gui-view-state-snip-types-and-colors gui-view-state)))]
                [starting-pos (saam:add-snips (gui-view-state-gui-model-state gui-view-state)
                                              label type editor (length snips-content))]
                [locked? (send editor is-locked?)]
                [modified? (send editor is-modified?)])
            (send editor begin-edit-sequence #f)
            (send editor lock #f)
            (for-each (lambda (snip-content)
                        (let* ([snip-text (make-object text%)]
                               [snip (make-object editor-snip% snip-text)])
                          (send snip-text insert snip-content)
                          (send snip-text lock #t)
                          (send editor insert snip starting-pos starting-pos)
                          ; XXX bug here on Solaris, can be worked around
                          ; (invalidate-bitmap-cache gui-view-state)
                          ; see collects/test/tool2.ss
                          (send editor change-style snip-style
                                starting-pos (add1 starting-pos) #f)))
                      snips-content)
            (send editor set-modified modified?)
            (send editor lock locked?)
            (send editor end-edit-sequence))
          (invalidate-bitmap-cache gui-view-state)
          (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))))
  
  ; gui-view-state label symbol text% -> void
  ; Remove snips for a given label and type.
  ; We could get the editor from the label, but there's no reason to bother...
  (define (remove-inserted-snips gui-view-state label type editor)
    (when editor
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (let-values ([(starting-pos ending-pos)
                    (saam:remove-inserted-snips (gui-view-state-gui-model-state gui-view-state)
                                                label type editor)]
                   [(locked?) (send editor is-locked?)]
                   [(modified?) (send editor is-modified?)])
        ; all the snips for a given label and type are contiguous and deleted at once.
        (send editor begin-edit-sequence #f)
        (send editor lock #f)
        (send editor delete starting-pos ending-pos #f)
        (send editor set-modified modified?)
        (send editor lock locked?)
        (send editor end-edit-sequence))
      (invalidate-bitmap-cache gui-view-state)
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  ; gui-view-state (-> top) -> top
  ; removes all the snips (and remembers them), runs the thunk, then puts all the snips back in...
  ; remove-inserted-snips and add-snips take care of is-locked? and is-modified?, but even
  ; though they also take care of begin/end-edit-sequence, we still need to wrap everything
  ; in a sequence here otherwise the user would see the snips suddenly disappear and reappear...
  (define (run-thunk-without-snips gui-view-state thunk)
    (let ([gui-model-state (gui-view-state-gui-model-state gui-view-state)]
          [snip-types-by-label-by-editor (assoc-set-make)])
      (saam:for-each-source
       gui-model-state
       (lambda (editor)
         (send editor begin-edit-sequence #f)
         (let ([snip-types-by-label (assoc-set-make)])
           (assoc-set-set snip-types-by-label-by-editor editor snip-types-by-label)
           (saam:for-each-label-in-source
            gui-model-state
            editor
            (lambda (label)
              (saam:for-each-snip-type
               gui-model-state
               (lambda (type)
                 (when (saam:label-has-snips-of-this-type? gui-model-state label type)
                   (set-set (assoc-set-get snip-types-by-label label
                                           (lambda ()
                                             (let ([set (set-make)])
                                               (assoc-set-set snip-types-by-label label set)
                                               set)))
                            type)
                   (remove-inserted-snips gui-view-state label type editor)))))))))
      (let ([result (thunk)])
        (assoc-set-for-each
         snip-types-by-label-by-editor
         (lambda (editor snip-types-by-label)
           (assoc-set-for-each
            snip-types-by-label
            (lambda (label types-set)
              (set-for-each
               types-set
               (lambda (type)
                 (add-snips gui-view-state label type editor)))))
           (send editor end-edit-sequence)))
        result)))
  
  
  ; ARROWS
  ; (box number) (box number) -> number
  (define (average box1 box2)
    (/ (+ (unbox box1) (unbox box2)) 2))
  
  ; non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer
  ; text% text% text% dc% real real -> void
  ; Computes actual locations for arrow and draws it.
  ; Note that we don't do anything to prevent arrows of length zero from being drawn - these
  ; might show up when using macros that duplicate terms, so arrows of length zero are then
  ; the correct thing to do as far as I am concerned).
  (define (draw-arrow start-label-pos-left start-label-pos-right
                      end-label-pos-left end-label-pos-right
                      top-editor start-editor end-editor
                      dc dx dy)
    (let ([start-sub-ed-left-x-loc (box 0)]
          [start-sub-ed-top-y-loc (box 0)]
          [start-sub-ed-right-x-loc (box 0)]
          [start-sub-ed-bot-y-loc (box 0)]
          [end-sub-ed-left-x-loc (box 0)]
          [end-sub-ed-top-y-loc (box 0)]
          [end-sub-ed-right-x-loc (box 0)]
          [end-sub-ed-bot-y-loc (box 0)])
      (send start-editor position-location start-label-pos-left start-sub-ed-left-x-loc start-sub-ed-top-y-loc #t)
      (send start-editor position-location start-label-pos-right start-sub-ed-right-x-loc #f #f)
      (send start-editor position-location (sub1 start-label-pos-right) #f start-sub-ed-bot-y-loc #f)
      (send end-editor position-location end-label-pos-left end-sub-ed-left-x-loc end-sub-ed-top-y-loc #t)
      (send end-editor position-location end-label-pos-right end-sub-ed-right-x-loc #f #f)
      (send end-editor position-location (sub1 end-label-pos-right) #f end-sub-ed-bot-y-loc #f)
      (let*-values
          ([(start-sub-ed-x-loc) (average start-sub-ed-left-x-loc start-sub-ed-right-x-loc)]
           [(start-sub-ed-y-loc) (average start-sub-ed-top-y-loc start-sub-ed-bot-y-loc)]
           [(end-sub-ed-x-loc) (average end-sub-ed-left-x-loc end-sub-ed-right-x-loc)]
           [(end-sub-ed-y-loc) (average end-sub-ed-top-y-loc end-sub-ed-bot-y-loc)]
           [(start-dc-x-loc start-dc-y-loc)
            (send start-editor editor-location-to-dc-location start-sub-ed-x-loc start-sub-ed-y-loc)]
           [(end-dc-x-loc end-dc-y-loc)
            (send end-editor editor-location-to-dc-location end-sub-ed-x-loc end-sub-ed-y-loc)]
           [(start-top-ed-x-loc start-top-ed-y-loc)
            (send top-editor dc-location-to-editor-location start-dc-x-loc start-dc-y-loc)]
           [(end-top-ed-x-loc end-top-ed-y-loc)
            (send top-editor dc-location-to-editor-location end-dc-x-loc end-dc-y-loc)])
        (arrow:draw-arrow
         dc start-top-ed-x-loc start-top-ed-y-loc end-top-ed-x-loc end-top-ed-y-loc dx dy))))
  
  )
