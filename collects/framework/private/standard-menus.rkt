(define standard-menus<%>
  (interface
   (basic<%>)
   get-menu%
   get-menu-item%
   get-checkable-menu-item%
   get-file-menu
   get-edit-menu
   get-help-menu
   file-menu:new-callback
   file-menu:get-new-item
   file-menu:new-string
   file-menu:new-help-string
   file-menu:new-on-demand
   file-menu:create-new?
   file-menu:between-new-and-open
   file-menu:open-callback
   file-menu:get-open-item
   file-menu:open-string
   file-menu:open-help-string
   file-menu:open-on-demand
   file-menu:create-open?
   file-menu:open-recent-callback
   file-menu:get-open-recent-item
   file-menu:open-recent-string
   file-menu:open-recent-help-string
   file-menu:open-recent-on-demand
   file-menu:create-open-recent?
   file-menu:between-open-and-revert
   file-menu:revert-callback
   file-menu:get-revert-item
   file-menu:revert-string
   file-menu:revert-help-string
   file-menu:revert-on-demand
   file-menu:create-revert?
   file-menu:between-revert-and-save
   file-menu:save-callback
   file-menu:get-save-item
   file-menu:save-string
   file-menu:save-help-string
   file-menu:save-on-demand
   file-menu:create-save?
   file-menu:save-as-callback
   file-menu:get-save-as-item
   file-menu:save-as-string
   file-menu:save-as-help-string
   file-menu:save-as-on-demand
   file-menu:create-save-as?
   file-menu:between-save-as-and-print
   file-menu:print-callback
   file-menu:get-print-item
   file-menu:print-string
   file-menu:print-help-string
   file-menu:print-on-demand
   file-menu:create-print?
   file-menu:between-print-and-close
   file-menu:close-callback
   file-menu:get-close-item
   file-menu:close-string
   file-menu:close-help-string
   file-menu:close-on-demand
   file-menu:create-close?
   file-menu:between-close-and-quit
   file-menu:quit-callback
   file-menu:get-quit-item
   file-menu:quit-string
   file-menu:quit-help-string
   file-menu:quit-on-demand
   file-menu:create-quit?
   file-menu:after-quit
   edit-menu:undo-callback
   edit-menu:get-undo-item
   edit-menu:undo-string
   edit-menu:undo-help-string
   edit-menu:undo-on-demand
   edit-menu:create-undo?
   edit-menu:redo-callback
   edit-menu:get-redo-item
   edit-menu:redo-string
   edit-menu:redo-help-string
   edit-menu:redo-on-demand
   edit-menu:create-redo?
   edit-menu:between-redo-and-cut
   edit-menu:cut-callback
   edit-menu:get-cut-item
   edit-menu:cut-string
   edit-menu:cut-help-string
   edit-menu:cut-on-demand
   edit-menu:create-cut?
   edit-menu:between-cut-and-copy
   edit-menu:copy-callback
   edit-menu:get-copy-item
   edit-menu:copy-string
   edit-menu:copy-help-string
   edit-menu:copy-on-demand
   edit-menu:create-copy?
   edit-menu:between-copy-and-paste
   edit-menu:paste-callback
   edit-menu:get-paste-item
   edit-menu:paste-string
   edit-menu:paste-help-string
   edit-menu:paste-on-demand
   edit-menu:create-paste?
   edit-menu:between-paste-and-clear
   edit-menu:clear-callback
   edit-menu:get-clear-item
   edit-menu:clear-string
   edit-menu:clear-help-string
   edit-menu:clear-on-demand
   edit-menu:create-clear?
   edit-menu:between-clear-and-select-all
   edit-menu:select-all-callback
   edit-menu:get-select-all-item
   edit-menu:select-all-string
   edit-menu:select-all-help-string
   edit-menu:select-all-on-demand
   edit-menu:create-select-all?
   edit-menu:between-select-all-and-find
   edit-menu:find-callback
   edit-menu:get-find-item
   edit-menu:find-string
   edit-menu:find-help-string
   edit-menu:find-on-demand
   edit-menu:create-find?
   edit-menu:find-next-callback
   edit-menu:get-find-next-item
   edit-menu:find-next-string
   edit-menu:find-next-help-string
   edit-menu:find-next-on-demand
   edit-menu:create-find-next?
   edit-menu:find-previous-callback
   edit-menu:get-find-previous-item
   edit-menu:find-previous-string
   edit-menu:find-previous-help-string
   edit-menu:find-previous-on-demand
   edit-menu:create-find-previous?
   edit-menu:show/hide-replace-callback
   edit-menu:get-show/hide-replace-item
   edit-menu:show/hide-replace-string
   edit-menu:show/hide-replace-help-string
   edit-menu:show/hide-replace-on-demand
   edit-menu:create-show/hide-replace?
   edit-menu:replace-callback
   edit-menu:get-replace-item
   edit-menu:replace-string
   edit-menu:replace-help-string
   edit-menu:replace-on-demand
   edit-menu:create-replace?
   edit-menu:replace-all-callback
   edit-menu:get-replace-all-item
   edit-menu:replace-all-string
   edit-menu:replace-all-help-string
   edit-menu:replace-all-on-demand
   edit-menu:create-replace-all?
   edit-menu:find-case-sensitive-callback
   edit-menu:get-find-case-sensitive-item
   edit-menu:find-case-sensitive-string
   edit-menu:find-case-sensitive-help-string
   edit-menu:find-case-sensitive-on-demand
   edit-menu:create-find-case-sensitive?
   edit-menu:between-find-and-preferences
   edit-menu:preferences-callback
   edit-menu:get-preferences-item
   edit-menu:preferences-string
   edit-menu:preferences-help-string
   edit-menu:preferences-on-demand
   edit-menu:create-preferences?
   edit-menu:after-preferences
   help-menu:before-about
   help-menu:about-callback
   help-menu:get-about-item
   help-menu:about-string
   help-menu:about-help-string
   help-menu:about-on-demand
   help-menu:create-about?
   help-menu:after-about))

(define standard-menus-mixin
  (mixin
   (basic<%>)
   (standard-menus<%>)
   (inherit on-menu-char on-traverse-char)
   (define remove-prefs-callback
     (preferences:add-callback
      'framework:menu-bindings
      (λ (p v)
        (let loop ((menu (get-menu-bar)))
          (when (is-a? menu menu:can-restore<%>)
            (if v (send menu restore-keybinding) (send menu set-shortcut #f)))
          (when (is-a? menu menu:can-restore-underscore<%>)
            (if v
              (send menu restore-underscores)
              (send menu erase-underscores)))
          (when (is-a? menu menu-item-container<%>)
            (for-each loop (send menu get-items)))))))
   (inherit get-menu-bar show can-close? get-edit-target-object)
   (define/augment
    on-close
    (λ () (remove-prefs-callback) (inner (void) on-close)))
   (define/public get-menu% (λ () menu:can-restore-underscore-menu%))
   (define/public get-menu-item% (λ () menu:can-restore-menu-item%))
   (define/public
    get-checkable-menu-item%
    (λ () menu:can-restore-checkable-menu-item%))
   (define/public get-file-menu (λ () file-menu))
   (define/public get-edit-menu (λ () edit-menu))
   (define/public get-help-menu (λ () help-menu))
   (define/public
    file-menu:new-callback
    (λ (item control) (handler:edit-file #f) #t))
   (define/public (file-menu:get-new-item) file-menu:new-item)
   (define/public (file-menu:new-string) (string-constant new-menu-item))
   (define/public (file-menu:new-help-string) (string-constant new-info))
   (define/public file-menu:new-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-new?) #t)
   (define/public file-menu:between-new-and-open (λ (menu) (void)))
   (define/public
    file-menu:open-callback
    (λ (item control) (handler:open-file) #t))
   (define/public (file-menu:get-open-item) file-menu:open-item)
   (define/public (file-menu:open-string) (string-constant open-menu-item))
   (define/public (file-menu:open-help-string) (string-constant open-info))
   (define/public file-menu:open-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-open?) #t)
   (define/public file-menu:open-recent-callback (λ (x y) (void)))
   (define/public (file-menu:get-open-recent-item) file-menu:open-recent-item)
   (define/public
    (file-menu:open-recent-string)
    (string-constant open-recent-menu-item))
   (define/public
    (file-menu:open-recent-help-string)
    (string-constant open-recent-info))
   (define/public
    file-menu:open-recent-on-demand
    (λ (menu) (handler:install-recent-items menu)))
   (define/public (file-menu:create-open-recent?) #t)
   (define/public file-menu:between-open-and-revert (λ (menu) (void)))
   (define/public file-menu:revert-callback (λ (item control) (void)))
   (define/public (file-menu:get-revert-item) file-menu:revert-item)
   (define/public (file-menu:revert-string) (string-constant revert-menu-item))
   (define/public (file-menu:revert-help-string) (string-constant revert-info))
   (define/public file-menu:revert-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-revert?) #f)
   (define/public file-menu:between-revert-and-save (λ (menu) (void)))
   (define/public file-menu:save-callback (λ (item control) (void)))
   (define/public (file-menu:get-save-item) file-menu:save-item)
   (define/public (file-menu:save-string) (string-constant save-menu-item))
   (define/public (file-menu:save-help-string) (string-constant save-info))
   (define/public file-menu:save-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-save?) #f)
   (define/public file-menu:save-as-callback (λ (item control) (void)))
   (define/public (file-menu:get-save-as-item) file-menu:save-as-item)
   (define/public
    (file-menu:save-as-string)
    (string-constant save-as-menu-item))
   (define/public
    (file-menu:save-as-help-string)
    (string-constant save-as-info))
   (define/public file-menu:save-as-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-save-as?) #f)
   (define/public file-menu:between-save-as-and-print (λ (menu) (void)))
   (define/public file-menu:print-callback (λ (item control) (void)))
   (define/public (file-menu:get-print-item) file-menu:print-item)
   (define/public (file-menu:print-string) (string-constant print-menu-item))
   (define/public (file-menu:print-help-string) (string-constant print-info))
   (define/public file-menu:print-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-print?) #f)
   (define/public
    file-menu:between-print-and-close
    (λ (menu) (make-object separator-menu-item% menu)))
   (define/public
    file-menu:close-callback
    (λ (item control) (when (can-close?) (on-close) (show #f)) #t))
   (define/public (file-menu:get-close-item) file-menu:close-item)
   (define/public (file-menu:close-string) (string-constant close-menu-item))
   (define/public (file-menu:close-help-string) (string-constant close-info))
   (define/public file-menu:close-on-demand (λ (menu-item) (void)))
   (define/public (file-menu:create-close?) #t)
   (define/public file-menu:between-close-and-quit (λ (menu) (void)))
   (define/public
    file-menu:quit-callback
    (λ (item control) (when (exit:user-oks-exit) (exit:exit))))
   (define/public (file-menu:get-quit-item) file-menu:quit-item)
   (define/public
    (file-menu:quit-string)
    (if (eq? (system-type) 'windows)
      (string-constant quit-menu-item-windows)
      (string-constant quit-menu-item-others)))
   (define/public (file-menu:quit-help-string) (string-constant quit-info))
   (define/public file-menu:quit-on-demand (λ (menu-item) (void)))
   (define/public
    (file-menu:create-quit?)
    (not (current-eventspace-has-standard-menus?)))
   (define/public file-menu:after-quit (λ (menu) (void)))
   (define/public
    edit-menu:undo-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'undo)))
      #t))
   (define/public (edit-menu:get-undo-item) edit-menu:undo-item)
   (define/public (edit-menu:undo-string) (string-constant undo-menu-item))
   (define/public (edit-menu:undo-help-string) (string-constant undo-info))
   (define/public
    edit-menu:undo-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'undo))))
        (send item enable enable?))))
   (define/public (edit-menu:create-undo?) #t)
   (define/public
    edit-menu:redo-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'redo)))
      #t))
   (define/public (edit-menu:get-redo-item) edit-menu:redo-item)
   (define/public (edit-menu:redo-string) (string-constant redo-menu-item))
   (define/public (edit-menu:redo-help-string) (string-constant redo-info))
   (define/public
    edit-menu:redo-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'redo))))
        (send item enable enable?))))
   (define/public (edit-menu:create-redo?) #t)
   (define/public
    edit-menu:between-redo-and-cut
    (λ (menu) (make-object separator-menu-item% menu)))
   (define/public
    edit-menu:cut-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'cut)))
      #t))
   (define/public (edit-menu:get-cut-item) edit-menu:cut-item)
   (define/public (edit-menu:cut-string) (string-constant cut-menu-item))
   (define/public (edit-menu:cut-help-string) (string-constant cut-info))
   (define/public
    edit-menu:cut-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'cut))))
        (send item enable enable?))))
   (define/public (edit-menu:create-cut?) #t)
   (define/public edit-menu:between-cut-and-copy (λ (menu) (void)))
   (define/public
    edit-menu:copy-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'copy)))
      #t))
   (define/public (edit-menu:get-copy-item) edit-menu:copy-item)
   (define/public (edit-menu:copy-string) (string-constant copy-menu-item))
   (define/public (edit-menu:copy-help-string) (string-constant copy-info))
   (define/public
    edit-menu:copy-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'copy))))
        (send item enable enable?))))
   (define/public (edit-menu:create-copy?) #t)
   (define/public edit-menu:between-copy-and-paste (λ (menu) (void)))
   (define/public
    edit-menu:paste-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'paste)))
      #t))
   (define/public (edit-menu:get-paste-item) edit-menu:paste-item)
   (define/public (edit-menu:paste-string) (string-constant paste-menu-item))
   (define/public (edit-menu:paste-help-string) (string-constant paste-info))
   (define/public
    edit-menu:paste-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'paste))))
        (send item enable enable?))))
   (define/public (edit-menu:create-paste?) #t)
   (define/public edit-menu:between-paste-and-clear (λ (menu) (void)))
   (define/public
    edit-menu:clear-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'clear)))
      #t))
   (define/public (edit-menu:get-clear-item) edit-menu:clear-item)
   (define/public
    (edit-menu:clear-string)
    (if (eq? (system-type) 'windows)
      (string-constant clear-menu-item-windows)
      (string-constant clear-menu-item-windows)))
   (define/public (edit-menu:clear-help-string) (string-constant clear-info))
   (define/public
    edit-menu:clear-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'clear))))
        (send item enable enable?))))
   (define/public (edit-menu:create-clear?) #t)
   (define/public edit-menu:between-clear-and-select-all (λ (menu) (void)))
   (define/public
    edit-menu:select-all-callback
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'select-all)))
      #t))
   (define/public (edit-menu:get-select-all-item) edit-menu:select-all-item)
   (define/public
    (edit-menu:select-all-string)
    (string-constant select-all-menu-item))
   (define/public
    (edit-menu:select-all-help-string)
    (string-constant select-all-info))
   (define/public
    edit-menu:select-all-on-demand
    (λ (item)
      (let* ((editor (get-edit-target-object))
             (enable?
              (and editor
                   (is-a? editor editor<%>)
                   (send editor can-do-edit-operation? 'select-all))))
        (send item enable enable?))))
   (define/public (edit-menu:create-select-all?) #t)
   (define/public
    edit-menu:between-select-all-and-find
    (λ (menu) (make-object separator-menu-item% menu)))
   (define/public edit-menu:find-callback (λ (item control) (void)))
   (define/public (edit-menu:get-find-item) edit-menu:find-item)
   (define/public (edit-menu:find-string) (string-constant find-menu-item))
   (define/public (edit-menu:find-help-string) (string-constant find-info))
   (define/public
    edit-menu:find-on-demand
    (λ (item)
      (send item enable
        (let
         ((target (get-edit-target-object)))
         (and target (is-a? target editor<%>))))))
   (define/public (edit-menu:create-find?) #f)
   (define/public edit-menu:find-next-callback (λ (item control) (void)))
   (define/public (edit-menu:get-find-next-item) edit-menu:find-next-item)
   (define/public
    (edit-menu:find-next-string)
    (string-constant find-next-menu-item))
   (define/public
    (edit-menu:find-next-help-string)
    (string-constant find-next-info))
   (define/public
    edit-menu:find-next-on-demand
    (λ (item)
      (send item enable
        (let
         ((target (get-edit-target-object)))
         (and target (is-a? target editor<%>))))))
   (define/public (edit-menu:create-find-next?) #f)
   (define/public edit-menu:find-previous-callback (λ (item control) (void)))
   (define/public
    (edit-menu:get-find-previous-item)
    edit-menu:find-previous-item)
   (define/public
    (edit-menu:find-previous-string)
    (string-constant find-previous-menu-item))
   (define/public
    (edit-menu:find-previous-help-string)
    (string-constant find-previous-info))
   (define/public
    edit-menu:find-previous-on-demand
    (λ (item)
      (send item enable
        (let
         ((target (get-edit-target-object)))
         (and target (is-a? target editor<%>))))))
   (define/public (edit-menu:create-find-previous?) #f)
   (define/public
    edit-menu:show/hide-replace-callback
    (λ (item control) (void)))
   (define/public
    (edit-menu:get-show/hide-replace-item)
    edit-menu:show/hide-replace-item)
   (define/public
    (edit-menu:show/hide-replace-string)
    (string-constant show-replace-menu-item))
   (define/public
    (edit-menu:show/hide-replace-help-string)
    (string-constant show/hide-replace-info))
   (define/public edit-menu:show/hide-replace-on-demand (λ (menu-item) (void)))
   (define/public (edit-menu:create-show/hide-replace?) #f)
   (define/public edit-menu:replace-callback (λ (item control) (void)))
   (define/public (edit-menu:get-replace-item) edit-menu:replace-item)
   (define/public
    (edit-menu:replace-string)
    (string-constant replace-menu-item))
   (define/public
    (edit-menu:replace-help-string)
    (string-constant replace-info))
   (define/public edit-menu:replace-on-demand (λ (menu-item) (void)))
   (define/public (edit-menu:create-replace?) #f)
   (define/public edit-menu:replace-all-callback (λ (item control) (void)))
   (define/public (edit-menu:get-replace-all-item) edit-menu:replace-all-item)
   (define/public
    (edit-menu:replace-all-string)
    (string-constant replace-all-menu-item))
   (define/public
    (edit-menu:replace-all-help-string)
    (string-constant replace-all-info))
   (define/public edit-menu:replace-all-on-demand (λ (menu-item) (void)))
   (define/public (edit-menu:create-replace-all?) #f)
   (define/public
    edit-menu:find-case-sensitive-callback
    (λ (item control) (void)))
   (define/public
    (edit-menu:get-find-case-sensitive-item)
    edit-menu:find-case-sensitive-item)
   (define/public
    (edit-menu:find-case-sensitive-string)
    (string-constant find-case-sensitive-menu-item))
   (define/public
    (edit-menu:find-case-sensitive-help-string)
    (string-constant find-case-sensitive-info))
   (define/public
    edit-menu:find-case-sensitive-on-demand
    (λ (item)
      (send item enable
        (let
         ((target (get-edit-target-object)))
         (and target (is-a? target editor<%>))))))
   (define/public (edit-menu:create-find-case-sensitive?) #f)
   (define/public
    edit-menu:between-find-and-preferences
    (λ (menu)
      (unless (current-eventspace-has-standard-menus?)
        (make-object separator-menu-item% menu))))
   (define/public
    edit-menu:preferences-callback
    (λ (item control) (preferences:show-dialog) #t))
   (define/public (edit-menu:get-preferences-item) edit-menu:preferences-item)
   (define/public
    (edit-menu:preferences-string)
    (string-constant preferences-menu-item))
   (define/public
    (edit-menu:preferences-help-string)
    (string-constant preferences-info))
   (define/public edit-menu:preferences-on-demand (λ (menu-item) (void)))
   (define/public
    (edit-menu:create-preferences?)
    (not (current-eventspace-has-standard-menus?)))
   (define/public edit-menu:after-preferences (λ (menu) (void)))
   (define/public help-menu:before-about (λ (menu) (void)))
   (define/public help-menu:about-callback (λ (item control) (void)))
   (define/public (help-menu:get-about-item) help-menu:about-item)
   (define/public (help-menu:about-string) (string-constant about-menu-item))
   (define/public (help-menu:about-help-string) (string-constant about-info))
   (define/public help-menu:about-on-demand (λ (menu-item) (void)))
   (define/public (help-menu:create-about?) #f)
   (define/public help-menu:after-about (λ (menu) (void)))
   (super-instantiate ())
   (define file-menu
     (make-object (get-menu%)
       (string-constant file-menu-label)
       (get-menu-bar)))
   (define edit-menu
     (make-object (get-menu%)
       (string-constant edit-menu-label)
       (get-menu-bar)))
   (define help-menu
     (make-object (get-menu%)
       (string-constant help-menu-label)
       (get-menu-bar)))
   (define file-menu:new-item
     (and (file-menu:create-new?)
          (new
           (get-menu-item%)
           (label (file-menu:new-string))
           (parent file-menu)
           (callback
            (let ((file-menu:new-callback
                   (λ (item evt) (file-menu:new-callback item evt))))
              file-menu:new-callback))
           (shortcut #\n)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:new-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:new-on-demand menu-item))))))
   (file-menu:between-new-and-open (get-file-menu))
   (define file-menu:open-item
     (and (file-menu:create-open?)
          (new
           (get-menu-item%)
           (label (file-menu:open-string))
           (parent file-menu)
           (callback
            (let ((file-menu:open-callback
                   (λ (item evt) (file-menu:open-callback item evt))))
              file-menu:open-callback))
           (shortcut #\o)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:open-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:open-on-demand menu-item))))))
   (define file-menu:open-recent-item
     (and (file-menu:create-open-recent?)
          (new
           (get-menu%)
           (label (file-menu:open-recent-string))
           (parent file-menu)
           (help-string (file-menu:open-recent-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:open-recent-on-demand menu-item))))))
   (file-menu:between-open-and-revert (get-file-menu))
   (define file-menu:revert-item
     (and (file-menu:create-revert?)
          (new
           (get-menu-item%)
           (label (file-menu:revert-string))
           (parent file-menu)
           (callback
            (let ((file-menu:revert-callback
                   (λ (item evt) (file-menu:revert-callback item evt))))
              file-menu:revert-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:revert-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:revert-on-demand menu-item))))))
   (file-menu:between-revert-and-save (get-file-menu))
   (define file-menu:save-item
     (and (file-menu:create-save?)
          (new
           (get-menu-item%)
           (label (file-menu:save-string))
           (parent file-menu)
           (callback
            (let ((file-menu:save-callback
                   (λ (item evt) (file-menu:save-callback item evt))))
              file-menu:save-callback))
           (shortcut #\s)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:save-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:save-on-demand menu-item))))))
   (define file-menu:save-as-item
     (and (file-menu:create-save-as?)
          (new
           (get-menu-item%)
           (label (file-menu:save-as-string))
           (parent file-menu)
           (callback
            (let ((file-menu:save-as-callback
                   (λ (item evt) (file-menu:save-as-callback item evt))))
              file-menu:save-as-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:save-as-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:save-as-on-demand menu-item))))))
   (file-menu:between-save-as-and-print (get-file-menu))
   (define file-menu:print-item
     (and (file-menu:create-print?)
          (new
           (get-menu-item%)
           (label (file-menu:print-string))
           (parent file-menu)
           (callback
            (let ((file-menu:print-callback
                   (λ (item evt) (file-menu:print-callback item evt))))
              file-menu:print-callback))
           (shortcut #\p)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:print-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:print-on-demand menu-item))))))
   (file-menu:between-print-and-close (get-file-menu))
   (define file-menu:close-item
     (and (file-menu:create-close?)
          (new
           (get-menu-item%)
           (label (file-menu:close-string))
           (parent file-menu)
           (callback
            (let ((file-menu:close-callback
                   (λ (item evt) (file-menu:close-callback item evt))))
              file-menu:close-callback))
           (shortcut #\w)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:close-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:close-on-demand menu-item))))))
   (file-menu:between-close-and-quit (get-file-menu))
   (define file-menu:quit-item
     (and (file-menu:create-quit?)
          (new
           (get-menu-item%)
           (label (file-menu:quit-string))
           (parent file-menu)
           (callback
            (let ((file-menu:quit-callback
                   (λ (item evt) (file-menu:quit-callback item evt))))
              file-menu:quit-callback))
           (shortcut #\q)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (file-menu:quit-help-string))
           (demand-callback
            (λ (menu-item) (file-menu:quit-on-demand menu-item))))))
   (file-menu:after-quit (get-file-menu))
   (define edit-menu:undo-item
     (and (edit-menu:create-undo?)
          (new
           (get-menu-item%)
           (label (edit-menu:undo-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:undo-callback
                   (λ (item evt) (edit-menu:undo-callback item evt))))
              edit-menu:undo-callback))
           (shortcut #\z)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:undo-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:undo-on-demand menu-item))))))
   (define edit-menu:redo-item
     (and (edit-menu:create-redo?)
          (new
           (get-menu-item%)
           (label (edit-menu:redo-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:redo-callback
                   (λ (item evt) (edit-menu:redo-callback item evt))))
              edit-menu:redo-callback))
           (shortcut (if (eq? (system-type) 'windows) #\y #\z))
           (shortcut-prefix
            (if (eq? (system-type) 'windows)
              (get-default-shortcut-prefix)
              (cons 'shift (get-default-shortcut-prefix))))
           (help-string (edit-menu:redo-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:redo-on-demand menu-item))))))
   (edit-menu:between-redo-and-cut (get-edit-menu))
   (define edit-menu:cut-item
     (and (edit-menu:create-cut?)
          (new
           (get-menu-item%)
           (label (edit-menu:cut-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:cut-callback
                   (λ (item evt) (edit-menu:cut-callback item evt))))
              edit-menu:cut-callback))
           (shortcut #\x)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:cut-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:cut-on-demand menu-item))))))
   (edit-menu:between-cut-and-copy (get-edit-menu))
   (define edit-menu:copy-item
     (and (edit-menu:create-copy?)
          (new
           (get-menu-item%)
           (label (edit-menu:copy-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:copy-callback
                   (λ (item evt) (edit-menu:copy-callback item evt))))
              edit-menu:copy-callback))
           (shortcut #\c)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:copy-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:copy-on-demand menu-item))))))
   (edit-menu:between-copy-and-paste (get-edit-menu))
   (define edit-menu:paste-item
     (and (edit-menu:create-paste?)
          (new
           (get-menu-item%)
           (label (edit-menu:paste-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:paste-callback
                   (λ (item evt) (edit-menu:paste-callback item evt))))
              edit-menu:paste-callback))
           (shortcut #\v)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:paste-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:paste-on-demand menu-item))))))
   (edit-menu:between-paste-and-clear (get-edit-menu))
   (define edit-menu:clear-item
     (and (edit-menu:create-clear?)
          (new
           (get-menu-item%)
           (label (edit-menu:clear-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:clear-callback
                   (λ (item evt) (edit-menu:clear-callback item evt))))
              edit-menu:clear-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:clear-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:clear-on-demand menu-item))))))
   (edit-menu:between-clear-and-select-all (get-edit-menu))
   (define edit-menu:select-all-item
     (and (edit-menu:create-select-all?)
          (new
           (get-menu-item%)
           (label (edit-menu:select-all-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:select-all-callback
                   (λ (item evt) (edit-menu:select-all-callback item evt))))
              edit-menu:select-all-callback))
           (shortcut #\a)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:select-all-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:select-all-on-demand menu-item))))))
   (edit-menu:between-select-all-and-find (get-edit-menu))
   (define edit-menu:find-item
     (and (edit-menu:create-find?)
          (new
           (get-menu-item%)
           (label (edit-menu:find-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:find-callback
                   (λ (item evt) (edit-menu:find-callback item evt))))
              edit-menu:find-callback))
           (shortcut #\f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:find-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:find-on-demand menu-item))))))
   (define edit-menu:find-next-item
     (and (edit-menu:create-find-next?)
          (new
           (get-menu-item%)
           (label (edit-menu:find-next-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:find-next-callback
                   (λ (item evt) (edit-menu:find-next-callback item evt))))
              edit-menu:find-next-callback))
           (shortcut #\g)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:find-next-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:find-next-on-demand menu-item))))))
   (define edit-menu:find-previous-item
     (and (edit-menu:create-find-previous?)
          (new
           (get-menu-item%)
           (label (edit-menu:find-previous-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:find-previous-callback
                   (λ (item evt) (edit-menu:find-previous-callback item evt))))
              edit-menu:find-previous-callback))
           (shortcut #\g)
           (shortcut-prefix (cons 'shift (get-default-shortcut-prefix)))
           (help-string (edit-menu:find-previous-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:find-previous-on-demand menu-item))))))
   (define edit-menu:show/hide-replace-item
     (and (edit-menu:create-show/hide-replace?)
          (new
           (get-menu-item%)
           (label (edit-menu:show/hide-replace-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:show/hide-replace-callback
                   (λ (item evt)
                     (edit-menu:show/hide-replace-callback item evt))))
              edit-menu:show/hide-replace-callback))
           (shortcut #\r)
           (shortcut-prefix (cons 'shift (get-default-shortcut-prefix)))
           (help-string (edit-menu:show/hide-replace-help-string))
           (demand-callback
            (λ (menu-item)
              (edit-menu:show/hide-replace-on-demand menu-item))))))
   (define edit-menu:replace-item
     (and (edit-menu:create-replace?)
          (new
           (get-menu-item%)
           (label (edit-menu:replace-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:replace-callback
                   (λ (item evt) (edit-menu:replace-callback item evt))))
              edit-menu:replace-callback))
           (shortcut #\r)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:replace-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:replace-on-demand menu-item))))))
   (define edit-menu:replace-all-item
     (and (edit-menu:create-replace-all?)
          (new
           (get-menu-item%)
           (label (edit-menu:replace-all-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:replace-all-callback
                   (λ (item evt) (edit-menu:replace-all-callback item evt))))
              edit-menu:replace-all-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:replace-all-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:replace-all-on-demand menu-item))))))
   (define edit-menu:find-case-sensitive-item
     (and (edit-menu:create-find-case-sensitive?)
          (new
           (get-checkable-menu-item%)
           (label (edit-menu:find-case-sensitive-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:find-case-sensitive-callback
                   (λ (item evt)
                     (edit-menu:find-case-sensitive-callback item evt))))
              edit-menu:find-case-sensitive-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:find-case-sensitive-help-string))
           (demand-callback
            (λ (menu-item)
              (edit-menu:find-case-sensitive-on-demand menu-item))))))
   (edit-menu:between-find-and-preferences (get-edit-menu))
   (define edit-menu:preferences-item
     (and (edit-menu:create-preferences?)
          (new
           (get-menu-item%)
           (label (edit-menu:preferences-string))
           (parent edit-menu)
           (callback
            (let ((edit-menu:preferences-callback
                   (λ (item evt) (edit-menu:preferences-callback item evt))))
              edit-menu:preferences-callback))
           (shortcut (case (system-type) ((macosx) #\,) (else #\;)))
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (edit-menu:preferences-help-string))
           (demand-callback
            (λ (menu-item) (edit-menu:preferences-on-demand menu-item))))))
   (edit-menu:after-preferences (get-edit-menu))
   (help-menu:before-about (get-help-menu))
   (define help-menu:about-item
     (and (help-menu:create-about?)
          (new
           (get-menu-item%)
           (label (help-menu:about-string))
           (parent help-menu)
           (callback
            (let ((help-menu:about-callback
                   (λ (item evt) (help-menu:about-callback item evt))))
              help-menu:about-callback))
           (shortcut #f)
           (shortcut-prefix (get-default-shortcut-prefix))
           (help-string (help-menu:about-help-string))
           (demand-callback
            (λ (menu-item) (help-menu:about-on-demand menu-item))))))
   (help-menu:after-about (get-help-menu))
   (reorder-menus this)))
