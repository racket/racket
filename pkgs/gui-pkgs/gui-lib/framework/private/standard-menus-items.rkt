#lang at-exp racket/base

(provide
 (struct-out generic)

 (struct-out generic/docs)

 (struct-out generic-override)
 (struct-out generic-augment)
 (struct-out generic-method)
 (struct-out generic-private-field)

 (struct-out menu-item)
 menu-name->get-menu-name ;; : menu-item -> symbol

 (struct-out before/after)
 (struct-out before)
 (struct-out after)

 (struct-out between)

 (struct-out an-item)
 (struct-out a-checkable-item)
 (struct-out a-submenu-item)

 ;; an-item -> symbol
 ;; calcualates the names of various identifiers associated with the item.
 an-item->callback-name
 an-item->create-menu-item-name
 an-item->get-item-name
 an-item->item-name
 an-item->on-demand-name
 an-item->string-name
 an-item->help-string-name

 before/after->name
 between->name

 items)

(define-struct generic (name initializer))
(define-struct (generic/docs generic) (documentation))
(define-struct (generic-override generic/docs) ())
(define-struct (generic-augment generic/docs) ())
(define-struct (generic-method generic/docs) ())
(define-struct (generic-private-field generic) ())

(define-struct menu-item (menu-name))
(define (menu-name->get-menu-name menu-item)
  (string->symbol
   (format "get-~a" (menu-item-menu-name menu-item))))

(define-struct (before/after menu-item) (name procedure))
(define-struct (before before/after) ())
(define-struct (after before/after) ())
(define (before/after->name before/after)
  (string->symbol (format "~a:~a-~a"
                          (menu-item-menu-name before/after)
                          (if (before? before/after)
                              "before"
                              "after")
                          (before/after-name before/after))))

(define-struct (between menu-item) (before after procedure))
(define (between->name between)
  (string->symbol (format "~a:between-~a-and-~a"
                          (menu-item-menu-name between)
                          (between-before between)
                          (between-after between))))

(define-struct (an-item menu-item)
  (item-name
   help-string
   proc
   shortcut
   shortcut-prefix
   menu-string
   on-demand
   create))
(define-struct (a-submenu-item an-item) ())
(define-struct (a-checkable-item an-item) ())

(define (an-item->callback-name item)
  (string->symbol
   (format "~a:~a-callback" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->create-menu-item-name item)
  (string->symbol
   (format "~a:create-~a?" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->get-item-name item)
  (string->symbol
   (format "~a:get-~a-item" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->item-name item)
  (string->symbol
   (format "~a:~a-item" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->on-demand-name item)
  (string->symbol
   (format "~a:~a-on-demand" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->string-name item)
  (string->symbol
   (format "~a:~a-string" (menu-item-menu-name item) (an-item-item-name item))))
(define (an-item->help-string-name item)
  (string->symbol
   (format "~a:~a-help-string" (menu-item-menu-name item) (an-item-item-name item))))

(define (edit-menu:do const)
  `(λ (menu evt)
     (let ([edit (get-edit-target-object)])
       (when (and edit
                  (is-a? edit editor<%>))
         (send edit do-edit-operation ',const)))
     #t))

(define (edit-menu:can-do-on-demand const)
  `(λ (item)
     (let* ([editor (get-edit-target-object)]
            [enable?
             (and editor
                  (is-a? editor editor<%>)
                  (send editor can-do-edit-operation? ',const))])
       (send item enable enable?))))

(define edit-menu:edit-target-on-demand
  `(λ (item)
     (send item enable
           (let ([target (get-edit-target-object)])
             (and target (is-a? target editor<%>))))))

(define on-demand-do-nothing '(λ (menu-item) (void)))

(define items
  (list (make-generic-augment
         'on-close
         '(λ ()
            (remove-prefs-callback)
            (inner (void) on-close))
         (list
          '@defmethod[(on-close) void?]{
             Removes the preferences callbacks for the menu items
           }))
        (make-generic-method
         'get-menu% '(λ () menu:can-restore-underscore-menu%)
         (list
          '@defmethod[(get-menu%)
                      (is-a?/c menu:can-restore-underscore-menu%)]{
             The result of this method is used as the class
             for creating the result of these methods:
             @method[frame:standard-menus get-file-menu],
             @method[frame:standard-menus get-edit-menu], and
             @method[frame:standard-menus get-help-menu].}))
        (make-generic-method
         'get-menu-item% '(λ () menu:can-restore-menu-item%)
         (list
          '@defmethod[(get-menu-item%) (is-a?/c menu:can-restore-menu-item%)]{
             The result of this method is used as the class for creating
             the menu items in this frame.

             Returns @racket[menu:can-restore-menu-item] by default.}))
        (make-generic-method
         'get-checkable-menu-item% '(λ () menu:can-restore-checkable-menu-item%)
         (list
          '@defmethod[(get-checkable-menu-item%) (is-a?/c menu:can-restore-checkable-menu-item%)]{
             The result of this method is used as the class for creating
             checkable menu items in this class.

             returns @racket[menu:can-restore-checkable-menu-item] by default.}))

        (make-generic-method
         'get-file-menu
         '(λ () file-menu)
         (list
          '@defmethod[(get-file-menu) (is-a?/c menu%)]{
             Returns the file menu.
             See also @method[frame:standard-menus<%> get-menu%].}))

        (make-generic-private-field
         'file-menu
         '(make-object (get-menu%)
            (string-constant file-menu-label)
            (get-menu-bar)))
        (make-generic-method
         'get-edit-menu
         '(λ () edit-menu)
         (list
          '@defmethod[(get-edit-menu) (is-a?/c menu%)]{
             Returns the edit menu.
             See also @method[frame:standard-menus<%> get-menu%].}))
        (make-generic-private-field
         'edit-menu
         '(make-object (get-menu%) (string-constant edit-menu-label) (get-menu-bar)))
        (make-generic-method
         'get-help-menu
         '(λ () help-menu)
         (list
          '@defmethod[(get-help-menu) (is-a?/c menu%)]{
             Returns the help menu.
             See also @method[frame:standard-menus<%> get-menu%].}))
        (make-generic-private-field
         'help-menu
         '(make-object (get-menu%) (string-constant help-menu-label) (get-menu-bar)))

        (make-an-item 'file-menu 'new
                      '(string-constant new-info)
                      '(λ (item control) (handler:edit-file #f) #t)
                      #\n
                      '(get-default-shortcut-prefix)
                      '(string-constant new-menu-item)
                      on-demand-do-nothing
                      #t)
        (make-between 'file-menu 'new 'open 'nothing)
        (make-an-item 'file-menu 'open '(string-constant open-info)
                      '(λ (item control) (handler:open-file) #t)
                      #\o
                      '(get-default-shortcut-prefix)
                      '(string-constant open-menu-item)
                      on-demand-do-nothing
                      #t)
        (make-a-submenu-item 'file-menu 'open-recent
                             '(string-constant open-recent-info)
                             '(λ (x y) (void))
                             #f
                             '(get-default-shortcut-prefix)
                             '(string-constant open-recent-menu-item)
                             '(λ (menu)
                                (handler:install-recent-items menu))
                             #t)
        (make-between 'file-menu 'open 'revert 'nothing)
        (make-an-item 'file-menu 'revert
                      '(string-constant revert-info)
                      '(λ (item control) (void))
                      #f
                      '(get-default-shortcut-prefix)
                      '(string-constant revert-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-between 'file-menu 'revert 'save 'nothing)
        (make-an-item 'file-menu 'save
                      '(string-constant save-info)
                      '(λ (item control) (void))
                      #\s
                      '(get-default-shortcut-prefix)
                      '(string-constant save-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-an-item 'file-menu 'save-as
                      '(string-constant save-as-info)
                      '(λ (item control) (void))
                      #\s
                      '(cons 'shift (get-default-shortcut-prefix))
                      '(string-constant save-as-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-between 'file-menu 'save-as 'print 'nothing)
        (make-an-item 'file-menu 'print
                      '(string-constant print-info)
                      '(λ (item control) (void))
                      #\p
                      '(get-default-shortcut-prefix)
                      '(string-constant print-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-between 'file-menu 'print 'close 'separator)
        (make-an-item 'file-menu 'close
                      '(string-constant close-info)
                      '(λ (item control) (when (can-close?) (on-close) (show #f)) #t)
                      #\w
                      '(get-default-shortcut-prefix)
                      '(if (eq? (system-type) 'unix)
                           (string-constant close-menu-item)
                           (string-constant close-window-menu-item))
                      on-demand-do-nothing
                      #t)
        (make-between 'file-menu 'close 'quit 'nothing)
        (make-an-item 'file-menu 'quit
                      '(string-constant quit-info)
                      '(λ (item control)
                         (when (exit:user-oks-exit)
                           (exit:exit)))
                      #\q
                      '(get-default-shortcut-prefix)
                      '(if (eq? (system-type) 'windows)
                           (string-constant quit-menu-item-windows)
                           (string-constant quit-menu-item-others))
                      on-demand-do-nothing
                      '(not (eq? (system-type) 'macosx)))
        (make-after 'file-menu 'quit 'nothing)

        (make-an-item 'edit-menu 'undo
                      '(string-constant undo-info)
                      (edit-menu:do  'undo)
                      #\z
                      '(get-default-shortcut-prefix)
                      '(string-constant undo-menu-item)
                      (edit-menu:can-do-on-demand 'undo)
                      #t)
        (make-an-item 'edit-menu 'redo
                      '(string-constant redo-info)
                      (edit-menu:do 'redo)
                      '(if (eq? (system-type) 'windows)
                           #\y
                           #\z)
                      '(if (eq? (system-type) 'windows)
                           (get-default-shortcut-prefix)
                           (cons 'shift (get-default-shortcut-prefix)))
                      '(string-constant redo-menu-item)
                      (edit-menu:can-do-on-demand 'redo)
                      #t)
        (make-between 'edit-menu 'redo 'cut 'separator)
        (make-an-item 'edit-menu 'cut '(string-constant cut-info)
                      (edit-menu:do 'cut)
                      #\x
                      '(get-default-shortcut-prefix)
                      '(string-constant cut-menu-item)
                      (edit-menu:can-do-on-demand 'cut)
                      #t)
        (make-between 'edit-menu 'cut 'copy 'nothing)
        (make-an-item 'edit-menu 'copy
                      '(string-constant copy-info)
                      (edit-menu:do 'copy)
                      #\c
                      '(get-default-shortcut-prefix)
                      '(string-constant copy-menu-item)
                      (edit-menu:can-do-on-demand 'copy)
                      #t)
        (make-between 'edit-menu 'copy 'paste 'nothing)
        (make-an-item 'edit-menu 'paste
                      '(string-constant paste-info)
                      (edit-menu:do 'paste)
                      #\v
                      '(get-default-shortcut-prefix)
                      '(string-constant paste-menu-item)
                      (edit-menu:can-do-on-demand 'paste)
                      #t)
        (make-between 'edit-menu 'paste 'clear 'nothing)
        (make-an-item 'edit-menu 'clear
                      '(string-constant clear-info)
                      (edit-menu:do 'clear)
                      #f
                      '(get-default-shortcut-prefix)
                      '(if (eq? (system-type) 'windows)
                           (string-constant clear-menu-item-windows)
                           (string-constant clear-menu-item-windows))
                      (edit-menu:can-do-on-demand 'clear)
                      #t)
        (make-between 'edit-menu 'clear 'select-all 'nothing)
        (make-an-item 'edit-menu 'select-all
                      '(string-constant select-all-info)
                      (edit-menu:do 'select-all)
                      #\a
                      '(get-default-shortcut-prefix)
                      '(string-constant select-all-menu-item)
                      (edit-menu:can-do-on-demand 'select-all)
                      #t)
        (make-between 'edit-menu 'select-all 'find 'separator)

        (make-an-item 'edit-menu 'find
                      '(string-constant find-info)
                      '(λ (item control) (void))
                      #\f
                      '(get-default-shortcut-prefix)
                      '(string-constant find-menu-item)
                      edit-menu:edit-target-on-demand
                      #f)
        (make-an-item 'edit-menu 'find-from-selection
                      '(string-constant find-info)
                      '(λ (item control) (void))
                      '(if (eq? (system-type) 'macosx)
                           #\f
                           #f)
                      '(if (eq? (system-type) 'macosx)
                           (cons 'option (get-default-shortcut-prefix))
                           (get-default-shortcut-prefix))
                      '(string-constant find-from-selection-menu-item)
                      edit-menu:edit-target-on-demand
                      #f)

        (make-an-item 'edit-menu 'find-next
                      '(string-constant find-next-info)
                      '(λ (item control) (void))
                      #\g
                      '(get-default-shortcut-prefix)
                      '(string-constant find-next-menu-item)
                      edit-menu:edit-target-on-demand
                      #f)
        (make-an-item 'edit-menu 'find-previous
                      '(string-constant find-previous-info)
                      '(λ (item control) (void))
                      #\g
                      '(cons 'shift (get-default-shortcut-prefix))
                      '(string-constant find-previous-menu-item)
                      edit-menu:edit-target-on-demand
                      #f)
        (make-an-item 'edit-menu 'show/hide-replace
                      '(string-constant show/hide-replace-info)
                      '(λ (item control) (void))
                      #\r
                      '(cons 'shift (get-default-shortcut-prefix))
                      '(string-constant show-replace-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-an-item 'edit-menu 'replace
                      '(string-constant replace-info)
                      '(λ (item control) (void))
                      #\f
                      '(cons 'shift (get-default-shortcut-prefix))
                      '(string-constant replace-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-an-item 'edit-menu 'replace-all
                      '(string-constant replace-all-info)
                      '(λ (item control) (void))
                      #f
                      '(get-default-shortcut-prefix)
                      '(string-constant replace-all-menu-item)
                      on-demand-do-nothing
                      #f)

        (make-a-checkable-item 'edit-menu 'find-case-sensitive
                               '(string-constant find-case-sensitive-info)
                               '(λ (item control) (void))
                               #f
                               '(get-default-shortcut-prefix)
                               '(string-constant find-case-sensitive-menu-item)
                               edit-menu:edit-target-on-demand
                               #f)

        (make-between 'edit-menu 'find 'preferences 'nothing-with-standard-menus)
        (make-an-item 'edit-menu 'preferences
                      '(string-constant preferences-info)
                      '(λ (item control) (preferences:show-dialog) #t)
                      '(case (system-type)
                         [(macosx) #\,]
                         [else #\;])
                      '(get-default-shortcut-prefix)
                      '(string-constant preferences-menu-item)
                      on-demand-do-nothing
                      '(not (current-eventspace-has-standard-menus?)))
        (make-after 'edit-menu 'preferences 'nothing)

        (make-before 'help-menu 'about 'nothing)
        (make-an-item 'help-menu 'about
                      '(string-constant about-info)
                      '(λ (item control) (void))
                      #f
                      '(get-default-shortcut-prefix)
                      '(string-constant about-menu-item)
                      on-demand-do-nothing
                      #f)
        (make-after 'help-menu 'about 'nothing)))
