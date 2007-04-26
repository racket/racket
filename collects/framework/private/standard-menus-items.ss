(module standard-menus-items mzscheme

  (provide
   (struct generic (name initializer))
   
   (struct generic/docs (documentation))
   
   (struct generic-override ())
   (struct generic-augment ())
   (struct generic-method ())
   (struct generic-private-field ())
   
   (struct menu-item (menu-name))
   menu-name->get-menu-name ;; : menu-item -> symbol
   
   (struct before/after (name procedure))
   (struct before ())
   (struct after ())
   
   (struct between (before after procedure))
   
   (struct an-item (item-name help-string proc key menu-string on-demand create))
   (struct a-submenu-item ()) 
   
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
                  key
                  menu-string
                  on-demand
                  create))
  (define-struct (a-submenu-item an-item) ())
  
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
           '(lambda ()
              (remove-prefs-callback)
              (inner (void) on-close))
           '("@return : void"
             "Removes the preferences callbacks for the menu items"))
          (make-generic-method 
           'get-menu% '(λ () menu:can-restore-underscore-menu%)
           '("The result of this method is used as the class"
             "for creating the result of these methods:"
             "@ilink frame:standard-menus get-file-menu %"
             ", "
             "@ilink frame:standard-menus get-edit-menu %"
             ", "
             "@ilink frame:standard-menus get-help-menu %"
             ". "
             ""
             "@return : (derived-from \\iscmclass{menu:can-restore-underscore-menu})"
             ""
             "defaultly returns"
             "@link menu"))
          (make-generic-method 
           'get-menu-item% '(λ () menu:can-restore-menu-item%)
           '("The result of this method is used as the class for creating"
             "the menu items in this frame (see "
             "@link frame:standard-menus"
             "for a list)."
             ""
             "@return : (derived-from \\iscmclass{menu-item})"
             ""
             "defaultly returns"
             "@link menu:can-restore-menu-item %"
             "."))
          (make-generic-method 
           'get-checkable-menu-item% '(λ () menu:can-restore-checkable-menu-item%)
           '("The result of this method is used as the class for creating"
             "checkable menu items in this class (see "
             "@link frame:standard-menus"
             "for a list)."
             ""
             "@return : (derived-from \\iscmclass{checkable-menu-item})"
             ""
             "defaultly returns"
             "@link menu:can-restore-checkable-menu-item %"
             "."))
          
          (make-generic-method 
           'get-file-menu
           '(λ () file-menu)
           '("Returns the file menu"
             "See also"
             "@ilink frame:standard-menus get-menu\\%"
             ""
             "@return : (instance (derived-from \\iscmclass{menu}))"))
          (make-generic-private-field 
           'file-menu
           '(make-object (get-menu%)
              (string-constant file-menu-label)
              (get-menu-bar)))
          (make-generic-method
           'get-edit-menu
           '(λ () edit-menu)
           
           '("Returns the edit menu"
             "See also"
             "@ilink frame:standard-menus get-menu\\%"
             ""
             "@return : (instance (derived-from \\iscmclass{menu}))"))
          (make-generic-private-field 
           'edit-menu
           '(make-object (get-menu%) (string-constant edit-menu-label) (get-menu-bar)))
          (make-generic-method
           'get-help-menu
           '(λ () help-menu)
           
           '("Returns the help menu"
             "See also"
             "@ilink frame:standard-menus get-menu\\%"
             ""
             "@return : (instance (derived-from \\iscmclass{menu}))"))
          (make-generic-private-field
           'help-menu
           '(make-object (get-menu%) (string-constant help-menu-label) (get-menu-bar)))
          
          (make-an-item 'file-menu 'new 
                        '(string-constant new-info)
                        '(λ (item control) (handler:edit-file #f) #t)
                        #\n 
                        '(string-constant new-menu-item)
                        on-demand-do-nothing
                        #t)
          (make-between 'file-menu 'new 'open 'nothing)
          (make-an-item 'file-menu 'open '(string-constant open-info)
                        '(λ (item control) (handler:open-file) #t)
                        #\o 
                        '(string-constant open-menu-item)
                        on-demand-do-nothing
                        #t)
          (make-a-submenu-item 'file-menu 'open-recent 
                               '(string-constant open-recent-info)
                               '(λ (x y) (void))
                               #f
                               '(string-constant open-recent-menu-item)
                               '(λ (menu)
                                  (handler:install-recent-items menu))
                               #t)
          (make-between 'file-menu 'open 'revert 'nothing)
          (make-an-item 'file-menu 'revert 
                        '(string-constant revert-info)
                        '(λ (item control) (void))
                        #f 
                        '(string-constant revert-menu-item)
                        on-demand-do-nothing
                        #f)
          (make-between 'file-menu 'revert 'save 'nothing)
          (make-an-item 'file-menu 'save
                        '(string-constant save-info)
                        '(λ (item control) (void))
                        #\s 
                        '(string-constant save-menu-item)
                        on-demand-do-nothing
                        #f)
          (make-an-item 'file-menu 'save-as
                        '(string-constant save-as-info)
                        '(λ (item control) (void))
                        #f 
                        '(string-constant save-as-menu-item)
                        on-demand-do-nothing
                        #f)
          (make-between 'file-menu 'save-as 'print 'nothing)
          (make-an-item 'file-menu 'print
                        '(string-constant print-info)
                        '(λ (item control) (void))
                        #\p 
                        '(string-constant print-menu-item)
                        on-demand-do-nothing
                        #f)
          (make-between 'file-menu 'print 'close 'separator)
          (make-an-item 'file-menu 'close
                        '(string-constant close-info)
                        '(λ (item control) (when (can-close?) (on-close) (show #f)) #t)
                        #\w
                        '(string-constant close-menu-item)
                        on-demand-do-nothing
                        #t)
          (make-between 'file-menu 'close 'quit 'nothing)
          (make-an-item 'file-menu 'quit
                        '(string-constant quit-info)
                        '(λ (item control) 
                           (when (exit:user-oks-exit)
                             (exit:exit)))
                        #\q
                        '(if (eq? (system-type) 'windows) 
                             (string-constant quit-menu-item-windows)
                             (string-constant quit-menu-item-others))
                        on-demand-do-nothing
                        '(not (current-eventspace-has-standard-menus?)))
          (make-after 'file-menu 'quit 'nothing)
          
          (make-an-item 'edit-menu 'undo 
                        '(string-constant undo-info)
                        (edit-menu:do  'undo)
                        #\z 
                        '(string-constant undo-menu-item)
                        (edit-menu:can-do-on-demand 'undo)
                        #t)
          (make-an-item 'edit-menu 'redo 
                        '(string-constant redo-info)
                        (edit-menu:do 'redo)
                        #\y 
                        '(string-constant redo-menu-item)
                        (edit-menu:can-do-on-demand 'redo)
                        #t)
          (make-between 'edit-menu 'redo 'cut 'separator)
          (make-an-item 'edit-menu 'cut '(string-constant cut-info)
                        (edit-menu:do 'cut)
                        #\x 
                        '(string-constant cut-menu-item)
                        (edit-menu:can-do-on-demand 'cut)
                        #t)
          (make-between 'edit-menu 'cut 'copy 'nothing)
          (make-an-item 'edit-menu 'copy 
                        '(string-constant copy-info)
                        (edit-menu:do 'copy)
                        #\c 
                        '(string-constant copy-menu-item)
                        (edit-menu:can-do-on-demand 'copy)
                        #t)
          (make-between 'edit-menu 'copy 'paste 'nothing)
          (make-an-item 'edit-menu 'paste 
                        '(string-constant paste-info)
                        (edit-menu:do 'paste)
                        #\v 
                        '(string-constant paste-menu-item)
                        (edit-menu:can-do-on-demand 'paste)
                        #t)
          (make-between 'edit-menu 'paste 'clear 'nothing)
          (make-an-item 'edit-menu 'clear 
                        '(string-constant clear-info)
                        (edit-menu:do 'clear)
                        #f
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
                        '(string-constant select-all-menu-item)
                        (edit-menu:can-do-on-demand 'select-all)
                        #t)
          (make-between 'edit-menu 'select-all 'find 'separator)

          (make-an-item 'edit-menu 'find  
                        '(string-constant find-info)
                        '(λ (item control) (void))
                        #\f 
                        '(string-constant find-menu-item)
                        edit-menu:edit-target-on-demand
                        #f)
          (make-an-item 'edit-menu 'find-again 
                        '(string-constant find-again-info)
                        '(λ (item control) (void))
                        #\g 
                        '(string-constant find-again-menu-item)
                        edit-menu:edit-target-on-demand
                        #f)
          (make-an-item 'edit-menu 'replace-and-find-again 
                        '(string-constant replace-and-find-again-info)
                        '(λ (item control) (void))
                        '(if (eq? (system-type) 'macosx) #f #\h)
                        '(string-constant replace-and-find-again-menu-item)
                        edit-menu:edit-target-on-demand
                        #f)
          
          (make-between 'edit-menu 'find 'preferences 'nothing-with-standard-menus)
          (make-an-item 'edit-menu 'preferences 
                        '(string-constant preferences-info)
                        '(λ (item control) (preferences:show-dialog) #t)
                        '(case (system-type)
                           [(macosx) #\,]
                           [else #\;])
                        '(string-constant preferences-menu-item)
                        on-demand-do-nothing
                        '(not (current-eventspace-has-standard-menus?)))
          (make-after 'edit-menu 'preferences 'nothing)
          
          (make-before 'help-menu 'about 'nothing)
          (make-an-item 'help-menu 'about 
                        '(string-constant about-info)
                        '(λ (item control) (void))
                        #f
                        '(string-constant about-menu-item)
                        on-demand-do-nothing
                        #f)
          (make-after 'help-menu 'about 'nothing))))
