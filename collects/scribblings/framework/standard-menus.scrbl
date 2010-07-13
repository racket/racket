;; THIS FILE IS GENERATED. DO NOT EDIT.

@definterface[frame:standard-menus<%> (frame:basic<%>)]{
 @(defmethod (on-close) void? "Removes the preferences callbacks for the menu items")

@(defmethod (get-menu%) (is-a?/c menu:can-restore-underscore-menu%) "The result of this method is used as the class" "\n" "    " "for creating the result of these methods:" "\n" "    " (method frame:standard-menus get-file-menu) "," "\n" "    " (method frame:standard-menus get-edit-menu) ", and" "\n" "    " (method frame:standard-menus get-help-menu) ".")

@(defmethod (get-menu-item%) (is-a?/c menu:can-restore-menu-item%) "The result of this method is used as the class for creating" "\n" "the menu items in this frame." "\n" "\n" "Defaultly returns " (scheme menu:can-restore-menu-item) ".")

@(defmethod (get-checkable-menu-item%) (is-a?/c menu:can-restore-checkable-menu-item%) "The result of this method is used as the class for creating" "\n" "checkable menu items in this class." "\n" "\n" "Defaultly returns " (scheme menu:can-restore-checkable-menu-item) ".")

@(defmethod (get-file-menu) (is-a?/c menu%) "Returns the file menu." "\n" "See also " (method frame:standard-menus<%> get-menu%) ".")

@(defmethod (get-edit-menu) (is-a?/c menu%) "Returns the edit menu." "\n" "See also " (method frame:standard-menus<%> get-menu%) ".")

@(defmethod (get-help-menu) (is-a?/c menu%) "Returns the help menu." "\n" "See also " (method frame:standard-menus<%> get-menu%) ".")

@(defmethod (file-menu:get-new-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-new?) ").")

@(defmethod (file-menu:create-new?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (file-menu:new-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (handler:edit-file #f) #t)) " ")

@(defmethod (file-menu:new-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:new-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant new-menu-item)) ".")

@(defmethod (file-menu:new-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant new-info)) ".")

@(defmethod (file-menu:between-new-and-open (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "new") " and the " (tt "open") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-open-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-open?) ").")

@(defmethod (file-menu:create-open?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (file-menu:open-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (handler:open-file) #t)) " ")

@(defmethod (file-menu:open-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:open-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant open-menu-item)) ".")

@(defmethod (file-menu:open-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant open-info)) ".")

@(defmethod (file-menu:get-open-recent-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-open-recent?) ").")

@(defmethod (file-menu:create-open-recent?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (file-menu:open-recent-callback (x (is-a?/c menu-item%)) (y (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (file-menu:open-recent-on-demand (menu (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (handler:install-recent-items menu)))

@(defmethod (file-menu:open-recent-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant open-recent-menu-item)) ".")

@(defmethod (file-menu:open-recent-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant open-recent-info)) ".")

@(defmethod (file-menu:between-open-and-revert (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "open") " and the " (tt "revert") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-revert-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-revert?) ").")

@(defmethod (file-menu:create-revert?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (file-menu:revert-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (file-menu:revert-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:revert-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant revert-menu-item)) ".")

@(defmethod (file-menu:revert-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant revert-info)) ".")

@(defmethod (file-menu:between-revert-and-save (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "revert") " and the " (tt "save") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-save-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-save?) ").")

@(defmethod (file-menu:create-save?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (file-menu:save-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (file-menu:save-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:save-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant save-menu-item)) ".")

@(defmethod (file-menu:save-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant save-info)) ".")

@(defmethod (file-menu:get-save-as-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-save-as?) ").")

@(defmethod (file-menu:create-save-as?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (file-menu:save-as-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (file-menu:save-as-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:save-as-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant save-as-menu-item)) ".")

@(defmethod (file-menu:save-as-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant save-as-info)) ".")

@(defmethod (file-menu:between-save-as-and-print (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "save-as") " and the " (tt "print") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-print-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-print?) ").")

@(defmethod (file-menu:create-print?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (file-menu:print-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (file-menu:print-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:print-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant print-menu-item)) ".")

@(defmethod (file-menu:print-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant print-info)) ".")

@(defmethod (file-menu:between-print-and-close (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "print") " and the " (tt "close") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-close-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-close?) ").")

@(defmethod (file-menu:create-close?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (file-menu:close-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (when (can-close?) (on-close) (show #f)) #t)) " ")

@(defmethod (file-menu:close-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:close-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant close-menu-item)) ".")

@(defmethod (file-menu:close-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant close-info)) ".")

@(defmethod (file-menu:between-close-and-quit (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "close") " and the " (tt "quit") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (file-menu:get-quit-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> file-menu:create-quit?) ").")

@(defmethod (file-menu:create-quit?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme (not (eq? (system-type) (quote macosx)))) ".")

@(defmethod (file-menu:quit-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (when (exit:user-oks-exit) (exit:exit))) " ")

@(defmethod (file-menu:quit-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (file-menu:quit-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (if (eq? (system-type) (quote windows)) (string-constant quit-menu-item-windows) (string-constant quit-menu-item-others))) ".")

@(defmethod (file-menu:quit-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant quit-info)) ".")

@(defmethod (file-menu:after-quit (menu (is-a?/c menu-item%))) void? "This method is called " "after" " the addition of the" "\n" (tt "quit") " menu-item. Override it to add additional" "\n" "menu items at that point. ")

@(defmethod (edit-menu:get-undo-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-undo?) ").")

@(defmethod (edit-menu:create-undo?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:undo-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote undo)))) #t)) " ")

@(defmethod (edit-menu:undo-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote undo))))) (send item enable enable?))))

@(defmethod (edit-menu:undo-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant undo-menu-item)) ".")

@(defmethod (edit-menu:undo-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant undo-info)) ".")

@(defmethod (edit-menu:get-redo-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-redo?) ").")

@(defmethod (edit-menu:create-redo?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:redo-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote redo)))) #t)) " ")

@(defmethod (edit-menu:redo-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote redo))))) (send item enable enable?))))

@(defmethod (edit-menu:redo-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant redo-menu-item)) ".")

@(defmethod (edit-menu:redo-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant redo-info)) ".")

@(defmethod (edit-menu:between-redo-and-cut (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "redo") " and the " (tt "cut") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-cut-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-cut?) ").")

@(defmethod (edit-menu:create-cut?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:cut-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote cut)))) #t)) " ")

@(defmethod (edit-menu:cut-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote cut))))) (send item enable enable?))))

@(defmethod (edit-menu:cut-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant cut-menu-item)) ".")

@(defmethod (edit-menu:cut-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant cut-info)) ".")

@(defmethod (edit-menu:between-cut-and-copy (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "cut") " and the " (tt "copy") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-copy-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-copy?) ").")

@(defmethod (edit-menu:create-copy?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:copy-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote copy)))) #t)) " ")

@(defmethod (edit-menu:copy-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote copy))))) (send item enable enable?))))

@(defmethod (edit-menu:copy-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant copy-menu-item)) ".")

@(defmethod (edit-menu:copy-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant copy-info)) ".")

@(defmethod (edit-menu:between-copy-and-paste (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "copy") " and the " (tt "paste") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-paste-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-paste?) ").")

@(defmethod (edit-menu:create-paste?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:paste-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote paste)))) #t)) " ")

@(defmethod (edit-menu:paste-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote paste))))) (send item enable enable?))))

@(defmethod (edit-menu:paste-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant paste-menu-item)) ".")

@(defmethod (edit-menu:paste-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant paste-info)) ".")

@(defmethod (edit-menu:between-paste-and-clear (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "paste") " and the " (tt "clear") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-clear-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-clear?) ").")

@(defmethod (edit-menu:create-clear?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:clear-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote clear)))) #t)) " ")

@(defmethod (edit-menu:clear-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote clear))))) (send item enable enable?))))

@(defmethod (edit-menu:clear-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (if (eq? (system-type) (quote windows)) (string-constant clear-menu-item-windows) (string-constant clear-menu-item-windows))) ".")

@(defmethod (edit-menu:clear-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant clear-info)) ".")

@(defmethod (edit-menu:between-clear-and-select-all (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "clear") " and the " (tt "select-all") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-select-all-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-select-all?) ").")

@(defmethod (edit-menu:create-select-all?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #t) ".")

@(defmethod (edit-menu:select-all-callback (menu (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (let ((edit (get-edit-target-object))) (when (and edit (is-a? edit editor<%>)) (send edit do-edit-operation (quote select-all)))) #t)) " ")

@(defmethod (edit-menu:select-all-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<%>) (send editor can-do-edit-operation? (quote select-all))))) (send item enable enable?))))

@(defmethod (edit-menu:select-all-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant select-all-menu-item)) ".")

@(defmethod (edit-menu:select-all-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant select-all-info)) ".")

@(defmethod (edit-menu:between-select-all-and-find (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "select-all") " and the " (tt "find") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-find-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-find?) ").")

@(defmethod (edit-menu:create-find?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:find-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:find-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<%>))))))

@(defmethod (edit-menu:find-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant find-menu-item)) ".")

@(defmethod (edit-menu:find-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant find-info)) ".")

@(defmethod (edit-menu:get-find-next-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-find-next?) ").")

@(defmethod (edit-menu:create-find-next?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:find-next-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:find-next-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<%>))))))

@(defmethod (edit-menu:find-next-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant find-next-menu-item)) ".")

@(defmethod (edit-menu:find-next-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant find-next-info)) ".")

@(defmethod (edit-menu:get-find-previous-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-find-previous?) ").")

@(defmethod (edit-menu:create-find-previous?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:find-previous-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:find-previous-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<%>))))))

@(defmethod (edit-menu:find-previous-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant find-previous-menu-item)) ".")

@(defmethod (edit-menu:find-previous-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant find-previous-info)) ".")

@(defmethod (edit-menu:get-show/hide-replace-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-show/hide-replace?) ").")

@(defmethod (edit-menu:create-show/hide-replace?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:show/hide-replace-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:show/hide-replace-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (edit-menu:show/hide-replace-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant show-replace-menu-item)) ".")

@(defmethod (edit-menu:show/hide-replace-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant show/hide-replace-info)) ".")

@(defmethod (edit-menu:get-replace-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-replace?) ").")

@(defmethod (edit-menu:create-replace?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:replace-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:replace-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (edit-menu:replace-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant replace-menu-item)) ".")

@(defmethod (edit-menu:replace-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant replace-info)) ".")

@(defmethod (edit-menu:get-replace-all-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-replace-all?) ").")

@(defmethod (edit-menu:create-replace-all?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:replace-all-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:replace-all-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (edit-menu:replace-all-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant replace-all-menu-item)) ".")

@(defmethod (edit-menu:replace-all-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant replace-all-info)) ".")

@(defmethod (edit-menu:get-find-case-sensitive-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-find-case-sensitive?) ").")

@(defmethod (edit-menu:create-find-case-sensitive?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (edit-menu:find-case-sensitive-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (edit-menu:find-case-sensitive-on-demand (item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<%>))))))

@(defmethod (edit-menu:find-case-sensitive-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant find-case-sensitive-menu-item)) ".")

@(defmethod (edit-menu:find-case-sensitive-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant find-case-sensitive-info)) ".")

@(defmethod (edit-menu:between-find-and-preferences (menu (is-a?/c menu-item%))) void? "This method is called between the addition of the" "\n" (tt "find") " and the " (tt "preferences") " menu-item." "\n" "Override it to add additional menu items at that point. ")

@(defmethod (edit-menu:get-preferences-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> edit-menu:create-preferences?) ").")

@(defmethod (edit-menu:create-preferences?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme (not (current-eventspace-has-standard-menus?))) ".")

@(defmethod (edit-menu:preferences-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (begin (preferences:show-dialog) #t)) " ")

@(defmethod (edit-menu:preferences-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (edit-menu:preferences-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant preferences-menu-item)) ".")

@(defmethod (edit-menu:preferences-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant preferences-info)) ".")

@(defmethod (edit-menu:after-preferences (menu (is-a?/c menu-item%))) void? "This method is called " "after" " the addition of the" "\n" (tt "preferences") " menu-item. Override it to add additional" "\n" "menu items at that point. ")

@(defmethod (help-menu:before-about (menu (is-a?/c menu-item%))) void? "This method is called " "before" " the addition of the" "\n" (tt "about") " menu-item. Override it to add additional" "\n" "menu items at that point. ")

@(defmethod (help-menu:get-about-item) (or/c false/c (is-a?/c menu-item%)) "This method returns the " (scheme menu-item%) " object corresponding" "\n" "to this menu item, if it has been created (as controlled by" "\n" (method frame:standard-menus<%> help-menu:create-about?) ").")

@(defmethod (help-menu:create-about?) boolean? "The result of this method determines if the corresponding" "\n" "menu item is created. Override it to control the creation of the menu item." "\n" "\n" "Defaults to " (scheme #f) ".")

@(defmethod (help-menu:about-callback (item (is-a?/c menu-item%)) (control (is-a?/c control-event%))) void? "Defaults to " (schemeblock (void)) " ")

@(defmethod (help-menu:about-on-demand (menu-item (is-a?/c menu-item%))) void? "The menu item's on-demand proc calls this method." "\n" "\n" "Defaults to " (schemeblock (void)))

@(defmethod (help-menu:about-string) string? "The result of this method is used as the name of the " (scheme menu-item%) "." "\n" "\n" "Defaults to " (scheme (string-constant about-menu-item)) ".")

@(defmethod (help-menu:about-help-string) string? "The result of this method is used as the help string" "\n" "when the " (scheme menu-item%) " object is created." "\n" "\n" "Defaults to " (scheme (string-constant about-info)) ".")

@(defmethod (help-menu:after-about (menu (is-a?/c menu-item%))) void? "This method is called " "after" " the addition of the" "\n" (tt "about") " menu-item. Override it to add additional" "\n" "menu items at that point. ")

}