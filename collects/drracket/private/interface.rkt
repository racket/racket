#lang racket/unit
#|

This file has names with partial prefixes built into them
because the interfaces here used to be in other files, but
that was causing circular dependencies; the interfaces
were moved here to break the cycle, but the names should
remain the same for tools that use them.

|#

(require "drsig.rkt"
         "local-member-names.rkt"
         racket/class 
         framework)
(import)
(export drracket:interface^)

(define frame:basics<%> (interface (frame:standard-menus<%>)))
(define frame:<%>
  (interface (frame:editor<%> frame:basics<%> frame:text-info<%>)
    get-show-menu
    update-shown
    add-show-menu-items
    set-show-menu-sort-key))

(define unit:frame<%>
  (interface (frame:<%>
              frame:searchable-text<%>
              frame:delegate<%>)
    get-insert-menu
    get-special-menu
    get-interactions-text
    get-definitions-text
    get-interactions-canvas
    get-definitions-canvas
    get-button-panel
    execute-callback
    get-current-tab
    open-in-new-tab
    create-new-tab
    close-current-tab
    on-tab-change
    enable-evaluation
    disable-evaluation
    get-definitions/interactions-panel-parent
    register-capability-menu-item
    
    ensure-rep-shown
    ensure-rep-hidden
    ensure-defs-shown
    
    
    get-language-menu
    register-toolbar-button
    register-toolbar-buttons
    unregister-toolbar-button
    sort-toolbar-buttons-panel
    get-tabs))

(define unit:definitions-text<%> 
  (interface ()
    begin-metadata-changes
    end-metadata-changes
    get-tab
    get-next-settings
    after-set-next-settings
    set-needs-execution-message))

(define rep:context<%>
  (interface ()
    ensure-rep-shown   ;; (interactions-text -> void)
    ;; make the rep visible in the frame
    
    repl-submit-happened ;; (-> boolean)
    ;; notify the context that an evaluation is about to
    ;; happen in the REPL (so it can show a warning about
    ;; the language/etc is out of sync if neccessary).
    
    enable-evaluation  ;; (-> void)
    ;; make the context enable all methods of evaluation
    ;; (disable buttons, menus, etc)
    
    disable-evaluation ;; (-> void)
    ;; make the context disable all methods of evaluation
    ;; (disable buttons, menus, etc)
    
    set-breakables ;; (union thread #f) (union custodian #f) -> void
    ;; the context might initiate breaks or kills to
    ;; the thread passed to this function
    
    get-breakables ;; -> (values (union thread #f) (union custodian #f))
    ;; returns the last values passed to set-breakables.
    
    reset-offer-kill ;; (-> void)
    ;; the next time the break button is pushed, it will only
    ;; break. (if the break button is clicked twice without
    ;; this method being called in between, it will offer to
    ;; kill the user's program)
    
    update-running        ;; (boolean -> void)
    ;; a callback to indicate that the repl may have changed its running state
    ;; use the repls' get-in-evaluation? method to find out what the current state is.
    
    clear-annotations  ;; (-> void)
    ;; clear any error highlighting context
    
    get-directory      ;; (-> (union #f string[existing directory]))
    ;; returns the directory that should be the default for
    ;; the `current-directory' parameter in the repl.
    ))

(define unit:tab<%>
  (interface (rep:context<%>)
    get-frame
    get-defs
    get-ints
    get-visible-defs
    set-visible-defs
    set-visible-ints
    set-focus-d/i
    get-i
    set-i
    break-callback
    is-current-tab?
    get-enabled
    on-close
    can-close?
    toggle-log))

(define module-language-tools:definitions-text<%> (interface () move-to-new-language))
(define module-language-tools:tab<%> (interface ()))
(define module-language-tools:frame<%> (interface ()))

