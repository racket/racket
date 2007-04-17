(module mred mzscheme
  (require (lib "etc.ss")
	   (prefix wx: "private/kernel.ss")
	   "private/wxtop.ss"
	   "private/app.ss"
	   "private/misc.ss"
	   "private/mrwindow.ss"
	   "private/mrcontainer.ss"
	   "private/mrtop.ss"
	   "private/mrpanel.ss"
	   "private/mrcanvas.ss"
	   "private/mritem.ss"
	   "private/mrtextfield.ss"
	   "private/mrmenuintf.ss"
	   "private/mrmenu.ss"
	   "private/mrpopup.ss"
	   "private/editor.ss"
	   "private/messagebox.ss"
	   "private/filedialog.ss"
	   "private/fontdialog.ss"
	   "private/moredialogs.ss"
	   "private/gdi.ss"
	   "private/snipfile.ss"
	   "private/repl.ss"
	   "private/afm.ss")

  ;; Initialize AFM/PS:
  (wx:set-ps-procs
   afm-draw-text
   afm-get-text-extent
   afm-expand-name
   afm-glyph-exists?)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (wx:set-dialogs get-file put-file get-ps-setup-from-user message-box)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define mred-module-name ((current-module-name-resolver)
			    '(lib "mred.ss" "mred") #f #f))
  (define class-module-name ((current-module-name-resolver)
			     '(lib "class.ss") #f #f))

  (define make-namespace-with-mred
    (opt-lambda ([flag 'mred])
      (unless (memq flag '(initial mred empty))
	(raise-type-error 'make-namespace-with-mred
			  "flag symbol, one of 'mred, 'initial, or 'empty"
			  flag))
      (let ([orig (current-namespace)]
	    [ns (make-namespace (if (eq? flag 'empty) 'empty 'initial))])
	(parameterize ([current-namespace ns])
	  (namespace-attach-module orig mred-module-name)
	  (when (eq? flag 'mred)
	    (namespace-require mred-module-name)
	    (namespace-require class-module-name)))
	ns)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax propagate
    (lambda (stx)
      (syntax-case stx ()
	[(_ n ...)
	 (let ([ns (syntax->list (syntax (n ...)))])
	   (with-syntax ([(k:n ...)
			  (map
			   (lambda (n)
			     (datum->syntax-object
			      n
			      (string->symbol
			       (format 
				"wx:~a"
				(syntax-e n)))
			      #f))
			   ns)])
	     (syntax (begin
		       ;; We can't just re-export, because kernel.ss's
		       ;;  exports are protected.
		       (define n k:n) ...
		       (provide n ...)))))])))

  (propagate add-color<%>
	     add-editor-keymap-functions
	     add-text-keymap-functions
	     add-pasteboard-keymap-functions
	     begin-busy-cursor
	     bell
	     bitmap%
	     brush%
	     brush-list%
	     editor-data%
	     editor-data-class%
	     editor-data-class-list<%>
	     check-for-break
	     clipboard<%>
	     clipboard-client%
	     color%
	     color-database<%>
	     control-event%
	     current-eventspace
	     current-ps-setup
	     cursor%
	     dc<%>
	     dc-path%
	     get-display-depth
	     end-busy-cursor
	     event%
	     event-dispatch-handler
	     eventspace?
	     find-graphical-system-path
	     flush-display
	     font%
	     font-list%
	     font-name-directory<%>
	     get-resource
	     get-the-editor-data-class-list
	     get-the-snip-class-list
	     image-snip%
	     is-busy?
	     is-color-display?
	     key-event%
	     keymap%
	     make-eventspace
	     editor-admin%
	     editor-set-x-selection-mode
	     editor-snip-editor-admin<%>
	     editor-stream-in%
	     editor-stream-in-base%
	     editor-stream-in-bytes-base%
	     editor-stream-out%
	     editor-stream-out-base%
	     editor-stream-out-bytes-base%
	     editor-wordbreak-map%
	     mouse-event%
	     mult-color<%>
	     pen%
	     pen-list%
	     point%
	     ps-setup%
	     read-editor-global-footer
	     read-editor-global-header
	     read-editor-version
	     region%
	     scroll-event%
	     snip%
	     snip-admin%
	     snip-class%
	     snip-class-list<%>
	     special-control-key
	     special-option-key
	     label->plain-label
	     string-snip%
	     style<%>
	     style-delta%
	     style-list%
	     tab-snip%
	     write-editor-global-footer
	     write-editor-global-header
	     write-editor-version
	     write-resource
	     queue-callback
	     yield
	     eventspace-shutdown?
	     get-panel-background
	     send-event
	     gl-context<%>
	     gl-config%)

  (define the-color-database (wx:get-the-color-database))
  (define the-font-name-directory (wx:get-the-font-name-directory))
  (define the-clipboard (wx:get-the-clipboard))
  (define the-x-selection-clipboard (wx:get-the-x-selection))
  (define the-font-list (wx:get-the-font-list))
  (define the-pen-list (wx:get-the-pen-list))
  (define the-brush-list (wx:get-the-brush-list))
  (define the-style-list (wx:get-the-style-list))
  (define the-editor-wordbreak-map (wx:get-the-editor-wordbreak-map))

  (provide button%
	   canvas%
	   check-box%
	   choice%
	   dialog%
	   frame%
	   gauge%
	   tab-panel%
	   group-box-panel%
	   list-box%
	   editor-canvas%
	   message%
	   pane%
	   horizontal-pane%
	   vertical-pane%
	   grow-box-spacer-pane%
	   panel%
	   horizontal-panel%
	   vertical-panel%
	   radio-box%
	   slider%
	   text-field%
	   combo-field%
	   window<%>
	   area<%>
	   top-level-window<%>
	   subarea<%>
	   subwindow<%>
	   area-container<%>
	   area-container-window<%>
	   canvas<%>
	   control<%>
	   list-control<%>
	   menu-item<%>
	   separator-menu-item%
	   selectable-menu-item<%>
	   labelled-menu-item<%>
	   menu-item%
	   checkable-menu-item%
	   get-default-shortcut-prefix
	   menu-item-container<%>
	   menu%
	   menu-bar%
	   popup-menu%
	   get-top-level-windows
	   editor-snip%
	   editor<%>
	   text%
	   pasteboard%
	   graphical-read-eval-print-loop
	   message-box
	   message+check-box
	   message-box/custom
	   message+check-box/custom
           get-face-list
	   get-file
	   get-file-list
	   put-file
	   get-directory
	   get-choices-from-user
	   get-text-from-user
	   get-ps-setup-from-user
	   get-page-setup-from-user
	   can-get-page-setup-from-user?
	   play-sound
	   get-display-size
	   get-display-left-top-inset
	   get-color-from-user
	   get-font-from-user
	   append-editor-operation-menu-items
	   append-editor-font-menu-items
	   get-top-level-focus-window
	   get-top-level-edit-target-window
	   register-collecting-blit
	   unregister-collecting-blit
	   bitmap-dc%
	   post-script-dc%
	   printer-dc%
	   current-text-keymap-initializer
	   sleep/yield
	   get-window-text-extent
	   get-family-builtin-face
	   send-message-to-window
	   the-clipboard
	   the-x-selection-clipboard
	   the-editor-wordbreak-map
	   the-brush-list
	   the-color-database
	   the-font-name-directory
	   the-pen-list
	   the-font-list
	   the-style-list
	   normal-control-font
	   small-control-font
	   tiny-control-font
	   view-control-font
	   menu-control-font
	   timer%
	   readable-snip<%>
	   open-input-text-editor
	   open-input-graphical-file
	   open-output-text-editor
	   text-editor-load-handler
	   application-about-handler
	   application-preferences-handler
	   application-quit-handler
	   application-file-handler
	   current-eventspace-has-standard-menus?
	   current-eventspace-has-menu-root?
	   eventspace-handler-thread
	   make-namespace-with-mred
	   file-creator-and-type
	   current-ps-afm-file-paths
	   current-ps-cmap-file-paths
	   hide-cursor-until-moved))
