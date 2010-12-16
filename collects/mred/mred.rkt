(module mred mzscheme
  (require (only racket/base
                 define-namespace-anchor
                 namespace-anchor->empty-namespace
                 make-base-empty-namespace)
           scheme/class
           racket/draw
           mzlib/etc
	   (prefix wx: "private/kernel.ss")
	   (prefix wx: "private/wxme/style.ss")
	   (prefix wx: "private/wxme/editor.ss")
	   (prefix wx: "private/wxme/text.ss")
	   (prefix wx: "private/wxme/pasteboard.ss")
	   (prefix wx: "private/wxme/snip.ss")
	   (prefix wx: "private/wxme/keymap.ss")
	   (prefix wx: "private/wxme/editor-admin.ss")
           (prefix wx: "private/wxme/editor-data.ss")
	   (prefix wx: "private/wxme/editor-snip.ss")
	   (prefix wx: "private/wxme/stream.ss")
	   (prefix wx: "private/wxme/wordbreak.ss")
	   (prefix wx: "private/wxme/snip-admin.ss")
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
           "private/helper.ss"
           "private/dynamic.ss"
           "private/check.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These functions are re-implemented in scheme/gui/base
  ;; and racket/gui/base to attach those names, instead of
  ;; just 'mred.

  (define-namespace-anchor anchor)

  (define (make-gui-empty-namespace)
    (let ([ns (make-base-empty-namespace)])
      (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                               'mred
                               ns)
      ns))

  (define (make-gui-namespace)
    (let ([ns (make-gui-empty-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-require 'scheme/base)
        (namespace-require 'mred)
        (namespace-require 'scheme/class))
      ns))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-eventspace)
    (parameterize ([wx:the-snip-class-list (wx:make-the-snip-class-list)]
                   [wx:the-editor-data-class-list (wx:make-the-editor-data-class-list)])
      (wx:make-eventspace)))

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
	     editor-data%
	     editor-data-class%
	     editor-data-class-list<%>
	     check-for-break
	     clipboard<%>
	     clipboard-client%
	     control-event%
	     current-eventspace
	     cursor%
	     get-display-depth
	     end-busy-cursor
	     event%
	     event-dispatch-handler
	     eventspace?
	     flush-display
	     get-highlight-background-color
             get-highlight-text-color
	     get-the-editor-data-class-list
	     get-the-snip-class-list
	     image-snip%
	     is-busy?
	     is-color-display?
	     key-event%
	     keymap%
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
	     read-editor-global-footer
	     read-editor-global-header
	     read-editor-version
	     scroll-event%
	     snip%
	     snip-admin%
	     snip-class%
	     snip-class-list<%>
	     special-control-key
	     special-option-key
             map-command-as-meta-key
	     label->plain-label
	     string-snip%
	     style<%>
	     style-delta%
	     style-list%
	     tab-snip%
	     write-editor-global-footer
	     write-editor-global-header
	     write-editor-version
	     queue-callback
	     yield
	     eventspace-shutdown?
	     get-panel-background

             the-style-list
             the-editor-wordbreak-map
             make-screen-bitmap
             make-gl-bitmap)
   
  (define the-clipboard (wx:get-the-clipboard))
  (define the-x-selection-clipboard (wx:get-the-x-selection))

  (define (find-graphical-system-path what)
    (unless (memq what '(init-file x-display))
      (raise-type-error 'find-graphical-system-path "'init-file or 'x-display" what))
    (or (wx:find-graphical-system-path what)
        (case what
          [(init-file)
           (build-path (find-system-path 'init-dir)
                       (case (system-type)
                         [(windows) "gracketrc.rktl"]
                         [else ".gracketrc"]))]
          [else #f])))

  (provide (all-from racket/draw))

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
	   textual-read-eval-print-loop
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
	   printer-dc%
	   current-text-keymap-initializer
	   sleep/yield
	   get-window-text-extent
	   send-message-to-window
	   the-clipboard
	   the-x-selection-clipboard
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
           make-eventspace
	   make-gui-namespace
	   make-gui-empty-namespace
	   file-creator-and-type
	   hide-cursor-until-moved
           system-position-ok-before-cancel?
           label-string?
           key-code-symbol?
           find-graphical-system-path))
