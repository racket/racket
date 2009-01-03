
(module tool mzscheme
  (require drscheme/tool
           mred
           mzlib/unit
           mzlib/class
	   string-constants
	   mzlib/contract
           "top-level.ss"
	   "toolbar.ss"
	   "readable.ss")

  (provide tool@)

  (define-syntax (name stx) (syntax-case stx () [(_ x e) #'(let ((x e)) x)]))

  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^) 
      (define (phase1) (void))
      (define (phase2) 
	(drscheme:get/extend:extend-unit-frame 
	 (lambda (drs:frame%)
	   (name guibuilder-frame%
	   (class drs:frame%
	     (inherit get-insert-menu get-edit-target-object)

	     (define toolbar #f)
	     (define toolbar-shown? #f)

	     (define/override (get-definitions/interactions-panel-parent)
	       (let ([p (super get-definitions/interactions-panel-parent)])
		 (set! toolbar (new toolbar% [parent p][style '(deleted)]))
		 (add-tools toolbar #f
			    (lambda (c%) 
			      (let ([e (get-edit-target-object)])
				(if (e . is-a? . gb:edit%)
				    (send e insert-element c%)
				    (message-box 
				     (string-constant gui-tool-heading)
				     (string-constant gui-tool-before-clicking-message)
				     this
				     '(ok stop))))))
		 (new vertical-panel% (parent p))))

	     (define/override (add-show-menu-items menu)
	       (super add-show-menu-items menu)
	       (make-object menu-item% 
			    (string-constant gui-tool-show-gui-toolbar)
			    menu
			    (lambda (i e)
			      (let ([p (send toolbar get-parent)])
				(if toolbar-shown?
				    (send p delete-child toolbar)
				    (send p change-children (lambda (l)
							      (cons toolbar l))))
				(set! toolbar-shown? (not toolbar-shown?))
				(send i set-label (if toolbar-shown?
						      (string-constant gui-tool-hide-gui-toolbar)
						      (string-constant gui-tool-show-gui-toolbar)))))))

	     (super-new)

	     (make-object menu-item% (string-constant gui-tool-insert-gui) (get-insert-menu)
			  (lambda (b e)
			    (let ([e (get-edit-target-object)])
			      (when e
				(let* ([gb (make-object gb:edit%)]
				       [s (make-object gui-code-snip% gb)])
				  (send e insert s)
				  (send gb create-main-panel)
				  (send gb set-caret-owner #f 'display))))))
             (inherit register-capability-menu-item)
             (register-capability-menu-item 'drscheme:special:insert-gui-tool (get-insert-menu)))))))
      
      (drscheme:language:register-capability 'drscheme:special:insert-gui-tool (flat-contract boolean?) #t))))
