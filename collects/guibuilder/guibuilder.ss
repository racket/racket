
(module guibuilder mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   (prefix framework: (lib "framework.ss" "framework"))
	   "utils.ss"
	   "top-level.ss"
	   "toolbar.ss")

  ;; These modules implement snips for the various
  ;;  kinds of windows and controls.
  (require "base.ss"
	   "panel.ss"
	   "simple-control.ss"
	   "text-field.ss"
	   "multiple-choice.ss"
	   "slider-guage.ss"
	   "canvas.ss")
  
  (define my-base-frame% framework:frame:editor%)

  (define gb:frame%
    (class my-base-frame% 
      (init [file #f])
      (inherit get-editor show get-area-container get-menu-bar)

      (define gb-editor #f)
      (define (get-gb-editor)
	gb-editor)

      (override*	
	[get-editor% (lambda () 
		       (class framework:text:info%
			 (inherit insert)
			 (super-new)
			 (set! gb-editor (new gb:edit%))
			 (insert (make-object mred:editor-snip% gb-editor))))])
      
      (define toolbar #f)
      (public*
	[init-tools
	 (lambda (mb)
	   (set! toolbar (make-object toolbar% (get-area-container)))
	   (send (get-area-container) change-children
		 (lambda (l)
		   (cons toolbar (remove toolbar l))))
	   
	   (let* ([emenu (make-object mred:menu% "Element" mb)]
		  [vmenu (make-object mred:menu% "Output" mb)])
	     (make-object mred:menu-item% "Configure Selected" emenu
			  (lambda (i e)
			    (send (get-gb-editor)
				  for-each-selected-snip
				  (lambda (s)
				    (send s gb-open-dialog)))))
	     (make-object mred:separator-menu-item% emenu)
	     (add-tools toolbar emenu (lambda (c%) (insert-element c%)))

	     (make-object mred:menu-item% "Configure Output"  vmenu
			  (lambda (i e) (send (get-gb-editor) open-dialog)))
	     (make-object mred:separator-menu-item% vmenu)
	     (make-object mred:menu-item%  "Make Sample Window" vmenu 
			  (lambda (i e) (send (get-gb-editor) instantiate)))
	     (make-object mred:menu-item%  "Make Source Code" vmenu 
			  (lambda (i e) (send (get-gb-editor) view-source)))))]
	[insert-element
	 (lambda (c%)
	   (let ([e (get-gb-editor)])
	     (send e insert-element c%)))])

      (super-make-object (or file "GUI Builder"))

      (init-tools (get-menu-bar))
      
      (let ([file (and file (normalize-path file))])
	(if (and file (file-exists? file) (send (get-gb-editor) load-file file))
	    ;; Force title size calc:
	    (let ([e (get-gb-editor)])
	      (send e get-main-location 
		    (send e get-main-panel)
		    (send (send e get-canvas) get-dc)
		    (box 0) (box 0)))
	    (begin
	      (send (get-gb-editor) create-main-panel)
	      (when file
		(send (get-gb-editor) set-filename file)))))
      
      (show #t)))
  
  (framework:handler:insert-format-handler "GUI Builder" "gui"
					   (lambda (file)
					     (make-object gb:frame% file)))
  
  (define (new-gui-builder-frame) (new gb:frame% [height 400]))
					       

  (new-gui-builder-frame))
