
(module readable mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
	   "top-level.ss"
	   "toolbar.ss")

  (define gui-snip-class%
    (class snip-class%
      (inherit set-classname set-version)

      (define/override (read f)
	(let ([e (make-object gb:edit%)])
	  (send e prepare-to-load)
	  (send e read-from-file f)
	  (send e done-loading #t)
	  (make-object gui-code-snip% e)))

      (super-new)
      (set-classname "(lib \"readable.ss\" \"guibuilder\")")
      (set-version 1)))

  (define gui-snip-class (new gui-snip-class%))

  (send (get-the-snip-class-list) add gui-snip-class)


  (define gui-code-snip%
    (class* editor-snip% (readable-snip<%>)
      (inherit get-editor set-min-width set-min-height set-snipclass get-admin)

      (define/public (read-special source line column position)
	(send (get-editor) build-code #f #f))

      (define/override (write f)
	(send (get-editor) write-to-file f))
      (define/override (copy)
	(make-object gui-code-snip% (send (get-editor) copy-self)))

      (define/override (on-event dc x y editorx editory e)
	(if (send e button-down? 'right)
	    (let ([tool-menu (make-object popup-menu%)])
	      (add-tools #f tool-menu (lambda (c%)
					(send (get-editor) insert-element c%)))
	      (send (get-admin) popup-menu 
		    tool-menu this 
		    (- (send e get-x) x)
		    (- (send e get-y) y)))
	    (super on-event dc x y editorx editory e)))

      (super-new)
      (set-snipclass gui-snip-class)))

  (provide gui-code-snip%))
