
(module initial-ones mzscheme
  ;; Instead of using the "slideshow.ss" language module,
  ;;  we can use mzscheme and import "slide.ss"
  (require (lib "slide.ss" "slideshow")
	   (lib "code.ss" "slideshow")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "etc.ss"))

  (provide do-initial-slides)

  (define (do-initial-slides)
    (begin-with-definitions
      (slide/name/center
       "Title Slide"
       (titlet "Slideshow Tutorial")
       (blank)
       (size-in-pixels
	(bitmap (build-path (collection-path "icons") "PLT-206.png")))
       (blank)
       (colorize (it "Press the spacebar to continue") blue)
       (comment "Welcome to Slideshow"))

      (slide/title/center
       "About Slideshow"
       (page-para (bt "Slideshow")
		  "is a library for creating slide presentations")
       (page-item "A Slideshow presentation is a PLT Scheme program")
       (page-item "Instead of a WYSIWYG interface,"
		  "you get the power of Scheme"))
      
      (define (symbol n)
	(text (string (integer->char n)) 'symbol font-size))
      (define sym:rightarrow (symbol 174))
      (define sym:leftarrow (symbol 172))

      (define (meta key)
	(hbl-append (t "Alt-") 
		    (if (pict? key) key (tt key))
		    (t ", Cmd-") 
		    (if (pict? key) key (tt key))
		    (t ", or Meta-") 
		    (if (pict? key) key (tt key))))

      (slide/title
       "How to Control this Viewer"
       (scale/improve-new-text
	(table 3
	       (apply
		append
		(map (lambda (s)
		       (list (apply page-para* (car s)) (t ":") (t (cadr s))))
		     `(((,(meta "q")) "end show")
		       (("Esc") "if confirmed, end show")
		       ((,sym:rightarrow ", Space," ,(tt "f") "," ,(tt "n") ", or click") "next slide")
		       ((,sym:leftarrow ", Backspace, Delete, or" ,(tt "b")) "previous slide")
		       ((,(tt "g")) "last slide")
		       ((,(tt "1")) "first slide")
		       ((,(meta "g")) "select a slide")
		       ((,(meta "p")) "show/hide slide number")
		       ((,(meta "c")) "show/hide commentary")
		       ((,(meta "d")) "show/hide preview")
		       ((,(meta "m")) "show/hide mouse cursor")
		       ((,(hbl-append (t "Shift-") sym:rightarrow) ", etc.") "move window 1 pixel")
		       ((,(meta sym:rightarrow) ", etc.") "move window 10 pixels"))))
	       lbl-superimpose lbl-superimpose
	       gap-size (/ gap-size 2))
	0.9)
       (comment "This window shows comments for each slide. "
		"The comments are typically fill in the details of what "
		"the slide presenter says when giving the talk."))
		

      (define mytalk.scm (tt "mytalk.scm"))


      (slide/title
       "Slideshow Programs"
       (page-para "A Slideshow program has the form")
       (scale/improve-new-text
	(code (module mytalk (lib "slideshow.ss" "slideshow")
		... #,(it "code to generate slide content") ...))
	0.9)
       (page-para "in a file named" mytalk.scm)
       (colorize (hline (* 3/4 client-w) gap-size) "green")
       'alts
       (list (list (page-para "To run a Slideshow program,")
		   (page-item "Double-click the" (bt "Slideshow") "executable or run" 
			      (tt "slideshow") "on the command line")
		   (page-item "Click the" (bt "Open File...") "link and select the"
			      "Slideshow program file, such as" mytalk.scm))
	     (list (page-para "Alternately, run a Slideshow program in DrScheme:")
		   (page-item "Open" mytalk.scm "in DrScheme")
		   (page-item "Select" (bt "Choose Language") "from the" (bt "Language") "menu")
		   (page-item "Choose the" (tt "(module ...)") "language")
		   (page-item "Click" (bt "Execute"))
		   (colorize (bt "Use DrScheme only if you trust the program") "red"))
	     (list (page-para (colorize (bt "Important security information:") "red"))
		   (page-item "A slideshow program has access to the"
			      (it "full") (it "Scheme") (it "language"))
		   (page-item "If you don't know the creator of a slide program"
			      "(or if you don't trust them), run the slides through the"
			      (bt "Slideshow") "executable or"
			      (tt "slideshow") "command line")
		   (colorize
		    (page-item/bullet
		     (ghost bullet)
		     "When run in" (bt "Slideshow") "instead of DrScheme,"
		     "a slide program cannot write files"
		     "or make network connections")
		    "blue"))
	     (list (page-para "When using a command line, you can specify the program directly:")
		   (hbl-append (tt "slideshow ") mytalk.scm)
		   (blank)
		   (page-para "To print the talk:")
		   (hbl-append (tt "slideshow --print ") mytalk.scm)
		   (blank)
		   (colorize
		    (page-para/r (it "Run") (tt "slideshow --help") (it "for more options"))
		    "blue"))))

      (define (sub-para . l)
	(colorize (apply para (* 3/4 client-w) l) "blue"))

      (slide/title
       "Slides and Picts"
       (page-para "The body of a Slideshow program")
       (page-item/bullet (bt " 1.")
			 "Makes and combines" (hbl-append (bit "pict") (t "s")))
       (sub-para "For example,")
       (code (t "Hello"))
       (sub-para "creates a pict like this:")
       (colorize (t "Hello") "black")
       (page-item/bullet (bt " 2.") "Registers certain picts as slides")
       (sub-para "For example,")
       (code (slide (t "Hello")))
       (sub-para "registers a slide containing only" (colorize (t "Hello") "black")))

      (slide/title/center
       "Slides versus Picts"
       (page-para "Technically, the pict concept comes from the"
		  (tt "\"texpict\"") "collection, and the"
		  (tt "\"slideshow\"") "collection builds on it")
       (page-item "The distinction between"
		  "Slideshow and texpict matters when you"
		  "use Help Desk to find information")
       (page-item "For now, we ignore the distinction"))

      (slide/title/center
       "The Rest of the Tutorial"
       (page-para "The rest of this tutorial (starting with the next slide) is meant to"
		  "be viewed while reading the program source")
       (blank)
       (page-para "The source is")
       (let ([s (path->string (build-path (this-expression-source-directory) "tutorial-show.ss"))])
	 (clickback
	  (scale/improve-new-text
	   (let ([p (tt s)])
	     (colorize
	      (place-over p 0 (pict-height p)
			  (linewidth 2 (hline (pict-width p) 2)))
	      "blue"))
	   (min 1 (/ (* 0.8 client-w ) (pict-width (tt s)))))
	  (lambda ()
	    (let* ([f (new frame% 
			   [label "tutorial-show.ss"]
			   [width 600]
			   [height 400])]
		   [e (new text%)]
		   [c (new editor-canvas% 
			   [parent f]
			   [editor e])])
	      (send e load-file s)
	      (send e change-style 
		    (make-object style-delta% 'change-family 'modern)
		    0 'end)
	      (send f show #t))))))
      )
    (void)))

