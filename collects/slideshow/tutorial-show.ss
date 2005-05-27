;; This example slideshow code is meant to be read while
;;  running the example show.

(module tutorial-show (lib "slideshow.ss" "slideshow")
  
  ;; The first few slides are not to be read
  ;; until you have seen the rest of the tutorial,
  ;; so we put them in a separate file
  (require "initial-ones.ss")
  (do-initial-slides)

  ;; The rest is meant for reading, as you view the slides

  ;; The first slide that's meant to be read
  (slide
   (t "Part I: Basic Concepts"))

  ;; The second slide
  (slide
   (t "This slide shows how four picts")
   (t "get vertically appended by the")
   (tt "slide")
   (t "function to create and install a slide"))
  
  ;; Etc.
  (slide
   (t "See how the")
   (tt "t")
   (t "function takes a string and")
   (t "produces a pict with a normal sans-serif font, but")
   (tt "tt")
   (t "produces a pict with a fixed-width font?"))
  
  (slide
   (page-para "Breaking up text into lines is painful, so"
              "the" (tt "page-para")
              "function takes a mixture of strings and"
              "picts and puts them into a pagaraph")
   (page-para "It doesn't matter how strings are broken into"
              "parts in the code")
   (page-para "The" (tt "page-para") "function puts space"
              "between separate strings"
              ", but not before punctuation" "!"))
  
  (slide/center
   (page-para "The" (tt "slide/center") "function centers"
              "the slide body vertically")
   (page-para "All of the" (tt "slide") "functions center"
              "the body picts horizontally, but" (tt "page-para")
              "makes a slide-width picture with left-aligned text")
   (frame (page-para "The" (tt "frame") "function wraps a frame"
                     "around a pict to create a new pict,"
                     "so you can easily see this individual pict")))
  
  (slide/title
   "Titles"
   (page-para "The" (tt "slide/title") "function takes a"
              "title string before the content picts"))
  
  (slide/title/center
   "Titles and Centering"
   (page-para "The" (tt "slide/title/center") "function centers the slide body vertically"))
  
  (slide/title/center
   "More Centering"
   (frame
    (page-para/c "The" (tt "page-para/c") "function generates"
                 "a paragraph with centered lines of text"))
   (frame
    (page-para* "This line uses the" (tt "page-para*") "function"))
   (page-para "The" (tt "page-para*") "function creates a paragraph"
              "that is wrapped to fit the slide, but it allows"
              "the resulting pict to be more narrow than the slide"))

  (let ([apply-page-para-to-example
	 (lambda (page-para...)
	   (page-para...
	    "And there's" (tt "page-para*/r") ", which is different"
	    "from" (tt "page-para*") "or" (tt "page-para*/c")
	    "only if the paragraph takes multiple lines"))])
    (slide/title
     "More Alignment"
     (frame
      (page-para/r "Of course, there's also" (tt "page-para/r")))
     (frame (apply-page-para-to-example page-para*/r))
     (page-para "For comparision, the same text using" (tt "page-para/r") ":")
     (frame (apply-page-para-to-example page-para/r))
     (page-para "Unless your font happens to make the" (tt "page-para*/r")
		"box exactly as wide as this slide, the last box will be slightly"
		"wider with extra space to the left")))
  
  (slide/title
   "Spacing"
   (page-para "The" (tt "slide") "functions insert space"
              "between each body pict")
   (page-para "The amount of space is" (number->string gap-size)
              ", which is the value of" (tt "gap-size")))
  
  (slide/title
   "Controlling Space"
   (vc-append line-sep
              (page-para "If you want to control the space,"
                         "simply append the picts yourself to create"
                         "one body pict")
              (page-para "The first argument to" (tt "vc-append")
                         "determines the space between pictures")
              (page-para "If the first argument is a pict instead of"
                         "a number, then 0 is used")
              (page-para "For text in one paragraph, the"
                         (tt "page-para") "function uses" (tt "line-sep")
                         ", which is" (number->string line-sep))))

  (slide/title/center
   "Appending Picts"
   (vl-append line-sep (frame (t "This is")) (frame (tt "vl-append")))
   (vc-append line-sep (frame (t "This is")) (frame (tt "vc-append")))
   (vr-append line-sep (frame (t "This is")) (frame (tt "vr-append"))))
  
  (slide/title/center
   "Horizontal Appending"
   (hc-append (frame (t "This is")) (frame (vr-append (tt "hc-append")
                                                      (t "obviously"))))
   (ht-append (frame (t "This is")) (frame (vr-append (tt "ht-append")
                                                      (t "obviously"))))
   (hb-append (frame (t "This is")) (frame (vr-append (tt "hb-append")
                                                      (t "obviously")))))
  
  (slide/title/center
   "Text Alignment"
   (hbl-append (frame (scale (tt "hbl-append") 1.5))
               (frame (t "aligns text baselines")))
   (page-para "It's especially useful for font mixtures")
   (hbl-append (frame (scale (tt "htl-append") 1.5))
               (frame (t "is the same for single lines")))
   (page-para "The difference between" (tt "htl-append")
              "and" (tt "hbl-append") "shows up with multiple lines:")
   (hbl-append (frame (scale (t "bottom lines align") 1.5))
               (frame (vl-append (t "when using") (tt "hbl-append"))))
   (htl-append (frame (scale (t "top lines align") 1.5))
               (frame (vl-append (t "when using") (tt "htl-append")))))
  
  (slide/title
   "Superimposing"
   (cc-superimpose (t "X") (t "O"))
   (page-para "The" (tt "cc-superimpose") 
              "function puts picts on top of each other, centered")
   (page-para "Each of" (tt "l") "," (tt "r") ", and" (tt "c")
              "is matched with each of"
              (tt "t") "," (tt "b") "," (tt "c") "," (tt "bl") ", and" (tt "tl")
              "in all combinations with" (tt "-superimpose"))
   (page-para "For example," (tt "cbl-superimpose") ":")
   (cbl-superimpose (frame (scale (t "one line") 1.5))
                    (frame
                     (colorize (vl-append (t "two")
                                          (t "lines"))
                               "blue"))))
  
  (slide
   (cc-superimpose 
    (frame (blank client-w client-h))
    (vc-append gap-size
               (t "By definition, the screen is 1024 x 768 units")
               (t "If you have more or less pixels, the image is scaled")
               (page-para*
                "There's a margin, so the \"client\" area is"
                (number->string client-w) "x" (number->string client-h))
               (page-para* "The font size is" (number->string font-size)))))
  
  (slide/title
   "Titled Client Area"
   (rb-superimpose
    (cc-superimpose
     (frame titleless-page)
     (page-para* "If you use a title, then" (tt "titleless-page")
                 "is the same size as the area left for the body"))
    (t "It's useful")))
   
  (slide/title
   "More on Paragraphs"
   (page-para "The" (tt "page-") "in" (tt "page-para")
              "makes the paragraph take the width of the slide")
   (para 300 
         "The" (tt "para") "function requires an explicit size"
         "for the width of the paragraph, 300 in this case")
   (para client-w "So" (tt "page-para") "is a shorthand for"
         (tt "para") "with" (tt "client-w"))
   (para*/c 500 "Naturally, there is" (tt "para*") ","
            (tt "para*/c") ", and" (tt "para*/r")))
  
  (slide/title/center
   "Text and Styles"
   (page-para "Functions exist for" (bt "bold") ","
              (it "italic") ", and even" (bit "bold-italic") "text")
   (page-para "The" (tt "text") "function gives you more direct control over the"
              (text "font" '(italic . modern) font-size) ","
              (text "size" main-font 24) ", and even"
              (text "angle" main-font font-size (/ 3.14159 4))))
  
  (require (lib "code.ss" "slideshow"))
  (slide/title/center
   "Scheme Code"
   (page-para "For Scheme code, the" (code (lib "code.ss" "slideshow"))
              "library provides a handy" (code code) "macro for"
              "typesetting literal code")
   (page-para "The" (code code) "macro uses source-location information"
              "to indent code")
   (code (define (length l)
           (cond
             [(null? l) 0]
             [else (+ 1 (length (cdr l)))]))))

  (slide/title/center
   "Colors"
   (page-para "Use the" (colorize (tt "colorize") "blue")
              "function to color most things, including text")
   (frame
    (colorize (page-para "A" (code colorize)
                         "applies only to sub-picts that do not"
                         "already have a" (colorize (t "color") "green"))
              "red")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part II starts here

  (define outline 
    (let ([sub-para (lambda l
		      (apply para (* 3/4 client-w) l))])
      (make-outline
       'one "Part I: Basic Concepts" 
       #f
       
       'two "Part II: Practical Slides" 
       (lambda (tag)
	 (sub-para "Using" (code make-outline) "and more..."))
       
       'three "Part III: Fancy Picts" 
       (lambda (tag)
	 (sub-para "Creating interesting graphics"))
     
       'four "Part IV: Advanced Slides" 
       (lambda (tag)
	 (sub-para "Beyond" (code 'next) "and" (code 'alts)))
     
       'background "Part V: Controlling the Background"
       (lambda (tag)
	 (sub-para "Changing the overall look of your talk"))

       'printing "Part VI: Printing"
       (lambda (tag)
	 (sub-para "Exporting slides as PostScript"))

       'end "Conclusion" 
       (lambda (tag)
	 (sub-para "This is the end")))))

  
  (outline 'two)
  
  (slide/title/center
   "Itemize"
   (page-item "Bulleted sequences are common in slides")
   (page-item "The" (code page-item) "function makes a bulleted"
              "paragraph that is as wide as the slide")
   (page-item/bullet (colorize (tt "+") "green")
                     "You can set the bullet, if you like, by using"
                     (code page-item/bullet))
   (page-subitem "Naturally, there is also" (code page-subitem)))
  
  (slide/title/center
   "Itemize"
   (page-para "You could write" (code page-item) "yourself:")
   (code (define (page-item . l)
           (htl-append
            (/ gap-size 2)
            bullet
            (apply para 
                   (- client-w (pict-width bullet) 
                      (/ gap-size 2))
                   l))))
   (page-para "where" (code bullet) "is a constant pict:" bullet))
  
  (slide/title/center
   "Grouping and Space"
   (page-para "Sometimes you want to group items on a slide")
   (page-item "A bullet goes with a statement")
   (page-item "And another does, too")
   (blank)
   (page-para "Creating a zero-sized pict with" (code (blank))
              "effectively doubles the gap, making a space that"
	      "often looks right"))
  
  (slide/title/center
   "Steps"
   (page-item "Suppose you want to show only one item at a time")
   'next
   (page-item "In addition to body picts, the" (code slide) 
	      "functions recognize certain staging symbols")
   (page-item "Use" (code 'next) "in a sequence of" (code slide)
              "arguments to create multiple slides, one"
              "containing only the preceding content, and another"
              "with the remainder")
   'next
   (blank)
   (colorize
    (page-para* (code 'next) "is not tied to" (code page-item)
                ", though it's often used with items")
    "blue"))
    
  
  (slide/title/center
   "Alternatives"
   (page-para "Steps can break up a linear slide, but sometimes"
              "you need to replace one thing with something else")
   'alts 
   (list (list 
          (page-para* "For example, replace this..."))
         (list
          (page-para* "... with something else")
          'next
          (blank)
          (page-item "An" (code 'alts) "in a sequence"
                     "must be followed by a list of lists")
          (page-item "Each list is a sequence, a different conclusion for the slide's sequence")))
   (page-item "Anything after the list of lists is folded into the last alternative")
   'next
   (blank)
   (page-para "Of course, you can mix" (code 'alts) "and"
              (code 'next) "in interesting ways"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part III starts here
  (outline 'three)

  (slide/title
   "Fancy Picts"
   (page-para "In part I, we saw some basic pict constructors:" 
	      (code t) "," (code vl-append) ", etc.")
   (blank)
   (page-para "The libraries")
   (vr-append
    line-sep
    (code (lib "mrpict.ss" "texpict"))
    (page-para* "and" (code (lib "utils.ss" "texpict"))))
   (page-para "provide many more functions for creating picts")
   (blank)
   (page-para "Slideshow re-exports all of those functions, so you"
	      "can just use them"))

  (slide/title
   "Bitmaps"
   (page-para "For example, the" (code bitmap) "function loads a bitmap to display")
   (blank)
   (bitmap (build-path (collection-path "icons") "plt.gif")))

  (require (lib "symbol.ss" "texpict"))
  (slide/title/center
   "Symbols"
   (page-para "The" (code (lib "symbol.ss" "texpict")) "library"
	      "provides various symbol constants, such as")
   (page-para* sym:in (code sym:in))
   (page-para* sym:rightarrow (code sym:rightarrow))
   (page-para* sym:infinity (code sym:infinity))
   (blank)
   (page-para "Slideshow does not re-export this library, so you"
	      "must" (code require) "it to use"
	      (code sym:in) ", etc.")
   (page-para "Unless otherwise stated in the following slides, however,"
	      "all definitions are provided by Slideshow"))

  (require (lib "mred.ss" "mred")) ; for message-box
  (slide/title
   "Clickbacks"
   (page-para "The" (code clickback)
	      "function attaches an arbitrary thunk to a pict"
	      "for interactive slides")
   (blank)
   (clickback (frame (t "Click Me"))
	      (lambda ()
		(message-box "Clicked" "Thanks!"))))

  (define (note . l)
    (colorize (apply page-para/r l) "blue"))

  (slide/title
   "Tables"
   (page-para "The" (code table) "function makes rows and columns")
   (frame
    (inset
     (table 3 ; three columns
	    (list (t "First") (standard-fish (* 2 gap-size) gap-size) (code cons)
		  (t "Second") (jack-o-lantern (* 4 gap-size)) (code car)
		  (t "Third") (cloud (* 3 gap-size) gap-size) (code cdr)
		  (t "Fourth") (file-icon (* 2 gap-size) (* 3 gap-size) #t) (code null?))
	    (list* lc-superimpose  ; left-align first column
		   cc-superimpose) ; h-center the rest
	    cc-superimpose ; v-center all rows
	    gap-size  ; separate all columns by gap-size
	    gap-size) ; separate all rows by gap-size
     gap-size))
   (note "The above also uses" (code standard-fish) ","
	 (code jack-o-lantern) "," (code cloud) ", and"
	 (code file-icon)))
  
  (require (lib "math.ss")) ; to get pi
  (define orientations 
    (map (lambda (x) (* pi x)) '(0 1/4 1/2 3/4 1 5/4 6/4 7/4)))
  (define (show-arrows code-arrow t-arrow arrow)
    (slide/title
     "Arrows"
     (page-para "The" code-arrow "function creates an"
		t-arrow "of a given size and orientation (in radians)")
     (blank)
     (page-para "Simple: " (arrow gap-size pi))
     (blank)
     (page-para "Fun:")
     (apply
      vc-append
      line-sep
      (map (lambda (shift)
	     (colorize
	      (apply hc-append gap-size (map (lambda (o) 
					       ;; Here's the other use of arrow
					       (arrow gap-size (+ o (* shift pi 1/32))))
					     orientations))
	      (scale-color (add1 shift) "green")))
	   '(0 1 2 3 4 5 6 7)))
     (blank)
     (note "(That's 64 uses of " code-arrow ")")))
  (show-arrows (code arrow) (t "arrow") arrow)
  (show-arrows (code arrowhead) (t "arrowhead") arrowhead)

  (require (lib "face.ss" "texpict"))
  (slide/title
   "Faces"
   (page-para "The" (code (lib "face.ss" "texpict"))
	      "library makes faces")
   (blank)
   (hc-append
    (* 3 gap-size)
    (face 'happy "yellow")
    (face 'badly-embarassed)))
    

  (require (lib "class.ss")) ; to demonstrate dc
  (slide/title 
   "Arbitrary Drawing"
   (page-para "The" (code dc) "function provides an escape hatch to the underlying"
              "MrEd toolkit")
   (page-para "For example," (code (disk 100)) "is the same as")
   (code
    (dc (lambda (dc dx dy)
          (send dc draw-ellipse dx dy 100 100))
        100 100 0 0))
   (dc (lambda (dc dx dy)
         (send dc draw-ellipse dx dy 100 100))
       100 100 0 0)
   (disk 100))

  (slide/title
   "Frames"
   (page-item "As we've already seen, the" (code frame)
	      "function wraps a" (frame (t "frame"))
	      "around a pict")
   (page-item "The" (code color-frame)
	      "function wraps a" (color-frame (t "colored frame") "red")
	      "; compare to" (code frame) "followed by" (code colorize)
	      "," (colorize (frame (t "like this")) "red"))
   (blank)
   (page-item "One way to increase the" (linewidth 3 (frame (t "line thickness")))
	      "is to use" (code linewidth))
   (blank)
   (page-item "It's often useful to" (frame (inset (t "add space") 3))
	      "around a pict with" (code inset) "before framing it"))

  (define (bound-frame p)
    (color-frame p "green"))

  (slide/title
   "Lines and Pict Dimensions"
   (page-item "The" (code hline) "function creates a horizontal line, given a bounding width and height:")
   (bound-frame (hline (* 2 gap-size) gap-size))
   (note "(The" (code hline) "result is framed in green above)")
   (page-item "Naturally, there's also" (code vline) ":")
   (bound-frame (vline (* 2 gap-size) gap-size))
   (blank)
   (page-item "To" (let ([p (t "underline")])
		     (vc-append p (hline (pict-width p) 1)))
	      "a pict, get its width using" (code pict-width)
	      ", then use" (code hline) "and" (code vc-append))
   (page-item "If the pict is text, you can restore the"
	      (let ([p (t "baseline")])
		(refocus 
		 (vc-append p (hline (pict-width p) 1))
		 p))
	      "using" (code refocus)))

  (slide/title
   "Placing Picts"
   (page-item "Another" (let ([p (t "underline strategy")])
			  (pin-over p 0 (pict-height p) (hline (pict-width p) 1)))
	      "is to use" (code pin-over) ", which places one pict on"
	      "top of another to generate a new pict")
   (page-item "The new pict has the"
	      (bound-frame
	       (let ([p (t "original")])
		 (pin-over p 0 (pict-height p) (hline (pict-width p) 1))))
	      "pict's bounding box and baselines")
   (note "(The green frame is the \"bounding box\" of the result)")
   (blank)
   (page-item "The" (code pin-over) "function is useful with" (code pt-arrow-line)
	      "to draw an"
	      (let ([p (bound-frame (inset (t "outgoing arrow") 2))])
		(pin-over p (/ (pict-width p) 2)  (/ (pict-height p) 2) 
			  ;; pt-arrow-line creates a pict where the
			  ;; "bounding box" corresponds to the non-arrow end
			  (linewidth 3 (colorize (pt-arrow-line 50 50 gap-size) "orange"))))
	      "without changing the layout"))

  (define blue-fish (standard-fish (* 3 gap-size) (* 2 gap-size) 'right "blue" "white"))
  (define plain-file (file-icon (* 2 gap-size) (* 3 gap-size) #t))
  (define fish-file-scene (bound-frame 
			   (inset (ht-append (* 4 gap-size) 
					     blue-fish 
					     (inset plain-file 0 (* 2 gap-size) 0 0))
				  gap-size)))

  (slide/title
   "Finding Picts"
   (page-para "Typically, an arrow needs to go from one pict to another")
   (page-para "Functions like" (code rc-find) "locate a point of a pict (such as \"right center\") inside a larger pict")
   (let-values ([(fdx fdy) (rc-find fish-file-scene blue-fish)]
		[(adx ady) (lt-find fish-file-scene plain-file)])
     (pin-over fish-file-scene
	       fdx fdy
	       (colorize
		(pt-arrow-line (- adx fdx) (- ady fdy) gap-size)
		"orange")))
   (page-para "There's a" (code -find) "function for every combination of"
	      (code l) "," (code c) ", and" (code r)
	      "with" (code t) "," (code c) "," (code b)
	      "," (code bl) ", and" (code tl)))
  
  (slide/title/center
   "Connecting with Arrows"
   (page-para "Actually, straight-arrow combinations are so common that"
	      "Slideshow provides"
	      (code pin-arrow-line))
   (pin-arrow-line gap-size fish-file-scene
		   blue-fish rc-find
		   plain-file lt-find
		   1 "orange"))
  
  (require (lib "balloon.ss" "texpict"))
  (slide/title/center
   "Balloons"
   (page-para "The" (code (lib "balloon.ss" "texpict"))
	      "library provides cartoon balloons" sym:emdash
	      "another reason to use" (code -find) "functions")
   (let* ([orig fish-file-scene]
	  [w/fish (pin-balloon (wrap-balloon (t "Fish") 'ne 0 (- gap-size))
			       orig blue-fish cb-find)]
	  [w/file (pin-balloon (wrap-balloon (t "File") 'nw (* -2 gap-size) 0)
			       w/fish
			       plain-file rc-find)])
     w/file))
		  
  (slide/title
   "Ghosting"
   (page-para "The" (code ghost) "function turns a picture invisible")
   (page-para "For example, the figure on the left and the"
              "figure on the right are the same size, because the"
	      "right one uses the" (code ghost) "of the left one")
   (let ([big-circle (circle 350)]
         [small-circle (circle 200)])
     (hc-append
      30
      (bound-frame big-circle)
      (bound-frame
       (lt-superimpose
	small-circle
	(rt-superimpose
	 small-circle
	 (lb-superimpose
	  small-circle
	  (rb-superimpose
	   small-circle
	   (ghost big-circle)))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Utilities for running additional examples
  
  (define (link s)
    (let ([p (t s)])
      (colorize
       (pin-over p 0 (pict-height p)
		 (linewidth 2 (hline (pict-width p) 2)))
       "blue")))

  (define (run-example-talk f)
    (let ([c (make-custodian)])
      (parameterize ([current-namespace (make-namespace-with-mred)]
		     [current-command-line-arguments
		      (vector (build-path (collection-path "slideshow")
					  "examples"
					  f))]
		     [current-custodian c]
		     [exit-handler (lambda (v)
				     (custodian-shutdown-all c))]
		     [current-directory (build-path (collection-path "slideshow")
						    "examples")])
	(parameterize ([current-eventspace (make-eventspace)])
	  (queue-callback
	   (lambda () (dynamic-require '(lib "slideshow.ss" "slideshow") #f)))))))

  (require (lib "edit.ss" "mred"))
  (define (show-example-code f)
    (new-text-frame (build-path (collection-path "slideshow")
				"examples"
				f)))  

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part IV
  (outline 'four)
  
  (define (in-picture-slide show-fish show-file show-link)
    (slide/title
     "In-Picture Sequences"
     (page-para "Although" (code 'next) "and" (code 'alts)
		"can create simple sequences, use procedure"
		"abstraction and" (code ghost) "to create"
		"complex sequences inside pict assemblies")
     (let* ([orig fish-file-scene]
	    [w/fish (pin-balloon (wrap-balloon (t "Fish") 'ne 0 (- gap-size))
				 (ghost orig) blue-fish cb-find)]
	    [w/file (pin-balloon (wrap-balloon (t "File") 'nw (* -2 gap-size) 0)
				 (ghost w/fish) plain-file rc-find)])
       (cc-superimpose orig
		       (show-fish w/fish)
		       (show-file w/file)))
     (blank)
     (blank)
     (blank)
     (show-link
      (page-para/r "Larger example:" 
		   (clickback (link "run")
			      (lambda ()
				(run-example-talk "interlocking-components.scm")))
		   (clickback (link "code")
			      (lambda ()
				(show-example-code "interlocking-components.scm")))))))

  (define (ident x) x)
  (in-picture-slide ghost ghost ghost)
  (in-picture-slide ident ghost ghost)
  (in-picture-slide ident ident ident)

  (define (encloud p)
    (refocus
     (cc-superimpose
      (cloud (* 9/8 (pict-width p))
	     (* 3/2 (pict-height p))
	     "light gray")
      p)
     p))

  (require (lib "step.ss" "slideshow"))
  (with-steps
   (intro basic-stx example-steps with-vonly with-vafter the-rest)
   (slide/title
    "Named Steps"
    (page-para "The" (code (lib "step.ss" "slideshow"))
	       "library provides a" (code with-steps)
	       "form to better express complex sequences")
    (lt-superimpose
     ((vonly basic-stx)
      (code (with-steps
	     (_step-name ...)
	     _slide-expr)))
     ((vonly example-steps)
      (code (with-steps
	     (intro detail conclusion)
	     _slide-expr)))
     ((vafter with-vonly)
      (code (with-steps
	     (intro detail conclusion)
	     code:blank
	     #,(encloud
		(cc-superimpose
		 ((vonly with-vonly)
		  (code ((vonly intro)
			 (t "For a start..."))))
		 ((vafter with-vafter)
		  (code ((vafter detail)
			 (t "like this"))))))))))
    (blank)
    (lt-superimpose
     ((vonly basic-stx)
      (page-para "A" (code with-steps) "form has a sequences of step names"
		 "followed by an expression to evaluate once for each step"))
     ((vonly example-steps)
      (page-para "For example, the above has three steps:"
		 (code intro) "," (code detail)
		 ", and" (code conclusion)))
     ((vonly with-vonly)
      (vl-append
       gap-size
       (page-para "In the body expression, use"
		  (code ((vonly _step-name) _pict-expr))
		  "to make" (code _pict-expr)
		  "visible only during" (code _step-name))
       (page-para "The expression" (code (vonly _step-name))
		  "produces either" (code ghost)
		  "or the identity function")))
     ((vonly with-vafter)
      (page-para "Use"
		 (code ((vafter _step-name) _pict-expr))
		 "to make" (code _pict-expr)
		 "visible after" (code _step-name)))
     ((vonly the-rest)
      (page-para "There's also" (code vbefore) "," (code vbetween) ", and more")))))


  (define smiley (face 'happy))
  (define desc (page-para "The" (code scroll-transition)
			  "function scrolls some part of the current slide"
			  "before shifting to the next slide."))
  
  (define (scroll-slide right?)
    (slide/title
     "Transition Animations"
     desc
     (hc-append
      (* 3 gap-size)
      ((if right? ghost values) smiley)
      ((if right? values ghost) smiley))
     (blank)
     ((if right? values ghost) (page-para "The face should have moved from left to right"))))
  (scroll-slide #f)
  
  ;; We really need a better way to do this!
  (let ([x (/ (- client-w (* 3 gap-size) (* 2 (pict-width smiley))) 2)]
	[y (+ (pict-height (titlet "Anything")) (* 2 gap-size)
	      (pict-height desc) gap-size)])
    (scroll-transition x y
		       (pict-width smiley) (pict-height smiley)
		       (+ (pict-width smiley) (* 3 gap-size)) 0
		       0.5 20))
  
  (scroll-slide #t)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Background

  ;; A pict to use behind the main content
  (define fade-bg
    (let ([w (+ (* 2 margin) client-w)]
	  [h (+ (* 2 margin) client-h)]
	  [trans (make-object brush% "white" 'transparent)]
	  [inside (make-object brush% "white" 'solid)])
      (inset (dc (lambda (dc x y)
		   (let ([b (send dc get-brush)]
			 [p (send dc get-pen)]
			 [draw-one
			  (lambda (i)
			    (send dc draw-rectangle
				  (+ x i) (+ y i)
				  (- w (* 2 i)) (- h (* 2 i))))])
		     (send dc set-brush trans)
		     (color-series 
		      dc margin 1
		      (make-object color% "black")
		      (make-object color% "white")
		      draw-one
		      #t #t)
		     (send dc set-brush inside)
		     (draw-one margin)
		     (send dc set-pen p)
		     (send dc set-brush b)))
		 w h 0 0)
	     (- margin))))

  (define orig-assembler (current-slide-assembler))

  ;; Normally, you'd set the slide assembler at the beginning
  ;;  of a talk and never change it
  (current-slide-assembler
   (lambda (s v-sep c)
     (lt-superimpose
      fade-bg
      (let ([c (colorize c "darkred")])
	(if s
	    (vc-append v-sep 
		       ;; left-aligns the title:
		       (page-para (titlet s)) 
		       c)
	    c)))))

  (outline 'background)

  (slide/title/center
   "Controlling the Background"
   (page-para "The" (code current-slide-assembler)
	      "parameter lets you change the overall look of a slide")
   (page-para "For this slide and the previous one, the assembler")
   (page-item "Colorizes the uncolored content as dark red")
   (page-item "Left-aligns the title")
   (page-item "Draws a fading box around the slide"))

  (current-slide-assembler orig-assembler)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Printing and Condensing
  (outline 'printing)

  (slide/title/center
   "Printing"
   (page-para "To export a set of slides as PostScript, "
	      "use the" (tt "slideshow") "command-line program:")
   (tt "slideshow --print myttalk.scm")
   (blank)
   (page-para "Slideshow steps through slides while producing PostScript pages")
   (page-para "The slides will look bad on the screen" sym:emdash "because"
	      "text is measured for printing instead of screen display"
	      sym:emdash "but the PostScript will be fine"))

  (slide/title/center
   "Condensing"
   (page-para "Often, it makes sense to eliminate" (code 'step)
	      "staging when printing slides:")
   (tt "slideshow --print --condense myttalk.scm")
   (blank)
   'next
   (page-para "You can also condense without printing")
   (tt "slideshow --condense myttalk.scm")
   'next
   (blank)
   (page-para "For example, in condensed form, this slide appears"
	      "without steps"))
   
  (slide/title/center
   "Steps and Condensing"
   (page-para "If you condense these slides,"
              "the previous slide's steps will be skipped")
   'next!
   (page-para "Not this slide's steps, because it uses" (code 'next!)))
  
  (slide/title/center
   "Condensing Alternatives"
   (page-para "Condensing" (it "does not") "merge" (code 'alts) "alternatives")
   (page-para "But sometimes you want condensing to just use the last alternative")
   'alts~
   (list (list (t "um..."))
         (list (page-para
                (code 'alts~) "creates alternatives where only the last one"
                "is used when condensing"))))

  (slide/title/center
   "Condensing Steps"
   (page-para "The" (code (lib "step.ss" "slideshow"))
	      "provides" (code with-steps~)
	      "where only the last step is included when condensing")
   (page-para "Also, a" (code with-steps) "step name that"
	      "ends with" (code ~) "is skipped when condensing"))

  (slide/title/center
   "Printing and Condensing Your Own Abstractions"
   (page-para "You can customize your slides using" (code printing?)
	      "and" (code condensing?))
   (blank)
   (page-para "This particular slide is"
	      (if printing? "printed" "not printed")
	      "and"
	      (if condense? "condensed" "not condensed"))
   (blank)
   (page-para "When you skip a whole slide, use"
	      (code skip-slides) "to keep page numbers in sync"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Conclusion starts here
  (outline 'end)

  (slide/title/center
   "Your Own Slides"
   (page-para "A Slideshow presentation is a Scheme program in a module,"
              "so to make your own:")
   (scale/improve-new-text ; a macro that improves font selection
    (code (module mytalk (lib "slideshow.ss" "slideshow")
	    ... #,(it "your code here") ...))
    0.9)
   (blank)
   (page-para "For further information, search for"
	      (tt "slideshow") "and" (tt "texpict")
	      "in Help Desk"))
  
  )
