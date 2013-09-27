#lang racket/base

;; Instead of using the `slideshow' language,
;;  we can use scheme/base and import slideshow/base, etc.

(require slideshow/base
         pict
         slideshow/code
         racket/class
         racket/list
         racket/path
         racket/gui/base
         racket/runtime-path)

(define-runtime-path tutorial-show "tutorial-show.rkt")

(provide do-initial-slides)

(define (do-initial-slides)

  (slide
   #:name "Title Slide"
   (titlet "Slideshow Tutorial")
   (blank)
   (size-in-pixels
    (bitmap (collection-file-path "PLT-206.png" "icons")))
   (blank)
   (colorize (it "Press the spacebar to continue") "blue")
   (comment "Welcome to Slideshow"))

  (slide
   #:title "About Slideshow"
   (para (bt "Slideshow")
         "is a library for creating slide presentations")
   (item "A Slideshow presentation is a Racket program")
   (item "Instead of a WYSIWYG interface,"
         "you get the power of Racket"))

  (define sym:rightarrow (t "→"))
  (define sym:leftarrow (t "←"))

  (define (meta key)
    (hbl-append (t "Alt-")
                (if (pict? key) key (tt key))
                (t ", Cmd-")
                (if (pict? key) key (tt key))
                (t ", or Meta-")
                (if (pict? key) key (tt key))))

  (slide
   #:title "How to Control this Viewer"
   (scale/improve-new-text
    (table 3
      (append-map
       (lambda (s) (list (para #:fill? #f (car s)) (t ":") (t (cadr s))))
       `(((,(meta "q")) "end show")
         (("Esc") "if confirmed, end show")
         ((,sym:rightarrow ", Space," ,(tt "f") "," ,(tt "n") ", or click") "next slide")
         ((,sym:leftarrow ", Backspace, Delete, or" ,(tt "b")) "previous slide")
         ((,(tt "1") "/" ,(tt "g")) "first/last slide")
         ((,(tt "a") "/" ,(tt "s")) "previous/next slide name")
         ((,(meta "g")) "select a slide")
         ((,(meta "p")) "show/hide slide number")
         ((,(meta "c")) "show/hide commentary")
         ((,(meta "d")) "show/hide preview")
         ((,(meta "m")) "show/hide mouse cursor")
         ((,(hbl-append (t "Shift-") sym:rightarrow) ", etc.") "move window 1 pixel")
         ((,(meta sym:rightarrow) ", etc.") "move window 10 pixels")))
      lbl-superimpose lbl-superimpose
      gap-size (/ gap-size 2))
    0.9)
   (comment "This window shows comments for each slide. "
            "The comments are typically fill in the details of what "
            "the slide presenter says when giving the talk."))

  (define mytalk.rkt (tt "mytalk.rkt"))

  (slide
   #:title "Slideshow Programs"
   (para "A Slideshow program has the form")
   (scale/improve-new-text
    (code #,(tt "#lang") slideshow
          ... #,(it "code to generate slide content") ...)
    0.9)
   (colorize (hline (* 3/4 client-w) gap-size) "green")
   'alts
   (list (list (para "To run a Slideshow program,")
               (item "Double-click the" (bt "Slideshow") "executable or run" 
                     (tt "slideshow") "on the command line")
               (item "Click the" (bt "Open File...") "link and select the"
                     "Slideshow program file, such as" mytalk.rkt))
         (list (para "Alternately, run a Slideshow program in DrRacket:")
               (item "Open" mytalk.rkt "in DrRacket")
               (item #:bullet (blank (+ (pict-width bullet) gap-size) 0)
                     "DrRacket's language should change automatically to"
                     (bt "Module"))
               (item "Click" (bt "Run") "in DrRacket")
               (colorize (bt "Use DrRacket only if you trust the program") "red"))
         (parameterize ([current-para-width client-w])
           (list (para (colorize (bt "Important security information:") "red"))
                 (para "A slideshow program has access to the"
                       (it "full") (it "Racket") (it "language"))
                 (para "If you don't know the creator of a slide program"
                       "(or if you don't trust them), run the slides through the"
                       (bt "Slideshow") "executable or"
                       (tt "slideshow") "command line")
                 (colorize
                  (para
                   "When run in" (bt "Slideshow") "instead of DrRacket,"
                   "a slide program cannot write files"
                   "or make network connections")
                  "blue")))
         (list (para "When using a command line, you can specify the program directly:")
               (hbl-append (tt "slideshow ") mytalk.rkt)
               (blank)
               (para "To print the talk:")
               (hbl-append (tt "slideshow --print ") mytalk.rkt)
               (blank)
               (colorize
                (para #:align 'right (it "Run") (tt "slideshow --help") (it "for more options"))
                "blue"))))

  (define (sub-para . l)
    (colorize (para #:width (* 3/4 (current-para-width)) l) "blue"))

  (slide
   #:title "Slides and Picts"
   (para "The body of a Slideshow program")
   (item #:bullet (bt " 1.")
         "Makes and combines" (hbl-append (bit "pict") (t "s")))
   (sub-para "For example,")
   (code (t "Hello"))
   (sub-para "creates a pict like this:")
   (colorize (t "Hello") "black")
   (item #:bullet (bt " 2.") "Registers certain picts as slides")
   (sub-para "For example,")
   (code (slide (t "Hello")))
   (sub-para "registers a slide containing only" (colorize (t "Hello") "black")))

  (slide
   #:title "The Rest of the Tutorial"
   (para "The rest of this tutorial (starting with the next slide) is meant to"
         "be viewed while reading the program source")
   (blank)
   (para #:width client-w "The source is")
   (let ([s (path->string tutorial-show)])
     (clickback
      (scale/improve-new-text
       (let ([p (tt s)])
         (colorize
          (pin-over p 0 (pict-height p)
                    (linewidth 2 (hline (pict-width p) 2)))
          "blue"))
       (min 1 (/ (* 0.8 client-w ) (pict-width (tt s)))))
      (lambda ()
        (let* ([f (new frame% [label (path-element->string
                                      (file-name-from-path tutorial-show))]
                       [width 600] [height 400])]
               [e (new text%)]
               [c (new editor-canvas% [parent f] [editor e])])
          (send e load-file s)
          (send e change-style
                (make-object style-delta% 'change-family 'modern)
                0 'end)
          (send f show #t))))))

  )
