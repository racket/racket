#lang slideshow

;; This example slideshow code is meant to be read while
;;  running the example show.

;; The first few slides are not to be read
;; until you have seen the rest of the tutorial,
;; so we put them in a separate file
(require "initial-ones.rkt")
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
 #:layout 'top
 (t "The")
 (tt "#:layout 'top") 
 (t "option for")
 (tt "slide")
 (t "aligns the slide to the screen top"))

(slide
 #:title "Titles"
 (t "The")
 (tt "#:title") 
 (t "option for")
 (tt "slide")
 (t "supplies a title string"))

(slide
 #:title "Paragraphs"
 (para "Breaking up text into lines is painful, so"
       "the" (tt "para")
       "function takes a mixture of strings and"
       "picts and puts them into a pagaraph")
 (para "It doesn't matter how strings are broken into"
       "parts in the code")
 (para "The" (tt "para") "function puts space"
       "between separate strings"
       ", but not before punctuation" "!"))

(slide
 #:title "Paragraph Alignment"
 (para "The" (tt "slide") "function centers"
       "body picts horizontally, but" (tt "para")
       "makes a picture with left-aligned text")
 (frame (para "The" (tt "frame") "function wraps a frame"
              "around a pict to create a new pict,"
              "so you can easily see this individual pict")))

(slide
 #:title "More Paragraph Alignment"
 (frame
  (para #:align 'center
        "The" (tt "#:align 'center") "option for" (tt "para") "generates"
        "a paragraph with centered lines of text"))
 (frame
  (para #:fill? #f "This line uses the" (tt "#:fill? #f") "option"))
 (para "The" (tt "#:fill? #f") "option creates a paragraph"
            "that is wrapped to fit the slide, but it allows"
            "the resulting pict to be more narrow than the slide"))

(parameterize ([current-para-width (* 4/5 client-w)])
  (let ([apply-para-to-example
         (lambda (fill?)
           (para #:fill? fill? #:align 'right
                 "This paragraph is right-aligned using" 
                 (tt "#:align 'right") ", and" (tt "#:fill?")
                 "is" (tt "#f") "the first time and"
                 (tt "#t") "the second time"))])
    (slide
     #:title "More Alignment"
     (frame
      (para #:align 'right "Of course, there's also" (tt "#:align 'right")))
     (frame (apply-para-to-example #f))
     (para "For comparision, the same text using the default" (tt "#:fill?") ":")
     (frame (apply-para-to-example #t))
     (para "Unless your font happens to make the"
           "first line exactly as the allowed width, the last box will be slightly"
           "wider with extra space to the left"))))

(slide
 #:title "Paragraph Width"
 (para "The" (tt "para") "function by default makes the"
       "paragraph take 2/3 of the slide width")
 (para #:width 300 
       "The" (tt "para") "function also accepts an explicit"
       (tt "#:width") "option, which is 300 for this paragraph"))

(slide
 #:title "Spacing"
 (para "The" (tt "slide") "functions insert space"
            "between each body pict")
 (para "The amount of space is" (number->string (current-gap-size))
            ", which is the value of" (tt "(current-gap-size)")
            ", which defaults to" (tt "gap-size")))

(slide
 #:title "Controlling Space"
 (vc-append (* gap-size 2)
            (para "If you want to control the space,"
                       "simply append the picts yourself to create"
                       "one body pict")
            (para "The first argument to" (tt "vc-append")
                       "determines the space between pictures")
            (para "If the first argument is a pict instead of"
                       "a number, then 0 is used")
            (para "For text in one paragraph, the"
                       (tt "para") "function uses" (tt "(current-line-sep)")
                       ", which returns" (number->string (current-line-sep)))))

(slide
 #:title "Appending Picts"
 (vl-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vl-append")))
 (vc-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vc-append")))
 (vr-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vr-append"))))

(slide
 #:title "Horizontal Appending"
 (hc-append (frame (t "This is")) (frame (vr-append (tt "hc-append")
                                                    (t "obviously"))))
 (ht-append (frame (t "This is")) (frame (vr-append (tt "ht-append")
                                                    (t "obviously"))))
 (hb-append (frame (t "This is")) (frame (vr-append (tt "hb-append")
                                                    (t "obviously")))))

(slide
 #:title "Text Alignment"
 (hbl-append (frame (scale (tt "hbl-append") 1.5))
             (frame (t "aligns text baselines")))
 (para "It's especially useful for font mixtures")
 (hbl-append (frame (scale (tt "htl-append") 1.5))
             (frame (t "is the same for single lines")))
 (para "The difference between" (tt "htl-append")
            "and" (tt "hbl-append") "shows up with multiple lines:")
 (hbl-append (frame (scale (t "bottom lines align") 1.5))
             (frame (vl-append (t "when using") (tt "hbl-append"))))
 (htl-append (frame (scale (t "top lines align") 1.5))
             (frame (vl-append (t "when using") (tt "htl-append")))))

(slide
 #:title "Superimposing"
 (cc-superimpose (t "X") (t "O"))
 (para "The" (tt "cc-superimpose") 
            "function puts picts on top of each other, centered")
 (para "Each of" (tt "l") "," (tt "r") ", and" (tt "c")
            "is matched with each of"
            (tt "t") "," (tt "b") "," (tt "c") "," (tt "bl") ", and" (tt "tl")
            "in all combinations with" (tt "-superimpose"))
 (para "For example," (tt "cbl-superimpose") ":")
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
             (para #:fill? #f
                   "There's a margin, so the ``client'' area is"
              (number->string client-w) "x" (number->string client-h))
             (para #:fill? #f
                   "The font size is" (number->string (current-font-size))))))

(slide
 #:title "Titled Client Area"
 (rb-superimpose
  (cc-superimpose
   (frame titleless-page)
   (para #:fill? #f
         "If you use a title, then" (tt "titleless-page")
         "is the same size as the area left for the body"))
  (t "It's useful")))

(slide
 #:title "Text and Styles"
 (para "Functions exist for" (bt "bold") ","
            (it "italic") ", and even" (bit "bold-italic") "text")
 (para "The" (tt "text") "function gives you more direct control over the"
            (text "font" '(italic . modern) (current-font-size)) ","
            (text "size" (current-main-font) 24) ", and even"
            (text "angle" (current-main-font) (current-font-size) (/ 3.14159 4))))

(require slideshow/code)
(slide
 #:title "Racket Code"
 (para "For Racket code, the" (code slideshow/code)
            "library provides a handy" (code code) "macro for"
            "typesetting literal code")
 (para "The" (code code) "macro uses source-location information"
            "to indent code")
 (code (define (length l)
         (cond
          [(null? l) 0]
          [else (+ 1 (length (cdr l)))]))))

(slide
 #:title "Colors"
 (para "Use the" (colorize (tt "colorize") "blue")
            "function to color most things, including text")
 (frame
  (colorize (para "A" (code colorize)
                       "applies only to sub-picts that do not"
                       "already have a" (colorize (t "color") "green"))
            "red")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part II starts here

(define outline 
  (let ([sub-para (lambda l
                    (para #:width (* 3/4 (current-para-width)) l))])
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

(slide
 #:title "Itemize"
 (item "Bulleted sequences are common in slides")
 (item "The" (code item) "function makes a bulleted"
            "paragraph that is as wide as the slide")
 (item #:bullet (colorize (tt "+") "green")
       "You can set the bullet, if you like, by using the"
       (code #:bullet) "argment to" (code item))
 (subitem "Naturally, there is also" (code subitem)))

(slide
 #:title "Itemize"
 (para "You could write" (code item) "yourself:")
 (code (define (item . l)
         (let ([w (- client-w
                     (pict-width bullet) 
                     (/ gap-size 2))])
           (htl-append (/ gap-size 2)
                       bullet
                       (para #:width w l)))))
 (para "where" (code bullet) "is a constant pict:" bullet))

(slide
 #:title "Grouping and Space"
 (para "Sometimes you want to group items on a slide")
 (item "A bullet goes with a statement")
 (item "And another does, too")
 (blank)
 (para "Creating a zero-sized pict with" (code (blank))
            "effectively doubles the gap, making a space that"
            "often looks right"))

(slide
 #:title "Steps"
 (item "Suppose you want to show only one item at a time")
 'next
 (item "In addition to body picts, the" (code slide) 
            "functions recognize certain staging symbols")
 (item "Use" (code 'next) "in a sequence of" (code slide)
            "arguments to create multiple slides, one"
            "containing only the preceding content, and another"
            "with the remainder")
 'next
 (blank)
 (colorize
  (para #:fill? #f
        (code 'next) "is not tied to" (code item)
        ", though it's often used with items")
  "blue"))


(slide
 #:title "Alternatives"
 (para "Steps can break up a linear slide, but sometimes"
            "you need to replace one thing with something else")
 'alts 
 (list (list 
        (para #:fill? #f "For example, replace this..."))
       (list
        (para #:fill? #f "... with something else")
        'next
        (blank)
        (item "An" (code 'alts) "in a sequence"
                   "must be followed by a list of lists")
        (item "Each list is a sequence, a different conclusion for the slide's sequence")))
 (item "Anything after the list of lists is folded into the last alternative")
 'next
 (blank)
 (para "Of course, you can mix" (code 'alts) "and"
            (code 'next) "in interesting ways"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part III starts here
(outline 'three)

(slide
 #:title "Fancy Picts"
 (para "In part I, we saw some basic pict constructors:" 
            (code t) "," (code vl-append) ", etc.")
 (blank)
 (para "Slideshow provides many more..."))

(slide
 #:title "Bitmaps"
 (para "For example, the" (code bitmap) "function loads a bitmap to display")
 (blank)
 (bitmap (build-path (collection-path "icons") "plt.gif")))

(require mred) ; for message-box
(slide
 #:title "Clickbacks"
 (para "The" (code clickback)
            "function attaches an arbitrary thunk to a pict"
            "for interactive slides")
 (blank)
 (clickback (frame (t "Click Me"))
            (lambda ()
              (message-box "Clicked" "Thanks!"))))

(define (note . l)
  (colorize (para #:align 'right l) "blue"))

(slide
 #:title "Tables"
 (para "The" (code table) "function makes rows and columns")
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

(define orientations 
  (map (lambda (x) (* pi x)) '(0 1/4 1/2 3/4 1 5/4 6/4 7/4)))
(define (show-arrows code-arrow t-arrow arrow)
  (slide
   #:title "Arrows"
   (para "The" code-arrow "function creates an"
              t-arrow "of a given size and orientation (in radians)")
   (blank)
   (para "Simple: " (arrow gap-size pi))
   (blank)
   (para "Fun:")
   (apply
    vc-append
    (current-line-sep)
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

(require slideshow/face)
(slide
 #:title "Faces"
 (para "The" (code slideshow/face)
            "library makes faces")
 (blank)
 (hc-append
  (* 3 gap-size)
  (face 'happy "yellow")
  (face 'badly-embarrassed)))


(slide
 #:title "Arbitrary Drawing"
 (para "The" (code dc) "function provides an escape hatch to the underlying"
            (code racket/draw) "library")
 (para "For example," (code (disk 100)) "is the same as")
 (code
  (dc (lambda (dc dx dy)
        (send dc draw-ellipse dx dy 100 100))
      100 100 0 0))
 (dc (lambda (dc dx dy)
       (send dc draw-ellipse dx dy 100 100))
     100 100 0 0)
 (disk 100))

(slide
 #:title "Frames"
 (item "As we've already seen, the" (code frame)
            "function wraps a" (frame (t "frame"))
            "around a pict")
 (item "The" (code #:color)
            "option wraps a" (frame (t "colored frame") #:color "red")
            "; compare to" (code frame) "followed by" (code colorize)
            "," (colorize (frame (t "like this")) "red"))
 (blank)
 (item "One way to increase the" (linewidth 3 (frame (t "line thickness")))
            "is to use" (code linewidth))
 (blank)
 (item "It's often useful to" (frame (inset (t "add space") 3))
            "around a pict with" (code inset) "before framing it"))

(define (bound-frame p)
  (frame p #:color "green"))

(slide
 #:title "Lines and Pict Dimensions"
 (item "The" (code hline) "function creates a horizontal line, given a bounding width and height:")
 (bound-frame (hline (* 2 gap-size) gap-size))
 (note "(The" (code hline) "result is framed in green above)")
 (item "Naturally, there's also" (code vline) ":")
 (bound-frame (vline (* 2 gap-size) gap-size))
 (blank)
 (item "To" (let ([p (t "underline")])
                   (vc-append p (hline (pict-width p) 1)))
            "a pict, get its width using" (code pict-width)
            ", then use" (code hline) "and" (code vc-append))
 (item "If the pict is text, you can restore the"
            (let ([p (t "baseline")])
              (refocus 
               (vc-append p (hline (pict-width p) 1))
               p))
            "using" (code refocus)))

(slide
 #:title "Placing Picts"
 (item "Another" (let ([p (t "underline strategy")])
                        (pin-over p 0 (pict-height p) (hline (pict-width p) 1)))
            "is to use" (code pin-over) ", which places one pict on"
            "top of another to generate a new pict")
 (item "The new pict has the"
            (bound-frame
             (let ([p (t "original")])
               (pin-over p 0 (pict-height p) (hline (pict-width p) 1))))
            "pict's bounding box and baselines")
 (note "(The green frame is the ``bounding box'' of the result)")
 (blank)
 (item "The" (code pin-over) "function is useful with" (code pip-arrow-line)
            "to draw an"
            (let ([p (bound-frame (inset (t "outgoing arrow") 2))])
              (pin-over p (/ (pict-width p) 2)  (/ (pict-height p) 2) 
                        ;; pip-arrow-line creates a pict where the
                        ;; "bounding box" corresponds to the non-arrow end
                        (linewidth 3 (colorize (pip-arrow-line 50 50 gap-size) "orange"))))
            "without changing the layout"))

(define blue-fish (standard-fish (* 3 gap-size) (* 2 gap-size) 
                                 #:direction 'right 
                                 #:color "blue" 
                                 #:eye-color "white"))
(define plain-file (file-icon (* 2 gap-size) (* 3 gap-size) #t))
(define fish-file-scene (bound-frame 
                         (inset (ht-append (* 4 gap-size) 
                                           blue-fish 
                                           (inset plain-file 0 (* 2 gap-size) 0 0))
                                gap-size)))

(slide
 #:title "Finding Picts"
 (para "Typically, an arrow needs to go from one pict to another")
 (para "Functions like" (code rc-find) "locate a point of a pict (such as ``right center'') inside a larger pict")
 (let-values ([(fdx fdy) (rc-find fish-file-scene blue-fish)]
              [(adx ady) (lt-find fish-file-scene plain-file)])
   (pin-over fish-file-scene
             fdx fdy
             (colorize
              (pip-arrow-line (- adx fdx) (- ady fdy) gap-size)
              "orange")))
 (para "There's a" (code -find) "function for every combination of"
            (code l) "," (code c) ", and" (code r)
            "with" (code t) "," (code c) "," (code b)
            "," (code bl) ", and" (code tl)))

(slide
 #:title "Connecting with Arrows"
 (para "Actually, straight-arrow combinations are so common that"
            "Slideshow provides"
            (code pin-arrow-line))
 (pin-arrow-line gap-size fish-file-scene
                 blue-fish rc-find
                 plain-file lt-find
                 #:color "orange"))

(require slideshow/balloon)
(slide
 #:title "Balloons"
 (para "The" (code slideshow/balloon)
            "library provides cartoon balloons ---"
            "another reason to use" (code -find) "functions")
 (let* ([orig fish-file-scene]
        [w/fish (pin-balloon (wrap-balloon (t "Fish") 'ne 0 (- gap-size))
                             orig blue-fish cb-find)]
        [w/file (pin-balloon (wrap-balloon (t "File") 'nw (* -2 gap-size) 0)
                             w/fish
                             plain-file rc-find)])
   w/file))

(slide
 #:title "Ghosting"
 (para "The" (code ghost) "function turns a picture invisible")
 (para "For example, the figure on the left and the"
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

(define (mk-link s)
  (let ([p (t s)])
    (colorize
     (pin-over p 0 (pict-height p)
               (linewidth 2 (hline (pict-width p) 2)))
     "blue")))

(define (run-example-talk f)
  (let ([c (make-custodian)])
    (parameterize ([current-namespace (make-gui-namespace)]
                   [current-command-line-arguments
                    (vector (path->string
                             (build-path (collection-path "slideshow")
                                         "examples"
                                         f)))]
                   [current-custodian c]
                   [exit-handler (lambda (v)
                                   (custodian-shutdown-all c))]
                   [current-directory (build-path (collection-path "slideshow")
                                                  "examples")])
      (parameterize ([current-eventspace (make-eventspace)])
        (queue-callback
         (lambda () (dynamic-require 'slideshow/start #f)))))))

(require mred/edit)
(define (show-example-code f)
  (new-text-frame (path->string (build-path (collection-path "slideshow")
                                            "examples"
                                            f))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part IV
(outline 'four)

(define (in-picture-slide show-fish show-file show-link)
  (slide
   #:title "In-Picture Sequences"
   (para "Although" (code 'next) "and" (code 'alts)
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
    (para #:align 'right
          "Larger example:" 
          (clickback (mk-link "run")
                     (lambda ()
                       (run-example-talk "interlocking-components.scm")))
          (clickback (mk-link "code")
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

(require slideshow/step)
(void
 (with-steps
  (intro basic-stx example-steps with-vonly with-vafter the-rest)
  (slide
   #:title "Named Steps"
   (para "The" (code slideshow/step)
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
     (para "A" (code with-steps) "form has a sequences of step names"
                "followed by an expression to evaluate once for each step"))
    ((vonly example-steps)
     (para "For example, the above has three steps:"
                (code intro) "," (code detail)
                ", and" (code conclusion)))
    ((vonly with-vonly)
     (vl-append
      gap-size
      (para "In the body expression, use"
                 (code ((vonly _step-name) _pict-expr))
                 "to make" (code _pict-expr)
                 "visible only during" (code _step-name))
      (para "The expression" (code (vonly _step-name))
                 "produces either" (code ghost)
                 "or the identity function")))
    ((vonly with-vafter)
     (para "Use"
                (code ((vafter _step-name) _pict-expr))
                "to make" (code _pict-expr)
                "visible after" (code _step-name)))
    ((vonly the-rest)
     (para "There's also" (code vbefore) "," (code vbetween) ", and more"))))))


(define smiley (face 'happy))
(define desc (para "The" (code #:timeout)
                        "option causes a slide to auto-advance,"
                        "which can be used for animations."))

(define (scroll-slide timeout t)
  (slide
   #:title "Animations"
   #:timeout timeout
   desc
   (let ([p (hc-append (* 3 gap-size) 
                       (ghost smiley)
                       (ghost smiley))])
     (pin-over p
               (* t (- (pict-width p) (pict-width smiley)))
               0
               smiley))
   (blank)
   ((if (equal? t 1) values ghost)
    (para "(The face moved from left to right)"))))
(scroll-slide #f 0)
(for ([i (in-range 9)])
  (scroll-slide 0.05 (/ (add1 i) 10.0)))
(scroll-slide #f 1)

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
                     (para (titlet s)) 
                     c)
          c)))))

(outline 'background)

(slide
 #:title "Controlling the Background"
 (para "The" (code current-slide-assembler)
            "parameter lets you change the overall look of a slide")
 (para "For this slide and the previous one, the assembler")
 (item "Colorizes the uncolored content as dark red")
 (item "Left-aligns the title")
 (item "Draws a fading box around the slide"))

(current-slide-assembler orig-assembler)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing and Condensing
(outline 'printing)

(slide
 #:title "Printing"
 (para "To export a set of slides as PostScript, "
            "use the" (tt "slideshow") "command-line program:")
 (tt "slideshow --print mytalk.rkt")
 (blank)
 (para "Slideshow steps through slides while producing PostScript pages")
 (para "The slides will look bad on the screen --- because"
            "text is measured for printing instead of screen display"
            "--- but the PostScript will be fine"))

(slide
 #:title "Condensing"
 (para "Often, it makes sense to eliminate" (code 'step)
            "staging when printing slides:")
 (tt "slideshow --print --condense mytalk.rkt")
 (blank)
 'next
 (para "You can also condense without printing")
 (tt "slideshow --condense mytalk.rkt")
 'next
 (blank)
 (para "For example, in condensed form, this slide appears"
            "without steps"))

(slide
 #:title "Steps and Condensing"
 (para "If you condense these slides,"
            "the previous slide's steps will be skipped")
 'next!
 (para "Not this slide's steps, because it uses" (code 'next!)))

(slide
 #:title "Condensing Alternatives"
 (para "Condensing" (it "does not") "merge" (code 'alts) "alternatives")
 (para "But sometimes you want condensing to just use the last alternative")
 'alts~
 (list (list (t "um..."))
       (list (para
              (code 'alts~) "creates alternatives where only the last one"
              "is used when condensing"))))

(slide
 #:title "Condensing Steps"
 (para "The" (code slideshow/step)
            "provides" (code with-steps~)
            "where only the last step is included when condensing")
 (para "Also, a" (code with-steps) "step name that"
            "ends with" (code ~) "is skipped when condensing"))

(slide
 #:title "Printing and Condensing Your Own Abstractions"
 (para "You can customize your slides using" (code printing?)
            "and" (code condensing?))
 (blank)
 (para "This particular slide is"
            (if printing? "printed" "not printed")
            "and"
            (if condense? "condensed" "not condensed"))
 (blank)
 (para "When you skip a whole slide, use"
            (code skip-slides) "to keep page numbers in sync"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conclusion starts here
(outline 'end)

(slide
 #:title "Your Own Slides"
 (para "A Slideshow presentation is a Racket program in a module,"
            "so to make your own:")
 (scale/improve-new-text ; a macro that improves font selection
  (code #,(tt "#lang") slideshow
        ... #,(it "your code here") ...)
  0.9)
 (blank)
 (para "For further information, search for"
            (tt "slideshow") "in the documentation"))
