
(module base scheme
  (require "slide.ss")

  (current-line-sep 5)

  (case (system-type)
    [(macosx)
     (current-main-font '("Gill Sans" . swiss))]
    [(windows)
     (current-main-font '("Tahoma" . swiss))])
  (current-para-width (* 3/4 client-w))

  (current-titlet (lambda (s)
                    (text s (current-main-font) 40)))
  (set-title-h! (pict-height (titlet "Hi")))

  (provide (rename-out [slide/kw slide])
           comment
           most-recent-slide retract-most-recent-slide re-slide start-at-recent-slide
	   scroll-transition pause-transition
	   make-outline
	   (rename-out [item/kw item]
                       [subitem/kw subitem]
                       [para/kw para])
	   font-size gap-size current-font-size current-line-sep line-sep title-size 
	   main-font current-main-font with-font current-title-color
	   red green blue purple orange size-in-pixels
	   t it bt bit tt titlet tt* rt
	   bullet o-bullet
	   margin get-margin set-margin! 
	   client-w client-h get-client-w get-client-h
	   full-page titleless-page get-full-page get-titleless-page
	   printing? condense?
	   skip-slides
	   set-use-background-frame!
	   enable-click-advance!
	   title-h get-title-h set-title-h! current-slide-assembler
	   current-page-number-font current-page-number-color 
           current-titlet current-para-width
	   set-page-numbers-visible! done-making-slides
           clickback make-slide-inset apply-slide-inset))
