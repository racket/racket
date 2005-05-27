
(module slide mzscheme
  (require (lib "unitsig.ss")
	   (lib "contract.ss")
	   (lib "mrpict.ss" "texpict")
	   (lib "utils.ss" "texpict")
	   "sig.ss"
	   "core.ss"
	   "util.ss"
	   "param.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       Link Config and Viewer with Core        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The actual config and viewer untis are determined by a parameter
  ;;  in "param.ss". This somewhat strange technqiue allows the units
  ;;  to be changed as this module is loaded in a new namespace; see
  ;;  "slides-to-picts.ss". Such namespace games are not necessary if
  ;;  talks are written as units and linked to the core.ss unit.

  (define-values/invoke-unit/sig ((open core^) 
				  (unit config : config^) 
				  (unit viewer : viewer^))
    ((current-slideshow-linker) core@))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                Contracts                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Let the contract check always pass for now. We do more specific checking.
  (define (slide-sequence? l) #t)

  (define slide-contract
    (() slide-sequence? . ->* . any))
  (define slide/title-contract
    ((string?) slide-sequence? . ->* . any))
  (define slide/inset-contract
    ((sinset?) slide-sequence? . ->* . any))
  (define slide/title/inset-contract
    ((string? sinset?) slide-sequence? . ->* . any))

  (define (side-inset? n) (and (number? n)
			       (exact? n)
			       (integer? n)
			       (n . >= . 0)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                 Exports                       ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Mostly we re-export core^, but we also add contracts.

  ;; Create macros for `get-margin', etc.
  (define-accessor margin get-margin)
  (define-accessor title-h get-title-h)
  (define-accessor client-w get-client-w)
  (define-accessor client-h get-client-h)
  (define-accessor full-page get-full-page)
  (define-accessor titleless-page get-titleless-page)
  
  (provide/contract [slide slide-contract]
		    [slide/title slide/title-contract]
		    [slide/title/tall slide/title-contract]
		    [slide/center slide-contract]
		    [slide/title/center slide/title-contract]
		    [slide/inset slide/inset-contract]
		    [slide/title/inset slide/title/inset-contract]
		    [slide/title/tall/inset slide/title/inset-contract]
		    [slide/center/inset slide/inset-contract]
		    [slide/title/center/inset slide/title/inset-contract]
		    [slide/name slide/title-contract]
		    [slide/name/tall slide/title-contract]
		    [slide/name/center slide/title-contract]
		    [slide/name/inset slide/title/inset-contract]
		    [slide/name/tall/inset slide/title/inset-contract]
		    [slide/name/center/inset slide/title/inset-contract]
		    [comment (() (listof (union string? pict?)) . ->* . any)])
  (provide most-recent-slide retract-most-recent-slide re-slide start-at-recent-slide
	   scroll-transition pause-transition
	   make-outline
	   item item* page-item page-item*
	   item/bullet item*/bullet page-item/bullet page-item*/bullet
	   subitem subitem* page-subitem page-subitem*
	   itemize itemize* page-itemize page-itemize*
	   para para* page-para page-para*
	   para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
	   font-size gap-size current-font-size line-sep title-size 
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
	   set-page-numbers-visible! done-making-slides)
  (provide/contract [clickback 
		     ((pict? (lambda (x)
			       (and (procedure? x)
				    (procedure-arity-includes? x 0))))
		      (any/c)
		      . opt-> .
		      pict?)]
		    [make-slide-inset
		     (side-inset? side-inset? side-inset? side-inset?
				  . -> .
				  sinset?)]
		    [apply-slide-inset
		     (sinset? pict? . -> . pict?)])
  ;; Things not at all in the core unit:
  (provide (all-from (lib "mrpict.ss" "texpict"))
	   (all-from (lib "utils.ss" "texpict"))))
