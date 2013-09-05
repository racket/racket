(module slide racket/base
  (require racket/unit
           racket/contract
           racket/class
           racket/gui/base
           texpict/mrpict
           texpict/utils
           "sig.rkt"
           "core.rkt"
           "private/utils.rkt"
           "param.rkt")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       Link Config and Viewer with Core        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The actual config and viewer units are determined by a parameter
  ;;  in "param.rkt". This somewhat strange technqiue allows the units
  ;;  to be changed as this module is loaded in a new namespace; see
  ;;  "slides-to-picts.rkt". Such namespace games are not necessary if
  ;;  talks are written as units and linked to the core.rkt unit.

  (define-values/invoke-unit ((current-slideshow-linker) core@)
    (import)
    (export core^
            (prefix config: config^)
            (prefix viewer: viewer^)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                Contracts                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Let the contract check always pass for now. We do more specific checking.
  (define (slide-sequence? l) #t)

  (define slide-contract
    (() () #:rest slide-sequence? . ->* . any))
  (define slide/title-contract
    ((string?) () #:rest slide-sequence? . ->* . any))
  (define slide/inset-contract
    ((sinset?) () #:rest slide-sequence? . ->* . any))
  (define slide/title/inset-contract
    ((string? sinset?) () #:rest slide-sequence? . ->* . any))

  (define (side-inset? n) (and (number? n)
			       (exact? n)
			       (integer? n)
			       (n . >= . 0)))

  (define elem/c (flat-rec-contract elem/c (or/c string? pict? (listof elem/c))))
  (define item-contract (() (#:bullet pict?
                                      #:width real? 
                                      #:align (or/c 'left 'center 'right)
                                      #:fill? any/c 
                                      #:decode? any/c)
                         #:rest elem/c
                         . ->* . pict?))

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
		    [comment (() () #:rest (listof (or/c string? pict?)) . ->* . any)]
                    [para/kw (() (#:width real? 
                                          #:align (or/c 'left 'center 'right)
                                          #:fill? any/c 
                                          #:decode? any/c)
                              #:rest elem/c
                             . ->* . pict?)]
                    [item/kw item-contract]
                    [subitem/kw item-contract]
                    [t (string? . -> . pict?)]
                    [bt (string? . -> . pict?)]
                    [it (string? . -> . pict?)]
                    [bit (string? . -> . pict?)]
                    [tt (string? . -> . pict?)]
                    [titlet (string? . -> . pict?)]
                    [rt (string? . -> . pict?)]
                    [tt* (() () #:rest (listof string?) . ->* . pict?)])
  (provide slide/kw
           most-recent-slide retract-most-recent-slide re-slide slide->pict start-at-recent-slide
	   scroll-transition pause-transition
	   make-outline
	   item item* page-item page-item*
	   item/bullet item*/bullet page-item/bullet page-item*/bullet
	   subitem subitem* page-subitem page-subitem*
	   itemize itemize* page-itemize page-itemize*
	   para para* page-para page-para*
	   para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
	   font-size gap-size current-gap-size current-font-size current-line-sep line-sep title-size 
	   main-font current-main-font with-font current-title-color
	   red green blue purple orange size-in-pixels
	   bullet o-bullet
	   margin get-margin set-margin! 
	   client-w client-h get-client-w get-client-h
	   full-page titleless-page get-full-page get-titleless-page
	   printing? condense?
	   skip-slides
	   set-use-background-frame!
	   enable-click-advance!
	   title-h get-title-h set-title-h! current-slide-assembler
	   current-page-number-font current-page-number-color current-page-number-adjust
           current-titlet current-para-width
	   set-page-numbers-visible! done-making-slides
           set-spotlight-style!
           slide/timeout
           slide/title/timeout
           slide/center/timeout
           slide/title/center/timeout
           (rename-out [sinset? slide-inset?]))
  (provide/contract [clickback 
		     ((pict? (-> any))
		      (any/c)
		      . ->* .
		      pict?)]
		    [interactive
		     (pict? 
                      ((is-a?/c frame%) . -> . (-> any))
                      . -> .
		      pict?)]
		    [make-slide-inset
		     (side-inset? side-inset? side-inset? side-inset?
				  . -> .
				  sinset?)]
		    [apply-slide-inset
		     (sinset? pict? . -> . pict?)])
  ;; Things not at all in the core unit:
  (provide (all-from-out texpict/mrpict)
	   (all-from-out texpict/utils)))
