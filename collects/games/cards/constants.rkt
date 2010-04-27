
(module constants mzscheme
  (require mzlib/class
	   mred)

  (provide ANIMATION-STEPS
	   ANIMATION-TIME

	   PRETTY-CARD-SEP-AMOUNT
	   
	   white-brush
	   hilite-brush
	   black-pen
	   dark-gray-pen
	   no-pen
	   black-color
	   nice-font)
  
  (define ANIMATION-STEPS 5)
  (define ANIMATION-TIME 0.3)

  (define PRETTY-CARD-SEP-AMOUNT 5)

  (define black-color
    (make-object color% "black"))

  (define white-brush
    (send the-brush-list
	  find-or-create-brush
	  "white" 'solid))

  (define hilite-brush
    (send the-brush-list
	  find-or-create-brush
	  black-color 'hilite))

  (define black-pen
    (send the-pen-list
	  find-or-create-pen
	  black-color 1 'solid))

  (define dark-gray-pen
    (send the-pen-list
	  find-or-create-pen
	  "dark gray" 1 'solid))

  (define no-pen
    (send the-pen-list
	  find-or-create-pen
	  black-color 1 'transparent))

  (define nice-font
    (send the-font-list
	  find-or-create-font
	  12 'decorative 'normal 'bold
	  #f 'default #t)))

