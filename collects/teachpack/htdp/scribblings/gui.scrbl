#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/gui))

@teachpack["gui"]{Simple Graphical User Interfaces}

@;declare-exporting[teachpack/htdp/gui]
@defmodule[#:require-form beginner-require htdp/gui]

The teachpack provides functions for creating and manipulating graphical
user interfaces. We recommend using @racketmodname[2htdp/universe] instead. 

@deftech{Window} A @tech{Window} is a data representation of a visible
window on your computer screen.

@deftech{GUI-ITEM} A @tech{GUI-Item} is a data representation of an active
component of a window on your computer screen.

@defproc[(create-window [g (listof (listof (unsyntax @tech{GUI-ITEM})))])  Window]{
Creates a window from the ``matrix'' of gui items @racket[g].}

@defproc[(window? [x any/c]) boolean?]{Is the given value a window?}

@defproc[(show-window [w Window]) true]{Shows @racket[w].}

@defproc[(hide-window [w window]) true]{Hides @racket[w].}

@defproc[(make-button [label string>][callback (-> event%  boolean)])  (unsyntax @tech{GUI-ITEM})]{Creates a
button with @racket[label] and @racket[callback] function. The latter
receives an argument that it may safely ignore.}

@defproc[(make-message [msg string?]) (unsyntax @tech{GUI-ITEM})]{Creates a message item from @racket[msg].}

@defproc[(draw-message [g (unsyntax @tech{GUI-ITEM})][m string?])  true]{Displays @racket[m]
in message item @racket[g] and erases the current message.}

@defproc[(make-text [txt string?]) (unsyntax @tech{GUI-ITEM})]{Creates an text editor (with
label @racket[txt]) that allows users to enter text.}

@defproc[(text-contents [g (unsyntax @tech{GUI-ITEM})])  string?]{
Determines the current contents of a text @tech{GUI-ITEM}.}

@defproc[(make-choice [choices (listof string?)]) (unsyntax @tech{GUI-ITEM})]{
Creates a choice menu from @racket[choices] that permits users to choose
from some alternatives.}

@defproc[(choice-index [g (unsyntax @tech{GUI-ITEM})]) natural-number/c]{Determines the
choice that is currently selected in a choice @tech{GUI-ITEM}; the result
is the 0-based index in the choice menu}

Example 1:
@(begin
#reader scribble/comment-reader
(racketblock
> (define w
    (create-window
      (list (list (make-button "QUIT" (lambda (e) (hide-window w)))))))
;; A button appears on the screen.
;; Click on the button and it will disappear.
> (show-window w)
;; The window disappears. 
))

Example 2:
@(begin
#reader scribble/comment-reader
(racketblock
;; text1 : GUI-ITEM
(define text1
  (make-text "Please enter your name"))

;; msg1 : GUI-ITEM
(define msg1
  (make-message (string-append "Hello, World" (make-string 33 #\SPACE))))

;; Event -> true
;; draws the current contents of text1 into msg1, prepended with "Hello, "
(define (respond e)
  (draw-message msg1 (string-append "Hello, " (text-contents text1))))

;; set up window with three "lines":
;;    a text field, a message, and two buttons
;; fill in text and click OKAY
(define w
  (create-window
   (list
    (list text1)
    (list msg1)
    (list (make-button "OKAY" respond)
          (make-button "QUIT" (lambda (e) (hide-window w)))))))
))
