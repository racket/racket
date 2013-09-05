#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/button}}

@defclass/title[button% object% (control<%>)]{

Whenever a button is clicked by the user, the button's callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each button is created.

@defconstructor[([label (or/c label-string? 
                              (is-a?/c bitmap%)
                              (list/c (is-a?/c bitmap%)
                                      label-string?
                                      (or/c 'left 'top 'right 'bottom)))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c button%) (is-a?/c control-event%) . -> . any) (lambda (b e) (void))]
                 [style (listof (or/c 'border 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a button with a string label, bitmap label, or both.
 @bitmaplabeluse[label] If @racket[label] is a list, then
 the button has both a bitmap and string label, and the
 symbol @racket['left], @racket['top], @racket['right], or @racket['bottom]
 specifies the location of the image relative to the text on the button.

If @litchar{&} occurs in @racket[label] (when @racket[label] includes a
string), it is specially parsed; on Windows and Unix, the character
following @litchar{&} is underlined in the displayed control to
indicate a keyboard mnemonic. (On Mac OS X, mnemonic underlines are
not shown.)  The underlined mnemonic character must be a letter or a
digit. The user can effectively click the button by typing the
mnemonic when the control's top-level-window contains the keyboard
focus. The user must also hold down the Meta or Alt key if the
keyboard focus is currently in a control that handles normal
alphanumeric input. The @litchar{&} itself is removed from
@racket[label] before it is displayed for the control; a @litchar{&&}
in @racket[label] is converted to @litchar{&} (with no mnemonic
underlining). On Mac OS X, a parenthesized mnemonic character is
removed (along with any surrounding space) before the label is
displayed, since a parenthesized mnemonic is often used for non-Roman
languages. Finally, for historical reasons, any text after a tab character is removed on all
platforms. All of these rules are consistent with label handling in
menu items (see @method[labelled-menu-item<%> set-label]). Mnemonic keyboard events are handled by
@method[top-level-window<%> on-traverse-char] (but not on Mac OS
X).

The @racket[callback] procedure is called (with the event type
@indexed-racket['button]) whenever the user clicks the button.

If @racket[style] includes @racket['border], the button is drawn with
a special border that indicates to the user that it is the default
action button (see @method[top-level-window<%>
on-traverse-char]). @DeletedStyleNote[@racket[style] @racket[parent]]{button}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]}


@defmethod[#:mode override
           (set-label [label (or/c label-string? 
                                   (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @racket[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap button. @bitmaplabeluseisbm[label]
 @|bitmapiforiglabel|

If the button has both a string and a bitmap label, then either can be
 set using @method[button% set-label].

}}

