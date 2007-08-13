#reader(lib "defreader.ss" "scribble")
@require["common.ss"]


@defclass[button% object% (control<%>)]{

Whenever a button is clicked by the user, the buttons's callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each button is created.

@defconstructor[([label (or/c label-string? (is-a?/c bitmap%))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c button%) (is-a?/c control-event%) . -> . any) (lambda (b e) (void))]
                 [style (one-of/c 'border 'deleted) null]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a button with a string or bitmap label.
 @bitmaplabeluse[label]

If @litchar{&} occurs in @scheme[label] (when @scheme[label] is a
string), it is specially parsed; under Windows and X, the character
following @litchar{&} is underlined in the displayed control to
indicate a keyboard mnemonic. (Under Mac OS X, mnemonic underlines are
not shown.)  The underlined mnemonic character must be a letter or a
digit. The user can effectively click the button by typing the
mnemonic when the control's top-level-window contains the keyboard
focus. The user must also hold down the Meta or Alt key if the
keyboard focus is currently in a control that handles normal
alphanumeric input. The @litchar{&} itself is removed from
@scheme[label] before it is displayed for the control; a @litchar{&&}
in @scheme[label] is converted to @litchar{&} (with no mnemonic
underlining). Under Mac OS X, a parenthesized mnemonic character is
removed (along with any surrounding space) before the label is
displayed, since a parenthesized mnemonic is often used for non-Roman
languages. Finally, any text after a tab character is removed on all
platforms. Mnemonic keyboard events are handled by
@method[top-level-window<%> on-traverse-char] (but not under Mac OS
X).

The @scheme[callback] procedure is called (with the event type
@indexed-scheme['button]) whenever the user clicks the button.

If @scheme[style] includes @scheme['border], the button is drawn with
a special border that indicates to the user that it is the default
action button (see @method[top-level-window<%>
on-traverse-char]). @DeletedStyleNote{button}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]}


@defmethod[#:mode 'add 
           (set-label [label (is-a?/c bitmap%)])
           void?]{

Sets the bitmap label for a bitmap button. @bitmaplabeluseisbm[label]
 @|bitmapiforiglabel|

}}

