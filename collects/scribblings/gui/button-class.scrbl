#reader(lib "defreader.ss" "scribble")
@require["common.ss"]


@defclass[button% object% (control<%>)]{

Whenever a button is clicked by the user, the buttons's callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each button is created.

@defconstructor[[label (or/c label-string? (is-a/c bitmap%))]
                [parent (or/c (is-a/c frame%) (is-a/c dialog%) (is-a/c panel%) (is-a/c pane%))]
                [callback ((is-a/c button%) (is-a/c control-event%) . -> . any) (lambda (b e) (void))]
                [style (symbols/c deleted border) null]
                [font (is-a/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 2]
                [horiz-margin (integer-in 0 1000) 2]
                [min-width (integer-in 0 10000) @italic{graphical minimum width}]
                [min-height (integer-in 0 10000) @italic{graphical minimum height}]
                [stretchable-width any/c #f]
                [stretchable-height any/c #f]]{

Creates a button with a string or bitmap label.
 label

@labelstripped[(scheme label) @elem{ (when @scheme[label] is a string)} @elem{effectively click the button}]

The @scheme[callback] procedure is called (with the event type
@indexed-scheme['button]) whenever the user clicks the button.

If @scheme[style] includes @scheme['border], the button is drawn with
a special border that indicates to the user that it is the default
action button (see @method[top-level-window<%>
on-traverse-char]). \DeletedStyleNote{button}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]}


@defmethod[#:mode 'add 
           (set-label [label (is-a/c bitmap%)])
           void?]{
@impl{

Sets the bitmap label for a bitmap button. @bitmaplabeluseisbm[label]
 @|bitmapiforiglabel|

}}}

