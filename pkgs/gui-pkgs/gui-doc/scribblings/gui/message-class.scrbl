#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/message}}

@defclass/title[message% object% (control<%>)]{

A message control is a static line of text or a static bitmap. The
 text or bitmap corresponds to the message's label (see
@method[message% set-label]).


@defconstructor[([label (or/c label-string? (is-a?/c bitmap%) 
                              (or/c 'app 'caution 'stop))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
                 [auto-resize any/c #f])]{

Creates a string or bitmap message initially showing @racket[label].
 @bitmaplabeluse[label] An @indexed-racket['app],
 @indexed-racket['caution], or @indexed-racket['stop] symbol for
 @racket[label] indicates an icon; @racket['app] is the application
 icon (Windows and Mac OS X) or a generic ``info'' icon (X),
 @racket['caution] is a caution-sign icon, and @racket['stop] is a
 stop-sign icon.

@labelsimplestripped[@racket[label] @elem{message}]

@DeletedStyleNote[@racket[style] @racket[parent]]{message}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

If @racket[auto-resize] is not @racket[#f], then automatic resizing is
initially enanbled (see @method[message% auto-resize]), and the
@racket[message%] object's @tech{graphical minimum size} is as small as
possible.

}

@defmethod*[([(auto-resize) boolean?]
             [(auto-resize [on? any/c]) void?])]{

Reports or sets whether the @racket[message%]'s @method[area<%> min-width] and
@method[area<%> min-height] are automatically set when the label is changed
via @method[message% set-label].

}

@defmethod[#:mode override
           (set-label [label (or/c label-string? (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @racket[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap message.
 @bitmaplabeluseisbm[label] @|bitmapiforiglabel|

}}

