#lang scribble/doc
@(require "common.ss")

@defclass/title[message% object% (control<%>)]{

A message control is a static line of text or a static bitmap. The
 text or bitmap corresponds to the message's label (see
@method[window<%> set-label]).


@defconstructor[([label (or/c label-string? (is-a?/c bitmap%) 
                              (or-of/c 'app 'caution 'stop))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
                 [auto-resize any/c #f])]{

Creates a string or bitmap message initially showing @scheme[label].
 @bitmaplabeluse[label] An @indexed-scheme['app],
 @indexed-scheme['caution], or @indexed-scheme['stop] symbol for
 @scheme[label] indicates an icon; @scheme['app] is the application
 icon (Windows and Mac OS X) or a generic ``info'' icon (X),
 @scheme['caution] is a caution-sign icon, and @scheme['stop] a
 stop-sign icon.

@labelsimplestripped[(scheme label) @elem{message}]

@DeletedStyleNote[@scheme[style] @scheme[parent]]{message}

@FontKWs[@scheme[font]] @WindowKWs[@scheme[enabled]] @SubareaKWs[] @AreaKWs[]

If @scheme[auto-resize] is not @scheme[#f], then automatic resizing is
initially enanbled (see @method[message% auto-resize]), and the
@scheme[message%] object's @tech{graphical minimum size} is as small as
possible.

}

@defmethod*[([(auto-resize) boolean?]
             [(auto-resize [on? any/c]) void?])]{

Reports or sets whether the @scheme[message%]'s @method[area<%> min-width] and
@method[area<%> min-height] are automatically set when the label is changed
via @method[message% set-label].

}

@defmethod[#:mode override
           (set-label [label (or/c label-string? (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @scheme[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap message.
 @bitmaplabeluseisbm[label] @|bitmapiforiglabel|

}}

