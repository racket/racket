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
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a string or bitmap message initially showing @scheme[label].
 @bitmaplabeluse[label] An @indexed-scheme['app],
 @indexed-scheme['caution], or @indexed-scheme['stop] symbol for
 @scheme[label] indicates an icon; @scheme['app] is the application
 icon (Windows and Mac OS X) or a generic ``info'' icon (X),
 @scheme['caution] is a caution-sign icon, and @scheme['stop] a
 stop-sign icon.

@labelsimplestripped[(scheme label) @elem{message}]

@DeletedStyleNote{message}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]

}

@defmethod[#:mode override
           (set-label [label (or/c label-string? (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @scheme[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap message.
 @bitmaplabeluseisbm[label] @|bitmapiforiglabel|

}}

