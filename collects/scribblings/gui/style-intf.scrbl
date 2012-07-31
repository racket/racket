#lang scribble/doc
@(require "common.rkt")

@definterface/title[style<%> ()]{

A @racket[style<%>] object encapsulates drawing information (font,
 color, alignment, etc.) in a hierarchical manner. A @racket[style<%>]
 object always exists within the context of a @racket[style-list%]
 object and is never created except by a @racket[style-list%] object.

See also @|stylediscuss|.


@defmethod[(get-alignment)
           (or/c 'top 'center 'bottom)]{

Returns the style's alignment: @racket['top], @racket['center], or
 @racket['bottom].

}


@defmethod[(get-background)
           (is-a?/c color%)]{

Returns the style's background color.

}


@defmethod[(get-base-style)
           (or/c (is-a?/c style<%>) #f)]{

Returns the style's base style. See @|stylediscuss| for more
 information. The return value is @racket[#f] only for the basic style
 in the list.

}

@defmethod[(get-delta [delta (is-a?/c style-delta%)])
           void?]{

Mutates @racket[delta], changing it to match the style's delta, if the style is not a join
 style. See @|stylediscuss| for more information.

}

@defmethod[(get-face)
           (or/c string? #f)]{

Returns the style's face name. See @racket[font%].

}


@defmethod[(get-family)
           (or/c 'default 'decorative 'roman 'script 
                 'swiss 'modern 'symbol 'system)]{

Returns the style's font family. See @racket[font%].

}

@defmethod[(get-font)
           (is-a?/c font%)]{

Returns the style's font information.

}

@defmethod[(get-foreground)
           (is-a?/c color%)]{

Returns the style's foreground color.

}

@defmethod[(get-name)
           (or/c string? #f)]{

Returns the style's name, or @racket[#f] if it is unnamed. Style names
 are only set through the style's @racket[style-list%] object.

}

@defmethod[(get-shift-style)
           (is-a?/c style<%>)]{

Returns the style's shift style if it is a join style. Otherwise, the
 root style is returned. See @|stylediscuss| for more information.

}

@defmethod[(get-size) byte?]{
  Returns the style's font size.}

@defmethod[(get-size-in-pixels)
           boolean?]{

Returns @racket[#t] if the style size is in pixels, instead of points,
 or @racket[#f] otherwise.

}

@defmethod[(get-smoothing)
           (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed)]{

Returns the style's font smoothing. See @racket[font%].

}

@defmethod[(get-style)
           (or/c 'normal 'italic 'slant)]{

Returns the style's font style. See @racket[font%].

}

@defmethod[(get-text-descent [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the descent of text using this style in a given DC.

}

@defmethod[(get-text-height [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the height of text using this style in a given DC.

}

@defmethod[(get-text-space [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the vertical spacing for text using this style in a given DC.

}

@defmethod[(get-text-width [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the width of a space character using this style in a given
DC.

}

@defmethod[(get-transparent-text-backing)
           boolean?]{

Returns @racket[#t] if text is drawn without erasing the
 text background or @racket[#f] otherwise.

}

@defmethod[(get-underlined)
           boolean?]{

Returns @racket[#t] if the style is underlined or @racket[#f]
 otherwise.

}

@defmethod[(get-weight)
           (or/c 'normal 'bold 'light)]{

Returns the style's font weight. See @racket[font%].

}

@defmethod[(is-join?)
           boolean?]{

Returns @racket[#t] if the style is a join style or @racket[#f]
 otherwise. See @|stylediscuss| for more information.

}

@defmethod[(set-base-style [base-style (is-a?/c style<%>)])
           void?]{

Sets the style's base style and recomputes the style's font, etc. See
 @|stylediscuss| for more information.

}

@defmethod[(set-delta [delta (is-a?/c style-delta%)])
           void?]{

Sets the style's delta (if it is not a join style) and recomputes the
style's font, etc. See @|stylediscuss| for more information.

}

@defmethod[(set-shift-style [style (is-a?/c style<%>)])
           void?]{

Sets the style's shift style (if it is a join style) and recomputes
the style's font, etc. See @|stylediscuss| for more information.

}

@defmethod[(switch-to [dc (is-a?/c dc<%>)]
                      [old-style (or/c (is-a?/c style<%>) #f)])
           void?]{

Sets the font, pen color, etc. of the given drawing context. If
 @racket[oldstyle] is not @racket[#f], only differences between the
 given style and this one are applied to the drawing context.

}}

