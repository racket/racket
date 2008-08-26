#lang scribble/doc
@(require "common.ss")

@defclass/title[style-delta% object% ()]{

A @scheme[style-delta%] object encapsulates a style change. The changes expressible
by a delta include:
@itemize{
@item{changing the font family}
@item{changing the font face}
@item{changing the font size to a new value}
@item{enlarging the font by an additive amount}
@item{enlarging the font by a multiplicative amount, etc.}
@item{changing the font style (normal, @italic{italic}, or {\sl slant})}
@item{toggling the font style}
@item{changing the font to @italic{italic} if it is currently {\sl slant}, etc.}
@item{changing the font weight, etc.}
@item{changing the underline, etc.}
@item{changing the vertical alignment, etc.}
@item{changing the foreground color}
@item{dimming or brightening the foreground color, etc.}
@item{changing the background color, etc.}
@item{changing text backing transparency}
}

The @method[style-delta% set-delta] method is convenient for most
style delta settings; it takes a high-level delta specification and
sets the internal delta information.

To take full advantage of a style delta, it is necessary to understand
the internal on/off settings that can be manipulated through methods
such as @method[style-delta% set-weight-on]. For example, the font
weight change is specified through the @scheme[weight-on] and
@scheme[weight-off] internal settings. Roughly, @scheme[weight-on]
turns on a weight setting when it is not present and
@scheme[weight-off] turns off a weight setting when it is
present. These two interact precisely in the following way:

@itemize{
@item{If both @scheme[weight-on] and @scheme[weight-off] are set to @scheme['base], 
then the font weight is not changed.}
@item{If @scheme[weight-on] is not @scheme['base], then the weight is set to 
@scheme[weight-on].}
@item{If @scheme[weight-off] is not @scheme['base], then the weight will be set back 
to @scheme['normal] when the base style has the weight @scheme[weight-off].}
@item{If both @scheme[weight-on] and @scheme[weight-off] are set to the same
value, then the weight is toggled with respect to that value: if
the base style has the weight @scheme[weight-on], then weight is changed to
@scheme['normal]; if the base style has a different weight, it is changed to
@scheme[weight-on].}
@item{If both @scheme[weight-on] and @scheme[weight-off] are set, but to
different values, then the weight is changed to @scheme[weight-on] 
only when the base style has the weight @scheme[weight-off].}
}

Font styles, smoothing, underlining, and alignment work in an analogous manner.

The possible values for @scheme[alignment-on] and @scheme[alignment-off] are:
@itemize{
@item{@indexed-scheme['base]}
@item{@indexed-scheme['top]}
@item{@indexed-scheme['center]}
@item{@indexed-scheme['bottom]}
}

The possible values for @scheme[style-on] and @scheme[style-off] are:
@itemize{
@item{@indexed-scheme['base]}
@item{@indexed-scheme['normal]}
@item{@indexed-scheme['italic]}
@item{@indexed-scheme['slant]}
}

The possible values for @scheme[smoothing-on] and @scheme[smoothing-off] are:
@itemize{
@item{@indexed-scheme['base]}
@item{@indexed-scheme['default]}
@item{@indexed-scheme['partly-smoothed]}
@item{@indexed-scheme['smoothed]}
@item{@indexed-scheme['unsmoothed]}
}

The possible values for @scheme[underlined-on] and @scheme[underlined-off] are:
@itemize{
@item{@scheme[#f] (acts like @scheme['base])}
@item{@scheme[#t]}
}

The possible values for @scheme[size-in-pixels-on] and @scheme[size-in-pixels-off] are:
@itemize{
@item{@scheme[#f] (acts like @scheme['base])}
@item{@scheme[#t]}
}

The possible values for @scheme[transparent-text-backing-on] and 
@scheme[transparent-text-backing-off] are:
@itemize{
@item{@scheme[#f] (acts like @scheme['base])}
@item{@scheme[#t]}
}

The possible values for @scheme[weight-on] and @scheme[weight-off] are:
@itemize{
@item{@indexed-scheme['base]}
@item{@indexed-scheme['normal]}
@item{@indexed-scheme['bold]}
@item{@indexed-scheme['light]}
}

The family and face settings in a style delta are interdependent:

@itemize{

 @item{When a delta's face is @scheme[#f] and its family is
       @scheme['base], then neither the face nor family are modified by
       the delta.}

 @item{When a delta's face is a string and its family is
       @scheme['base], then only face is modified by the delta.}

 @item{When a delta's family is not @scheme['base], then both the face
       and family are modified by the delta. If the delta's face is
       @scheme[#f], then applying the delta sets a style's face to
       @scheme[#f], so that the family setting prevails in choosing a
       font.}

}




@defconstructor*/make[(([change-command (one-of/c 'change-nothing 
                                                  'change-normal 
                                                  'change-toggle-underline 
                                                  'change-toggle-size-in-pixels 
                                                  'change-normal-color 
                                                  'change-bold) 
                                        'change-nothing])
                       ([change-command (one-of/c 'change-family 
                                                  'change-style 
                                                  'change-toggle-style 
                                                  'change-weight 
                                                  'change-toggle-weight 
                                                  'change-smoothing 
                                                  'change-toggle-smoothing
                                                  'change-alignment)]
                        [v symbol])
                       ([change-command (one-of/c 'change-size 
                                                  'change-bigger 
                                                  'change-smaller)]
                        [v (integer-in 0 255)])
                       ([change-command (one-of/c 'change-underline 
                                                  'change-size-in-pixels)]
                        [v any/c]))]{

The initialization arguments are passed on to 
 @method[style-delta% set-delta].

}


@defmethod[(collapse [delta (is-a?/c style-delta%)])
           boolean?]{

Tries to collapse into a single delta the changes that would be made
 by applying this delta after a given delta. If the return value is
 @scheme[#f], then it is impossible to perform the
 collapse. Otherwise, the return value is @scheme[#t] and this delta
 will contain the collapsed change specification.

}

@defmethod[(copy [delta (is-a?/c style-delta%)])
           void?]{

Copies the given style delta's settings into this one.

}

@defmethod[(equal? [delta (is-a?/c style-delta%)])
           boolean?]{

Returns @scheme[#t] if the given delta is equivalent to this one in
 all contexts or @scheme[#f] otherwise.

}

@defmethod[(get-alignment-off)
           (one-of/c 'base 'top 'center 'bottom)]{

See  @scheme[style-delta%].

}

@defmethod[(get-alignment-on)
           (one-of/c 'base 'top 'center 'bottom)]{

See  @scheme[style-delta%].

}

@defmethod[(get-background-add)
           (is-a?/c add-color<%>)]{

Gets the object additive color shift for the background (applied after
 the multiplicative factor). Call this @scheme[add-color<%>] object's
 methods to change the style delta's additive background color shift.

}

@defmethod[(get-background-mult)
           (is-a?/c mult-color<%>)]{

Gets the multiplicative color shift for the background (applied before
 the additive factor). Call this @scheme[mult-color<%>] object's
 methods to change the style delta's multiplicative background color
 shift.

}

@defmethod[(get-face)
           (or/c string? false/c)]{

Gets the delta's font face string. If this string is @scheme[#f] and the
 family is @indexed-scheme['base] when the delta is applied to a style,
 the style's face and family are not changed. However, if the face
 string is @scheme[#f] and the family is not @indexed-scheme['base], then
 the style's face is changed to @scheme[#f].

See also @method[style-delta% get-family].

}

@defmethod[(get-family)
           (one-of/c 'base 'default 'decorative 'roman 'script 
                     'swiss 'modern 'symbol 'system)]{

Returns the delta's font family. The possible values are
@itemize{
@item{@indexed-scheme['base] --- no change to family}
@item{@indexed-scheme['default]}
@item{@indexed-scheme['decorative]}
@item{@indexed-scheme['roman]}
@item{@indexed-scheme['script]}
@item{@indexed-scheme['swiss]}
@item{@indexed-scheme['modern] (fixed width)}
@item{@indexed-scheme['symbol] (Greek letters)}
@item{@indexed-scheme['system] (used to draw control labels)}
}

See also
@method[style-delta% get-face].

}

@defmethod[(get-foreground-add)
           (is-a?/c add-color<%>)]{

Gets the additive color shift for the foreground (applied after the
 multiplicative factor). Call this @scheme[add-color<%>] object's
 methods to change the style delta's additive foreground color shift.

}

@defmethod[(get-foreground-mult)
           (is-a?/c mult-color<%>)]{

Gets the multiplicative color shift for the foreground (applied before
 the additive factor). Call this @scheme[mult-color<%>] object's
 methods to change the style delta's multiplicative foreground color
 shift.

}

@defmethod[(get-size-add)
           (integer-in 0 255)]{

Gets the additive font size shift (applied after the multiplicative factor).

}

@defmethod[(get-size-in-pixels-off)
           boolean?]{

See  @scheme[style-delta%].

}

@defmethod[(get-size-in-pixels-on)
           boolean?]{

See  @scheme[style-delta%].

}

@defmethod[(get-size-mult)
           real?]{

Gets the multiplicative font size shift (applied before the additive factor).

}

@defmethod[(get-smoothing-off)
           (one-of/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)]{

See  @scheme[style-delta%].

}

@defmethod[(get-smoothing-on)
           (one-of/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)]{See 
@scheme[style-delta%].
}

@defmethod[(get-style-off)
           (one-of/c 'base 'normal 'italic 'slant)]{See 
@scheme[style-delta%].
}

@defmethod[(get-style-on)
           (one-of/c 'base 'normal 'italic 'slant)]{See 
@scheme[style-delta%].
}

@defmethod[(get-transparent-text-backing-off)
           boolean?]{See 
@scheme[style-delta%].
}

@defmethod[(get-transparent-text-backing-on)
           boolean?]{See 
@scheme[style-delta%].
}

@defmethod[(get-underlined-off)
           boolean?]{See 
@scheme[style-delta%].
}

@defmethod[(get-underlined-on)
           boolean?]{See 
@scheme[style-delta%].
}

@defmethod[(get-weight-off)
           (one-of/c 'base 'normal 'bold 'light)]{See 
@scheme[style-delta%].
}

@defmethod[(get-weight-on)
           (one-of/c 'base 'normal 'bold 'light)]{See 
@scheme[style-delta%].
}

@defmethod[(set-alignment-off [v (one-of/c 'base 'top 'center 'bottom)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-alignment-on [v (one-of/c 'base 'top 'center 'bottom)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod*[([(set-delta [change-command (one-of/c 'change-nothing 
                                                   'change-normal 
                                                   'change-toggle-underline 
                                                   'change-toggle-size-in-pixels 
                                                   'change-normal-color 
                                                   'change-bold) 
                                         'change-nothing])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (one-of/c 'change-family 
                                                   'change-style 
                                                   'change-toggle-style 
                                                   'change-weight 
                                                   'change-toggle-weight 
                                                   'change-smoothing 
                                                   'change-toggle-smoothing 
                                                   'change-alignment)]
                         [param symbol])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (one-of/c 'change-size 
                                                   'change-bigger 
                                                   'change-smaller)]
                         [param (integer-in 0 255)])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (one-of/c 'change-underline 
                                                   'change-size-in-pixels)]
                         [on? any/c])
              (is-a?/c style-delta%)])]{

Configures the delta with high-level specifications.  The return value
 is the delta itself.

Except for @scheme['change-nothing] and
 @scheme['change-normal], the command only changes part of the
 delta. Thus, applying @scheme['change-bold] and then
 @scheme['change-italic] sets the delta for both the style and
 weight change.

The @scheme[change-command] argument specifies how the delta is changed;
the possible values are:
@itemize{
@item{@scheme['change-nothing] --- reset all changes}
@item{@scheme['change-normal] --- turn off all styles and resizings}
@item{@scheme['change-toggle-underline] --- underline regions that are currently not underlined, and vice-versa}
@item{@scheme['change-toggle-size-in-pixels] --- interpret sizes in pixels for regions that are currently interpreted in points, and vice-versa}
@item{@scheme['change-normal-color] --- change the foreground and background to black and white, respectively}
@item{@scheme['change-italic] --- change the style of the font to @italic{italic}}
@item{@scheme['change-bold] --- change the weight of the font to @bold{bold}}
@item{@scheme['change-family] --- change the font family (@scheme[param] is a family; see
@scheme[font%]); see also
@method[style-delta% get-family]} @item{@scheme['change-style] --- change the style of the font (@scheme[param] is a style; see
@scheme[font%])}
@item{@scheme['change-toggle-style] --- toggle the style of the font (@scheme[param] is a style; see
@scheme[font%])}
@item{@scheme['change-weight] --- change the weight of the font (@scheme[param] is a weight; see
@scheme[font%])}
@item{@scheme['change-toggle-weight] --- toggle the weight of the font (@scheme[param] is a weight; see
@scheme[font%])}
@item{@scheme['change-smoothing] --- change the smoothing of the font (@scheme[param] is a smoothing; see
@scheme[font%])}
@item{@scheme['change-toggle-smoothing] --- toggle the smoothing of the font (@scheme[param] is a smoothing; see
@scheme[font%])}
@item{@scheme['change-alignment] --- change the alignment (@scheme[param] is an alignment; see
@scheme[style-delta%])}
@item{@scheme['change-size] --- change the size to an absolute value (@scheme[param] is a size)}
@item{@scheme['change-bigger] --- make the text larger (@scheme[param] is an additive amount)}
@item{@scheme['change-smaller] --- make the text smaller (@scheme[param] is an additive amount)}
@item{@scheme['change-underline] --- set the underline status to either underlined or plain}
@item{@scheme['change-size-in-pixels] --- set the size interpretation to pixels or points}
}
}


@defmethod*[([(set-delta-background [name string?])
              (is-a?/c style-delta%)]
             [(set-delta-background [color (is-a?/c color%)])
              (is-a?/c style-delta%)])]{

Makes the delta encode a background color change to match the absolute
 color given; that is, it sets the multiplicative factors to
 @scheme[0.0] in the result of @method[style-delta%
 get-background-mult], and it sets the additive values in the result
 of @method[style-delta% get-background-add] to the specified color's
 values.  The return value of the method is the delta itself.

For the case that a string color name is supplied, see
 @scheme[color-database<%>].

}

@defmethod[(set-delta-face [name string?]
                           [family (one-of/c 'base 'default 'decorative 'roman 
                                             'script 'swiss 'modern 'symbol 'system)
                                   'default])
           (is-a?/c style-delta%)]{

Like @method[style-delta% set-face], but sets the family at the same
 time.

The return value is @this-obj[].

}


@defmethod*[([(set-delta-foreground [name string?])
              (is-a?/c style-delta%)]
             [(set-delta-foreground [color (is-a?/c color%)])
              (is-a?/c style-delta%)])]{

Makes the delta encode a foreground color change to match the absolute
 color given; that is, it sets the multiplicative factors to
 @scheme[0.0] in the result of @method[style-delta%
 get-foreground-mult], and it sets the additive values in the result
 of @method[style-delta% get-foreground-add] to the specified color's
 values.  The return value of the method is the delta itself.

For the case that a string color name is supplied, see
 @scheme[color-database<%>].

}


@defmethod[(set-face [v (or/c string? false/c)])
           void?]{See
@method[style-delta% get-face]. See also
@method[style-delta% set-delta-face].

}

@defmethod[(set-family [v (one-of/c 'base 'default 'decorative 'roman 'script 
                                    'swiss 'modern 'symbol 'system)])
           void?]{
Sets the delta's font family. See
@method[style-delta% get-family].

}

@defmethod[(set-size-add [v (integer-in 0 255)])
           void?]{Sets the additive font size shift (applied
after the multiplicative factor).
}

@defmethod[(set-size-in-pixels-off [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-size-in-pixels-on [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-size-mult [v real?])
           void?]{Sets the multiplicative font size shift (applied
before the additive factor).
}

@defmethod[(set-smoothing-off [v (one-of/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-smoothing-on [v (one-of/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-style-off [v (one-of/c 'base 'normal 'italic 'slant)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-style-on [v (one-of/c 'base 'normal 'italic 'slant)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-transparent-text-backing-off [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-transparent-text-backing-on [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-underlined-off [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-underlined-on [v any/c])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-weight-off [v (one-of/c 'base 'normal 'bold 'light)])
           void?]{See 
@scheme[style-delta%].
}

@defmethod[(set-weight-on [v (one-of/c 'base 'normal 'bold 'light)])
           void?]{See 
@scheme[style-delta%].
}}
