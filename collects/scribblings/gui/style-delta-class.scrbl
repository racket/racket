#lang scribble/doc
@(require "common.rkt")

@defclass/title[style-delta% object% ()]{

A @racket[style-delta%] object encapsulates a style change. The changes
expressible by a delta include:
@itemize[
@item{changing the font family}
@item{changing the font face}
@item{changing the font size to a new value}
@item{enlarging the font by an additive amount}
@item{enlarging the font by a multiplicative amount, etc.}
@item{changing the font style (normal, @italic{italic}, or @slant{slant})}
@item{toggling the font style}
@item{changing the font to @italic{italic} if it is currently @slant{slant}, etc.}
@item{changing the font weight, etc.}
@item{changing the underline, etc.}
@item{changing the vertical alignment, etc.}
@item{changing the foreground color}
@item{dimming or brightening the foreground color, etc.}
@item{changing the background color, etc.}
@item{changing text backing transparency}
]

The @method[style-delta% set-delta] method is convenient for most
style delta settings; it takes a high-level delta specification and
sets the internal delta information.

To take full advantage of a style delta, it is necessary to understand
the internal on/off settings that can be manipulated through methods
such as @method[style-delta% set-weight-on]. For example, the font
weight change is specified through the @racket[weight-on] and
@racket[weight-off] internal settings. Roughly, @racket[weight-on]
turns on a weight setting when it is not present and
@racket[weight-off] turns off a weight setting when it is
present. These two interact precisely in the following way:

@itemize[
@item{If both @racket[weight-on] and @racket[weight-off] are set to @racket['base], 
then the font weight is not changed.}
@item{If @racket[weight-on] is not @racket['base], then the weight is set to 
@racket[weight-on].}
@item{If @racket[weight-off] is not @racket['base], then the weight will be set back 
to @racket['normal] when the base style has the weight @racket[weight-off].}
@item{If both @racket[weight-on] and @racket[weight-off] are set to the same
value, then the weight is toggled with respect to that value: if
the base style has the weight @racket[weight-on], then weight is changed to
@racket['normal]; if the base style has a different weight, it is changed to
@racket[weight-on].}
@item{If both @racket[weight-on] and @racket[weight-off] are set, but to
different values, then the weight is changed to @racket[weight-on] 
only when the base style has the weight @racket[weight-off].}
]

Font styles, smoothing, underlining, and alignment work in an analogous manner.

The possible values for @racket[alignment-on] and @racket[alignment-off] are:
@itemize[
@item{@indexed-racket['base]}
@item{@indexed-racket['top]}
@item{@indexed-racket['center]}
@item{@indexed-racket['bottom]}
]

The possible values for @racket[style-on] and @racket[style-off] are:
@itemize[
@item{@indexed-racket['base]}
@item{@indexed-racket['normal]}
@item{@indexed-racket['italic]}
@item{@indexed-racket['slant]}
]

The possible values for @racket[smoothing-on] and @racket[smoothing-off] are:
@itemize[
@item{@indexed-racket['base]}
@item{@indexed-racket['default]}
@item{@indexed-racket['partly-smoothed]}
@item{@indexed-racket['smoothed]}
@item{@indexed-racket['unsmoothed]}
]

The possible values for @racket[underlined-on] and @racket[underlined-off] are:
@itemize[
@item{@racket[#f] (acts like @racket['base])}
@item{@racket[#t]}
]

The possible values for @racket[size-in-pixels-on] and
@racket[size-in-pixels-off] are:
@itemize[
@item{@racket[#f] (acts like @racket['base])}
@item{@racket[#t]}
]

The possible values for @racket[transparent-text-backing-on] and 
@racket[transparent-text-backing-off] are:
@itemize[
@item{@racket[#f] (acts like @racket['base])}
@item{@racket[#t]}
]

The possible values for @racket[weight-on] and @racket[weight-off] are:
@itemize[
@item{@indexed-racket['base]}
@item{@indexed-racket['normal]}
@item{@indexed-racket['bold]}
@item{@indexed-racket['light]}
]

The family and face settings in a style delta are interdependent:

@itemize[

 @item{When a delta's face is @racket[#f] and its family is
       @racket['base], then neither the face nor family are modified by
       the delta.}

 @item{When a delta's face is a string and its family is
       @racket['base], then only face is modified by the delta.}

 @item{When a delta's family is not @racket['base], then both the face
       and family are modified by the delta. If the delta's face is
       @racket[#f], then applying the delta sets a style's face to
       @racket[#f], so that the family setting prevails in choosing a
       font.}

]


@defconstructor*/make[(([change-command (or/c 'change-nothing
                                              'change-normal
                                              'change-toggle-underline
                                              'change-toggle-size-in-pixels
                                              'change-normal-color
                                              'change-bold)
                                        'change-nothing])
                       ([change-command (or/c 'change-family
                                              'change-style
                                              'change-toggle-style
                                              'change-weight
                                              'change-toggle-weight
                                              'change-smoothing
                                              'change-toggle-smoothing
                                              'change-alignment)]
                        [v symbol])
                       ([change-command (or/c 'change-size
                                              'change-bigger
                                              'change-smaller)]
                        [v byte?])
                       ([change-command (or/c 'change-underline
                                              'change-size-in-pixels)]
                        [v any/c]))]{

The initialization arguments are passed on to
 @method[style-delta% set-delta].
}


@defmethod[(collapse [delta (is-a?/c style-delta%)])
           boolean?]{

Tries to collapse into a single delta the changes that would be made
 by applying this delta after a given delta. If the return value is
 @racket[#f], then it is impossible to perform the
 collapse. Otherwise, the return value is @racket[#t] and this delta
 will contain the collapsed change specification.

}

@defmethod[(copy [delta (is-a?/c style-delta%)]) void?]{
  Copies the given style delta's settings into this one.}

@defmethod[(equal? [delta (is-a?/c style-delta%)]) boolean?]{
  Returns @racket[#t] if the given delta is equivalent to this one in
  all contexts or @racket[#f] otherwise.}

@defmethod[(get-alignment-off) (or/c 'base 'top 'center 'bottom)]{
  See @racket[style-delta%].}

@defmethod[(get-alignment-on) (or/c 'base 'top 'center 'bottom)]{
  See @racket[style-delta%].}

@defmethod[(get-background-add) (is-a?/c add-color<%>)]{

Gets the object additive color shift for the background (applied after
 the multiplicative factor). Call this @racket[add-color<%>] object's
 methods to change the style delta's additive background color shift.

}

@defmethod[(get-background-mult)
           (is-a?/c mult-color<%>)]{

Gets the multiplicative color shift for the background (applied before
 the additive factor). Call this @racket[mult-color<%>] object's
 methods to change the style delta's multiplicative background color
 shift.

}

@defmethod[(get-face)
           (or/c string? #f)]{

Gets the delta's font face string. If this string is @racket[#f] and the
 family is @indexed-racket['base] when the delta is applied to a style,
 the style's face and family are not changed. However, if the face
 string is @racket[#f] and the family is not @indexed-racket['base], then
 the style's face is changed to @racket[#f].

See also @method[style-delta% get-family].

}

@defmethod[(get-family)
           (or/c 'base 'default 'decorative 'roman 'script 
                 'swiss 'modern 'symbol 'system)]{

Returns the delta's font family. The possible values are
@itemize[
@item{@indexed-racket['base] --- no change to family}
@item{@indexed-racket['default]}
@item{@indexed-racket['decorative]}
@item{@indexed-racket['roman]}
@item{@indexed-racket['script]}
@item{@indexed-racket['swiss]}
@item{@indexed-racket['modern] (fixed width)}
@item{@indexed-racket['symbol] (Greek letters)}
@item{@indexed-racket['system] (used to draw control labels)}
]

See also @method[style-delta% get-face].

}

@defmethod[(get-foreground-add) (is-a?/c add-color<%>)]{

Gets the additive color shift for the foreground (applied after the
 multiplicative factor). Call this @racket[add-color<%>] object's
 methods to change the style delta's additive foreground color shift.

}

@defmethod[(get-foreground-mult)
           (is-a?/c mult-color<%>)]{

Gets the multiplicative color shift for the foreground (applied before
 the additive factor). Call this @racket[mult-color<%>] object's
 methods to change the style delta's multiplicative foreground color
 shift.

}

@defmethod[(get-size-add) byte?]{
  Gets the additive font size shift (applied after the multiplicative factor).}

@defmethod[(get-size-in-pixels-off) boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-size-in-pixels-on) boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-size-mult) real?]{
  Gets the multiplicative font size shift (applied before the additive factor).}

@defmethod[(get-smoothing-off)
           (or/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)]{
  See @racket[style-delta%].
}

@defmethod[(get-smoothing-on)
           (or/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed)]{See 
  @racket[style-delta%].
}

@defmethod[(get-style-off)
           (or/c 'base 'normal 'italic 'slant)]{See 
  @racket[style-delta%].
}

@defmethod[(get-style-on) (or/c 'base 'normal 'italic 'slant)]{
  See @racket[style-delta%].}

@defmethod[(get-transparent-text-backing-off) boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-transparent-text-backing-on) boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-underlined-off)
           boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-underlined-on)
           boolean?]{
  See @racket[style-delta%].}

@defmethod[(get-weight-off) (or/c 'base 'normal 'bold 'light)]{
  See @racket[style-delta%].}

@defmethod[(get-weight-on) (or/c 'base 'normal 'bold 'light)]{
  See @racket[style-delta%].}

@defmethod[(set-alignment-off [v (or/c 'base 'top 'center 'bottom)]) void?]{
  See @racket[style-delta%].}

@defmethod[(set-alignment-on [v (or/c 'base 'top 'center 'bottom)]) void?]{
  See @racket[style-delta%].}

@defmethod*[([(set-delta [change-command (or/c 'change-nothing
                                               'change-normal
                                               'change-toggle-underline
                                               'change-toggle-size-in-pixels
                                               'change-normal-color
                                               'change-bold)
                                         'change-nothing])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (or/c 'change-family
                                               'change-style
                                               'change-toggle-style
                                               'change-weight
                                               'change-toggle-weight
                                               'change-smoothing
                                               'change-toggle-smoothing
                                               'change-alignment)]
                         [param symbol])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (or/c 'change-size
                                               'change-bigger
                                               'change-smaller)]
                         [param byte?])
              (is-a?/c style-delta%)]
             [(set-delta [change-command (or/c 'change-underline
                                               'change-size-in-pixels)]
                         [on? any/c])
              (is-a?/c style-delta%)])]{

Configures the delta with high-level specifications.  The return value
 is the delta itself.

Except for @racket['change-nothing] and
 @racket['change-normal], the command only changes part of the
 delta. Thus, applying @racket['change-bold] and then
 @racket['change-italic] sets the delta for both the style and
 weight change.

The @racket[change-command] argument specifies how the delta is changed;
the possible values are:
@itemize[
@item{@racket['change-nothing] --- reset all changes}
@item{@racket['change-normal] --- turn off all styles and resizings}
@item{@racket['change-toggle-underline] --- underline regions that are currently not underlined, and vice versa}
@item{@racket['change-toggle-size-in-pixels] --- interpret sizes in pixels for regions that are currently interpreted in points, and vice versa}
@item{@racket['change-normal-color] --- change the foreground and background to black and white, respectively}
@item{@racket['change-italic] --- change the style of the font to @italic{italic}}
@item{@racket['change-bold] --- change the weight of the font to @bold{bold}}
@item{@racket['change-family] --- change the font family (@racket[param] is a family; see
@racket[font%]); see also
@method[style-delta% get-family]} @item{@racket['change-style] --- change the style of the font (@racket[param] is a style; see
@racket[font%])}
@item{@racket['change-toggle-style] --- toggle the style of the font (@racket[param] is a style; see
@racket[font%])}
@item{@racket['change-weight] --- change the weight of the font (@racket[param] is a weight; see
@racket[font%])}
@item{@racket['change-toggle-weight] --- toggle the weight of the font (@racket[param] is a weight; see
@racket[font%])}
@item{@racket['change-smoothing] --- change the smoothing of the font (@racket[param] is a smoothing; see
@racket[font%])}
@item{@racket['change-toggle-smoothing] --- toggle the smoothing of the font (@racket[param] is a smoothing; see
@racket[font%])}
@item{@racket['change-alignment] --- change the alignment (@racket[param] is an alignment; see
@racket[style-delta%])}
@item{@racket['change-size] --- change the size to an absolute value (@racket[param] is a size)}
@item{@racket['change-bigger] --- make the text larger (@racket[param] is an additive amount)}
@item{@racket['change-smaller] --- make the text smaller (@racket[param] is an additive amount)}
@item{@racket['change-underline] --- set the underline status to either underlined or plain}
@item{@racket['change-size-in-pixels] --- set the size interpretation to pixels or points}
]
}


@defmethod*[([(set-delta-background [name string?])
              (is-a?/c style-delta%)]
             [(set-delta-background [color (is-a?/c color%)])
              (is-a?/c style-delta%)])]{

Makes the delta encode a background color change to match the absolute
 color given; that is, it sets the multiplicative factors to
 @racket[0.0] in the result of @method[style-delta%
 get-background-mult], and it sets the additive values in the result
 of @method[style-delta% get-background-add] to the specified color's
 values.  In addition, it also disables transparent text backing by
 setting @racket[transparent-text-backing-on] to @racket[#f] and
 @racket[transparent-text-backing-off] to @racket[#t].
 The return value of the method is the delta itself.

For the case that a string color name is supplied, see
 @racket[color-database<%>].

}

@defmethod[(set-delta-face [name string?]
                           [family (or/c 'base 'default 'decorative 'roman
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
 @racket[0.0] in the result of @method[style-delta%
 get-foreground-mult], and it sets the additive values in the result
 of @method[style-delta% get-foreground-add] to the specified color's
 values.  The return value of the method is the delta itself.

For the case that a string color name is supplied, see
 @racket[color-database<%>].

}


@defmethod[(set-face [v (or/c string? #f)])
           void?]{See
@method[style-delta% get-face]. See also
@method[style-delta% set-delta-face].

}

@defmethod[(set-family [v (or/c 'base 'default 'decorative 'roman 'script
                                'swiss 'modern 'symbol 'system)])
           void?]{
Sets the delta's font family. See
@method[style-delta% get-family].

}

@defmethod[(set-size-add [v byte?]) void?]{
  Sets the additive font size shift (applied
  after the multiplicative factor).}

@defmethod[(set-size-in-pixels-off [v any/c]) void?]{
  See @racket[style-delta%].}

@defmethod[(set-size-in-pixels-on [v any/c]) void?]{
  See @racket[style-delta%].}

@defmethod[(set-size-mult [v real?]) void?]{
  Sets the multiplicative font size shift (applied before the additive factor).}

@defmethod[(set-smoothing-off [v (or/c 'base 'default 'partly-smoothed
                                       'smoothed 'unsmoothed)])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-smoothing-on [v (or/c 'base 'default 'partly-smoothed
                                      'smoothed 'unsmoothed)])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-style-off [v (or/c 'base 'normal 'italic 'slant)])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-style-on [v (or/c 'base 'normal 'italic 'slant)])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-transparent-text-backing-off [v any/c])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-transparent-text-backing-on [v any/c]) void?]{
  See @racket[style-delta%].}

@defmethod[(set-underlined-off [v any/c]) void?]{
  See @racket[style-delta%].}

@defmethod[(set-underlined-on [v any/c])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-weight-off [v (or/c 'base 'normal 'bold 'light)])
           void?]{
  See @racket[style-delta%].}

@defmethod[(set-weight-on [v (or/c 'base 'normal 'bold 'light)])
           void?]{
  See @racket[style-delta%].}

}
