#lang scribble/doc
@(require "common.rkt")

@defclass/title[ps-setup% object% ()]{

A @racket[ps-setup%] object contains configuration information for
 producing PostScript files using a @racket[post-script-dc%] object.
 To a lesser extent, it contains information for printing with a
 @racket[printer-dc%] object.

When a @racket[post-script-dc%] object is created, its configuration
 is determined by the @racket[current-ps-setup] parameter's
 @racket[ps-setup%] value. After a @racket[post-script-dc%] object is
 created, it is unaffected by changes to the @racket[current-ps-setup]
 parameter or mutations to the @racket[ps-setup%] object.


@defconstructor[()]{

Creates a new @racket[ps-setup%] object with the (platform-specific)
 default configuration.

}

@defmethod[(copy-from [source (is-a?/c ps-setup%)]
                      [copy-filename? any/c #f])
           void?]{

Copies the settings @racket[copy-from] to @this-obj[], excluding the
filename unless @racket[copy-filename?] is true.

}

@defmethod[(get-command)
           string?]{

Historically, gets the printer command used to print a file on
 Unix. The default is @racket["lpr"]. This value is not currently used
 by any platforms.

}

@defmethod[(get-editor-margin [h-margin (box/c (and/c real? (not/c negative?)))]
                              [v-margin (box/c (and/c real? (not/c negative?)))])
           void?]{

Returns the current settings for horizontal and vertical margins when
 printing an @racket[editor<%>]. See also @method[ps-setup%
 set-editor-margin].

}

@defmethod[(get-file)
           (or/c path-string? #f)]{

Gets the PostScript output filename. A @racket[#f] value (the default)
 indicates that the user should be prompted for a filename when a
 @racket[post-script-dc%] object is created.

}

@defmethod[(get-level-2)
           boolean?]{

Reports whether Level 2 commands are output in PostScript files.

Currently, Level 2 commands are only needed to include color bitmap
 images in PostScript output (drawn with @method[dc<%> draw-bitmap]),
 or bitmap pen and brush stipples. When Level 2 commands are disabled,
 bitmaps are converted to grayscale images and stipples are not
 supported.

}

@defmethod[(get-margin [h-margin (box/c (and/c real? (not/c negative?)))]
                       [v-margin (box/c (and/c real? (not/c negative?)))])
           void?]{

Returns the current settings for horizontal and vertical PostScript
 margins. See also @method[ps-setup% set-margin].

}

@defmethod[(get-mode)
           (or/c 'preview 'file 'printer)]{

Gets the printing mode that determines where output is sent:
 @racket['preview], @racket['file], or @racket['printer].  The default
 for X is @racket['preview]. The value in Windows and Mac OS X is
 always @racket['file].

}

@defmethod[(get-orientation)
           (or/c 'portrait 'landscape)]{

Gets the orientation: @racket['portrait] or @racket['landscape]. The
 default is @racket['portrait]. Unlike most other settings, this one
 affects native printing (via @racket[printer-dc%]) as well as
 PostScript output.

Landscaped orientation affects the size of the drawing area as
 reported by @method[dc<%> get-size]: the horizontal and vertical
 sizes determined by the selected paper type are transposed and then
 scaled.

}

@defmethod[(get-paper-name)
           string?]{

Returns the name of the current paper type: @racket["A4 210 x 297 mm"], 
 @racket["A3 297 x 420 mm"], @racket["Letter 8 1/2 x 11 in"], or
 @racket["Legal 8 1/2 x 14 in"]. The default is @racket["Letter 8 1/2 x 11 in"].

The paper name determines the size of the drawing area as reported by
 @method[dc<%> get-size] (along with landscape transformations from
 @method[ps-setup% get-orientation] and/or the scaling factors of
 @method[ps-setup% get-scaling]). It also determines the bounding box
 of PostScript output when a @racket[post-script-dc%] context is
 created with a true value for the @racket[use-paper-bbox?]
 initialization argument.

}

@defmethod[(get-preview-command)
           string?]{

Gets the command used to view a PostScript file for X. The default is
 @racket["gv"]. This value is not used by other platforms.

}

@defmethod[(get-scaling [x (box/c (and/c real? (not/c negative?)))]
                        [y (box/c (and/c real? (not/c negative?)))])
           void?]{

Gets the scaling factor for PostScript output.  @boxisfill[@racket[x]
 @elem{the horizontal scaling factor}] @boxisfill[@racket[y] @elem{the
 vertical scaling factor}] The default is @racket[0.8] by
 @racket[0.8].

This scale is in addition to a scale that can be set by @method[dc<%>
 set-scale] in a @racket[post-script-dc%] context. The size reported
 by @method[dc<%> get-size] is the size of the selected paper type
 (transposed for landscaped mode) divided by this scale.

}

@defmethod[(get-translation [x (box/c (and/c real? (not/c negative?)))]
                            [y (box/c (and/c real? (not/c negative?)))])
           void?]{

Gets the translation (from the bottom left corner) for PostScript
 output.  @boxisfill[@racket[x] @elem{the horizontal offset}]
 @boxisfill[@racket[y] @elem{the vertical offset}] The default is
 @racket[0.0] and @racket[0.0].

The translation is not scaled by the numbers returned from
 @method[ps-setup% get-scaling] and the translation does not affect
 the size of the drawing area.

}

@defmethod[(set-command [command string?])
           void?]{

Historically, sets the printer command that was used to print a file
 on Unix. See @method[ps-setup% get-command].

}

@defmethod[(set-editor-margin [h exact-nonnegative-integer?]
                              [v exact-nonnegative-integer?])
           void?]{

Sets the horizontal and vertical margins used when printing an editor
 with the @method[editor<%> print] method. These margins are always
 used for printing, whether the drawing destination is a
 @racket[post-script-dc%] or @racket[printer-dc%].  The margins are in
 the units of the destination @racket[printer-dc%] or
 @racket[post-script-dc%].  In the case of @racket[post-script-dc%]
 printing, the editor margin is in addition to the PostScript margin
 that is determined by @method[ps-setup% set-margin].

}

@defmethod[(set-file [filename (or/c path-string? #f)])
           void?]{

Sets the PostScript output filename. See
 @method[ps-setup% get-file].

}

@defmethod[(set-level-2 [on? any/c])
           void?]{

Sets whether Level 2 commands are output in PostScript files.  See
 @method[ps-setup% get-level-2].

}

@defmethod[(set-margin [h (and/c real? (not/c negative?))]
                       [v (and/c real? (not/c negative?))])
           void?]{

Sets the horizontal and vertical PostScript margins. When drawing to a
 @racket[post-script-dc%], the page size reported by @method[dc<%>
 get-size] subtracts these margins from the normal page area (before
 taking into account scaling affects). In addition, drawing into the
 @racket[post-script-dc%] produces PostScript output that is offset by
 the margins.

When using the output of a @racket[post-script-dc%] as
 Encapsulated PostScript, the margin values are effectively
 irrelevant. Changing the margins moves the PostScript image in
 absolute coordinates, but it also moves the bounding box.

The margins are in unscaled @racket[post-script-dc%] units, which
 are points. The default margins are 16 points.

}

@defmethod[(set-mode [mode (or/c 'preview 'file 'printer)])
           void?]{

Sets the printing mode controlling where output is sent. See
 @method[ps-setup% get-mode].

On Windows and Mac OS X, if @racket['preview] or @racket['printer]
 is provided, @|MismatchExn|.

}

@defmethod[(set-orientation [orientation (or/c 'portrait 'landscape)])
           void?]{

Sets the orientation. See @method[ps-setup% get-orientation].

}

@defmethod[(set-paper-name [type string?])
           void?]{

Sets the name of the current paper type. See @method[ps-setup%
 get-paper-name].

}

@defmethod[(set-preview-command [command string?])
           void?]{

Sets the command used to view a PostScript file on Unix.  See
@method[ps-setup% get-preview-command].

}

@defmethod[(set-scaling [x (and/c real? (not/c negative?))]
                        [y (and/c real? (not/c negative?))])
           void?]{

Sets the scaling factor for PostScript output. See
 @method[ps-setup% get-scaling].

}

@defmethod[(set-translation [x real?]
                            [y real?])
           void?]{

Sets the translation (from the bottom left corner) for PostScript
 output. See @method[ps-setup% get-translation].

}}
