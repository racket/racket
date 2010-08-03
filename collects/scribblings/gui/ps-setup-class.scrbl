#lang scribble/doc
@(require "common.ss")

@defclass/title[ps-setup% object% ()]{

A @scheme[ps-setup%] object contains configuration information for
 producing PostScript files using a @scheme[post-script-dc%] object.
 To a lesser extent, it contains information for printing with a
 @scheme[printer-dc%] object.

When a @scheme[post-script-dc%] object is created, its configuration
 is determined by the @scheme[current-ps-setup] parameter's
 @scheme[ps-setup%] value. After a @scheme[post-script-dc%] object is
 created, it is unaffected by changes to the @scheme[current-ps-setup]
 parameter or mutations to the @scheme[ps-setup%] object.


@defconstructor[()]{

Creates a new @scheme[ps-setup%] object with the (platform-specific)
 default configuration.

}

@defmethod[(copy-from [source (is-a?/c ps-setup%)]
                      [copy-filename? any/c #f])
           void?]{

Copies the settings @scheme[copy-from] to @this-obj[], excluding the
filename unless @racket[copy-filename?] is true.

}

@defmethod[(get-command)
           string?]{

Gets the printer command used to print a file in X. The default is
 @scheme["lpr"]. This value is not used by other platforms.

}

@defmethod[(get-editor-margin [h-margin (box/c nonnegative-real?)]
                              [v-margin (box/c nonnegative-real?)])
           void?]{

Returns the current settings for horizontal and vertical margins when
 printing an @scheme[editor<%>]. See also @method[ps-setup%
 set-editor-margin].

}

@defmethod[(get-file)
           (or/c path-string? false/c)]{

Gets the PostScript output filename. A @scheme[#f] value (the default)
 indicates that the user should be prompted for a filename when a
 @scheme[post-script-dc%] object is created.

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

@defmethod[(get-margin [h-margin (box/c nonnegative-real?)]
                       [v-margin (box/c nonnegative-real?)])
           void?]{

Returns the current settings for horizontal and vertical PostScript
 margins. See also @method[ps-setup% set-margin].

}

@defmethod[(get-mode)
           (one-of/c 'preview 'file 'printer)]{

Gets the printing mode that determines where output is sent:
 @scheme['preview], @scheme['file], or @scheme['printer].  The default
 for X is @scheme['preview]. The value in Windows and Mac OS X is
 always @scheme['file].

}

@defmethod[(get-orientation)
           (one-of/c 'portrait 'landscape)]{

Gets the orientation: @scheme['portrait] or @scheme['landscape]. The
 default is @scheme['portrait]. Unlike most other settings, this one
 affects native printing (via @scheme[printer-dc%]) as well as
 PostScript output.

Landscaped orientation affects the size of the drawing area as
 reported by @method[dc<%> get-size]: the horizontal and vertical
 sizes determined by the selected paper type are transposed and then
 scaled.

}

@defmethod[(get-paper-name)
           string?]{

Returns the name of the current paper type: @scheme["A4 210 x 297
 mm"], @scheme["A3 297 x 420 mm"], @scheme["Letter 8 1/2 x 11 in"], or
 @scheme["Legal 8 1/2 x 14 in"]. The default is @scheme["Letter 8 1/2
 x 11 in"].

The paper name determines the size of the drawing area as reported by
 @method[dc<%> get-size] (along with landscape transformations from
 @method[ps-setup% get-orientation] and/or the scaling factors of
 @method[ps-setup% get-scaling]). It also determines the bounding box
 of PostScript output when a @scheme[post-script-dc%] context is
 created with a true value for the @scheme[use-paper-bbox?]
 initialization argument.

}

@defmethod[(get-preview-command)
           string?]{

Gets the command used to view a PostScript file for X. The default is
 @scheme["gv"]. This value is not used by other platforms.

}

@defmethod[(get-scaling [x (box/c nonnegative-real?)]
                        [y (box/c nonnegative-real?)])
           void?]{

Gets the scaling factor for PostScript output.  @boxisfill[(scheme x)
 @elem{the horizontal scaling factor}] @boxisfill[(scheme y) @elem{the
 vertical scaling factor}] The default is @scheme[0.8] by
 @scheme[0.8].

This scale is in addition to a scale that can be set by @method[dc<%>
 set-scale] in a @scheme[post-script-dc%] context. The size reported
 by @method[dc<%> get-size] is the size of the selected paper type
 (transposed for landscaped mode) divided by this scale.

}

@defmethod[(get-translation [x (box/c nonnegative-real?)]
                            [y (box/c nonnegative-real?)])
           void?]{

Gets the translation (from the bottom left corner) for PostScript
 output.  @boxisfill[@scheme[x] @elem{the horizontal offset}]
 @boxisfill[@scheme[y] @elem{the vertical offset}] The default is
 @scheme[0.0] and @scheme[0.0].

The translation is not scaled by the numbers returned from
 @method[ps-setup% get-scaling] and the translation does not affect
 the size of the drawing area.

}

@defmethod[(set-command [command string?])
           void?]{

Sets the printer command used to print a file under X. See
 @method[ps-setup% get-command].

}

@defmethod[(set-editor-margin [h exact-nonnegative-integer?]
                              [v exact-nonnegative-integer?])
           void?]{

Sets the horizontal and vertical margins used when printing an editor
 with the @method[editor<%> print] method. These margins are always
 used for printing, whether the drawing destination is a
 @scheme[post-script-dc%] or @scheme[printer-dc%].  The margins are in
 the units of the destination @scheme[printer-dc%] or
 @scheme[post-script-dc%].  In the case of @scheme[post-script-dc%]
 printing, the editor margin is in addition to the PostScript margin
 that is determined by @method[ps-setup% set-margin].

}

@defmethod[(set-file [filename (or/c path-string? false/c)])
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
 @scheme[post-script-dc%], the page size reported by @method[dc<%>
 get-size] subtracts these margins from the normal page area (before
 taking into account scaling affects). In addition, drawing into the
 @scheme[post-script-dc%] produces PostScript output that is offset by
 the margins.

When using the output of a @scheme[post-script-dc%] as
 Encapsulated PostScript, the margin values are effectively
 irrelevant. Changing the margins moves the PostScript image in
 absolute coordinates, but it also moves the bounding box.

The margins are in unscaled @scheme[post-script-dc%] units, which
 are points. The default margins are 16 points.

}

@defmethod[(set-mode [mode (one-of/c 'preview 'file 'printer)])
           void?]{

Sets the printing mode controlling where output is sent. See
 @method[ps-setup% get-mode].

Under Windows and Mac OS X, if @scheme['preview] or @scheme['printer]
 is provided, @|MismatchExn|.

}

@defmethod[(set-orientation [orientation (one-of/c 'portrait 'landscape)])
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

Sets the command used to view a PostScript file under X.  See
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
