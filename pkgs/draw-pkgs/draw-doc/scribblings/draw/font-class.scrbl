#lang scribble/doc
@(require "common.rkt")

@defclass/title[font% object% ()]{

A @defterm{font} is an object which determines the appearance of text,
 primarily when drawing text to a device context. A font is determined
 by seven properties:

@itemize[

 @item{size --- The size of the text, either in points (the default)
                or logical drawing units, depending on the
                ``size-in-pixels?'' property (see below).}

 @item{family --- A platform- and device-independent font
                  designation. The families are:

 @itemize[
 @item{@indexed-racket['default]}
 @item{@indexed-racket['decorative]}
 @item{@indexed-racket['roman]}
 @item{@indexed-racket['script]}
 @item{@indexed-racket['swiss]}
 @item{@indexed-racket['modern] (fixed width)}
 @item{@indexed-racket['symbol] (Greek letters and more)}
 @item{@indexed-racket['system] (similar to the font to draw control labels,
                                 but see @racket[normal-control-font])}
 ]

 @margin-note{The terminology ``family'' and ``face'' is mangled
              relative to its usual meaning. A @racket[font%] ``face''
              is really used more like a font family in the usual
              terminology or more generally as a face-description
              string that is combined with other @racket[font%]
              attributes to arrive at a face. A @racket[font%]
              ``family'' is a kind of abstract font family that is
              mapped to a particular font family on a given
              platform.}}

 @item{face --- A string face name, such as @racket["Courier"]. The format
                and meaning of a face name is platform- and
                device-specific. If a font's face name is @racket[#f],
                then the font's appearance depends only on the
                family. If a face is provided but no mapping is
                available for the face name (for a specific platform
                or device), then the face name is ignored and the
                family is used. See @racket[font-name-directory<%>]
                for information about how face names are mapped for
                drawing text.}

@item{style --- The slant style of the font, one of:
 @itemize[
 @item{@indexed-racket['normal]}
 @item{@indexed-racket['slant] (a.k.a ``oblique'')}
 @item{@indexed-racket['italic]}
 ]}

@item{weight --- The weight of the font, one of:
 @itemize[
 @item{@indexed-racket['normal]}
 @item{@indexed-racket['light]}
 @item{@indexed-racket['bold]}
 ]}

@item{underline? --- @racket[#t] for underlined, @racket[#f] for plain.}

@item{smoothing --- Amount of anti-alias smoothing, one of:
 @itemize[
 @item{@indexed-racket['default] (platform-specific, sometimes user-configurable)}
 @item{@indexed-racket['partly-smoothed] (gray anti-aliasing)}
 @item{@indexed-racket['smoothed] (sub-pixel anti-aliasing)}
 @item{@indexed-racket['unsmoothed]}
 ]}

@item{size-in-pixels? --- @racket[#t] if the size of the font
 is in logical drawing units (i.e., pixels for an unscaled screen or
 bitmap drawing context), @racket[#f] if the size of the font is in
 ``points'', where a ``point'' is equal to 1 pixel on Max OS X and
 @racket[(/ 96 72)] pixels on Windows and Unix}

@item{hinting --- Whether font metrics should be rounded to integers:
 @itemize[
   @item{@indexed-racket['aligned] (the default) --- rounds to integers
    to improve the consistency of letter spacing for pixel-based
    targets, but at the expense of making metrics unscalable}
   @item{@indexed-racket['unaligned] --- disables rounding}
 ]}

]

To avoid creating multiple fonts with the same characteristics, use
 the global @racket[font-list%] object @indexed-racket[the-font-list].

See also
@racket[font-name-directory<%>].

@history[#:changed "1.2" @elem{Defined ``points'' as  @racket[(/ 96 72)] pixels on Windows,
                               independent of the screen resolution.}]


@defconstructor*/make[(()
                       ([size (integer-in 1 1024)]
                        [family font-family/c]
                        [style font-style/c 'normal]
                        [weight font-weight/c 'normal]
                        [underline? any/c #f]
                        [smoothing font-smoothing/c 'default]
                        [size-in-pixels? any/c #f]
                        [hinting font-hinting/c 'aligned])
                       ([size (integer-in 1 1024)]
                        [face string?]
                        [family font-family/c]
                        [style font-style/c 'normal]
                        [weight font-weight/c 'normal]
                        [underline? any/c #f]
                        [smoothing font-smoothing/c 'default]
                        [size-in-pixels? any/c #f]
                        [hinting font-hinting/c 'aligned]))]{

When no arguments are provided, creates an instance of the default
 font. If no face name is provided, the font is created without a face
 name.

See @racket[font%] for information about @racket[family],
 @racket[style], @racket[weight], @racket[smoothing],
 @racket[size-in-pixels?], and @racket[hinting].
 @racket[font-name-directory<%>].

See also @racket[make-font].

}

@defmethod[(get-face)
           (or/c string? #f)]{

Gets the font's face name, or @racket[#f] if none is specified.

}

@defmethod[(get-family) font-family/c]{

Gets the font's family. See @racket[font%] for information about
families.

}

@defmethod[(get-font-id)
           exact-integer?]{

Gets the font's ID, for use with a
@racket[font-name-directory<%>]. The ID is determined by the font's
face and family specifications, only.

}

@defmethod[(get-hinting) font-hinting/c]{

Gets the font's hinting. See @racket[font%] for information about
hinting.

}

@defmethod[(get-point-size)
           (integer-in 1 1024)]{

Gets the font's size (roughly the height). Despite the method name,
 the size may be in logical units instead of points, depending on the
 result of @method[font% get-size-in-pixels].

Due to space included in a font by a font designer, a font tends to
 generate text that is slightly taller than the nominal size.

}

@defmethod[(get-size-in-pixels)
           boolean?]{

Returns @racket[#t] if the size reported by @method[font%
 get-point-size] is in logical drawing units, @racket[#f] if it is in
 points.

For a size in points and a screen or bitmap drawing context, the
 logical height depends on the resolution of the screen.

}

@defmethod[(get-smoothing) font-smoothing/c]{

Gets the font's anti-alias smoothing mode. See @racket[font%] for
 information about smoothing.

}

@defmethod[(get-style) font-style/c]{

Gets the font's slant style. See @racket[font%] for information about
 styles.

}

@defmethod[(get-underlined)
           boolean?]{

Returns @racket[#t] if the font is underlined or @racket[#f]
otherwise.

}

@defmethod[(get-weight) font-weight/c]{

Gets the font's weight. See @racket[font%] for information about
 weights.

}

@defmethod[(screen-glyph-exists? [c char?]
                                 [for-label? any/c #f])
           boolean?]{

Returns @racket[#t] if the given character has a corresponding glyph
 when drawing to the screen or a bitmap, @racket[#f] otherwise.

If the second argument is true, the result indicates whether the glyph
 is available for control labels. Otherwise, it indicates whether the
 glyph is available for @racket[dc<%>] drawing.

For @racket[dc<%>] drawing, due to automatic font substitution when
 drawing or measuring text, the result of this method does not depend
 on this font's attributes (size, face, etc.). The font's attributes
 merely provide a hint for the glyph search.

See also @method[dc<%> glyph-exists?] .

}}
