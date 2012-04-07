#lang scribble/doc
@(require "common.rkt" scribble/bnf)

@definterface/title[font-name-directory<%> ()]{

There is one @racket[font-name-directory<%>] object:
 @racket[the-font-name-directory]. It implements a mapping from font
 specifications (face, family, style, and weight) to information for
 rendering text on a specific device. Programmers rarely need to
 directly invoke methods of @racket[the-font-name-directory]. It is
 used automatically when drawing text to a @racket[dc<%>]
 object. Nevertheless, @racket[the-font-name-directory] is available
 so that programmers can query or modify the mapping manually. A
 programmer may also need to understand how the face-and-family
 mapping works.

To extract mapping information from @racket[the-font-name-directory],
 first obtain a @defterm{font ID}, which is an index based on a family
 and optional face string. Font IDs are returned by
 @method[font-name-directory<%> find-or-create-font-id] and
 @method[font-name-directory<%> get-font-id] . A Font ID can be
 combined with a weight and style to obtain a specific mapping value
 via @method[font-name-directory<%> get-screen-name] or
 @method[font-name-directory<%> get-post-script-name].

For a family without a face string, the corresponding font ID has a
 useful built-in mapping for every platform and device. For a family with a
 face string, @racket[the-font-name-directory] interprets the string
 (in a platform-specific way) to generate a mapping for
 drawing (to a canvas's @racket[dc<%>], a @racket[bitmap-dc%], or a
 @racket[printer-dc%]).

Currently, on all platforms, a face string is interpreted as a
 @hyperlink["http://www.pango.org"]{Pango} font description when it
 contains a comma, otherwise it is treated as a family name. A
 face can thus be just a family name such as @racket["Helvetica"], a family
 followed by a comma and font modifiers as in @racket["Helvetica, Bold"], or a sequence of comma-separated
 familie names followed by space-separated font options as an
 @racket["Helvetica, Arial, bold italic"]. Any size in a font
 description is overridden by a given @racket[font%]'s size. Any
 (slant) style or weight options in a font description are overridden
 by a non-@racket['normal] value for a given @racket[font%]'s style or
 weight, respectively.

@defmethod[(find-family-default-font-id [family (or/c 'default 'decorative 'roman 'script 
                                                      'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the font ID representing the default font for a family. See
@racket[font%] for information about font families.

}

@defmethod[(find-or-create-font-id [name string?]
                                   [family (or/c 'default 'decorative 'roman 'script 
                                                 'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the face name for a font ID, initializing the mapping for
 the face name if necessary.

Font ID are useful only as mapping indices for
 @indexed-racket[the-font-name-directory].

}

@defmethod[(get-face-name [font-id exact-integer?])
           (or/c string? #f)]{

Gets the face name for a font ID. If the font ID corresponds to
 the default font for a particular family, @racket[#f] is returned.

}

@defmethod[(get-family [font-id exact-integer?])
           (or/c 'default 'decorative 'roman 'script 
                 'swiss 'modern 'symbol 'system)]{

Gets the family for a font ID. See
@racket[font%] for information about font families.

}

@defmethod[(get-font-id [name string?]
                        [family (or/c 'default 'decorative 'roman 'script 
                                      'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the font ID for a face name paired with a default family. If the
 mapping for the given pair is not already initialized, @racket[0] is
 returned. See also @method[font-name-directory<%>
 find-or-create-font-id].

Font ID are useful only as mapping indices for
 @indexed-racket[the-font-name-directory].

}

@defmethod[(get-post-script-name [font-id exact-integer?]
                                 [weight (or/c 'normal 'bold 'light)]
                                 [style (or/c 'normal 'italic 'slant)])
           (or/c string? #f)]{

Gets a PostScript font description for a font ID, weight, and style
 combination.

See @racket[font%] for information about @racket[weight] and
 @racket[style].

}

@defmethod[(get-screen-name [font-id exact-integer?]
                            [weight (or/c 'normal 'bold 'light)]
                            [style (or/c 'normal 'italic 'slant)])
           (or/c string? #f)]{

Gets a platform-dependent screen font description (used for drawing to a
 canvas's @racket[dc<%>], a @racket[bitmap-dc%], or a
 @racket[printer-dc%]) for a font ID, weight, and style combination.

See @racket[font%] for information about @racket[weight] and
@racket[style].

}

@defmethod[(set-post-script-name [font-id exact-integer?]
                                 [weight (or/c 'normal 'bold 'light)]
                                 [style (or/c 'normal 'italic 'slant)]
                                 [name string?])
           void?]{

Sets a PostScript font description for a font ID, weight, and style
 combination. See also @method[font-name-directory<%>
 get-post-script-name].

See @racket[font%] for information about @racket[weight] and @racket[style].

}

@defmethod[(set-screen-name [font-id exact-integer?]
                            [weight (or/c 'normal 'bold 'light)]
                            [style (or/c 'normal 'italic 'slant)]
                            [name string?])
           void?]{

Sets a platform-dependent screen font description (used for drawing to a
 canvas's @racket[dc<%>], a @racket[bitmap-dc%], or a
 @racket[printer-dc%]) for a font ID, weight, and style combination.

See @racket[font%] for information about @racket[weight] and
 @racket[style].

}}

