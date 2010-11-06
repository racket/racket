#lang scribble/doc
@(require "common.ss"
          scribble/bnf)

@definterface/title[font-name-directory<%> ()]{

There is one @scheme[font-name-directory<%>] object:
 @scheme[the-font-name-directory]. It implements a mapping from font
 specifications (face, family, style, and weight) to information for
 rendering text on a specific device. Programmers rarely need to
 directly invoke methods of @scheme[the-font-name-directory]. It is
 used automatically when drawing text to a @scheme[dc<%>]
 object. Nevertheless, @scheme[the-font-name-directory] is available
 so that programmers can query or modify the mapping manually. A
 programmer may also need to understand how the face-and-family
 mapping works.

To extract mapping information from @scheme[the-font-name-directory],
 first obtain a @defterm{font ID}, which is an index based on a family
 and optional face string. Font IDs are returned by
 @method[font-name-directory<%> find-or-create-font-id] and
 @method[font-name-directory<%> get-font-id] . A Font ID can be
 combined with a weight and style to obtain a specific mapping value
 via @method[font-name-directory<%> get-screen-name] or
 @method[font-name-directory<%> get-post-script-name].

For a family without a face string, the corresponding font ID has a
 useful built-in mapping for every platform and device. For a family with a
 face string, @scheme[the-font-name-directory] interprets the string
 (in a platform-specific way) to generate a mapping for ``screen''
 drawing (to a canvas's @scheme[dc<%>], a @scheme[bitmap-dc%], or a
 @scheme[printer-dc%]). When drawing to a @scheme[post-script-dc%]
 object, the face-specific mapping defaults to the family's mapping.


@defmethod[(find-family-default-font-id [family (one-of/c 'default 'decorative 'roman 'script 
                                                          'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the font ID representing the default font for a family. See
@scheme[font%] for information about font families.

}

@defmethod[(find-or-create-font-id [name string?]
                                   [family (one-of/c 'default 'decorative 'roman 'script 
                                                     'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the face name for a font ID, initializing the mapping for
 the face name if necessary.

Font ID are useful only as mapping indices for
 @indexed-scheme[the-font-name-directory].

}

@defmethod[(get-face-name [font-id exact-integer?])
           (or/c string? false/c)]{

Gets the face name for a font ID. If the font ID corresponds to
 the default font for a particular family, @scheme[#f] is returned.

}

@defmethod[(get-family [font-id exact-integer?])
           (one-of/c 'default 'decorative 'roman 'script 
                     'swiss 'modern 'symbol 'system)]{

Gets the family for a font ID. See
@scheme[font%] for information about font families.

}

@defmethod[(get-font-id [name string?]
                        [family (one-of/c 'default 'decorative 'roman 'script 
                                          'swiss 'modern 'symbol 'system)])
           exact-integer?]{

Gets the font ID for a face name paired with a default family. If the
 mapping for the given pair is not already initialized, @scheme[0] is
 returned. See also @method[font-name-directory<%>
 find-or-create-font-id].

Font ID are useful only as mapping indices for
 @indexed-scheme[the-font-name-directory].

}

@defmethod[(get-post-script-name [font-id exact-integer?]
                                 [weight (one-of/c 'normal 'bold 'light)]
                                 [style (one-of/c 'normal 'italic 'slant)])
           (or/c string? false/c)]{

Gets a PostScript font name for a font ID, weight, and style
 combination.

See @scheme[font%] for information about @scheme[weight] and
 @scheme[style].

}

@defmethod[(get-screen-name [font-id exact-integer?]
                            [weight (one-of/c 'normal 'bold 'light)]
                            [style (one-of/c 'normal 'italic 'slant)])
           (or/c string? false/c)]{

Gets a platform-dependent screen font name (used for drawing to a
 canvas's @scheme[dc<%>], a @scheme[bitmap-dc%], or a
 @scheme[printer-dc%]) for a font ID, weight, and style combination.

See @scheme[font%] for information about @scheme[weight] and
@scheme[style].

}

@defmethod[(set-post-script-name [font-id exact-integer?]
                                 [weight (one-of/c 'normal 'bold 'light)]
                                 [style (one-of/c 'normal 'italic 'slant)]
                                 [name string?])
           void?]{

Sets a PostScript font name for a font ID, weight, and style
 combination. See also @method[font-name-directory<%>
 get-post-script-name].

See @scheme[font%] for information about @scheme[weight] and @scheme[style].

}

@defmethod[(set-screen-name [font-id exact-integer?]
                            [weight (one-of/c 'normal 'bold 'light)]
                            [style (one-of/c 'normal 'italic 'slant)]
                            [name string?])
           void?]{

Sets a platform-dependent screen font name (used for drawing to a
 canvas's @scheme[dc<%>], a @scheme[bitmap-dc%], or a
 @scheme[printer-dc%]) for a font ID, weight, and style combination.

See @scheme[font%] for information about @scheme[weight] and
 @scheme[style].

}}

