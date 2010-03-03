#lang scribble/doc
@(require "common.ss"
          scribble/bnf)

@definterface/title[font-name-directory<%> ()]{

There is one @scheme[font-name-directory<%>] object:
 @scheme[the-font-name-directory]. It implements the mapping from font
 specifications (face, family, style, and weight) to information for
 rendering text on a specific device. The mapping is different for
 each platform. For example, when drawing to a bitmap in Windows, the
 rendering information is simply the name of a Windows font. When
 drawing to a PostScript file, the rendering information is a
 PostScript font name, which encapsulates the style and weight. When
 drawing to a bitmap in X, the rendering information is an X font
 string, which encapsulates the style and weight, parameterized over
 the size (using a ``%d'' placeholder).

Programmers rarely need to directly invoke methods of
 @scheme[the-font-name-directory]. It is used automatically when
 drawing text to a @scheme[dc<%>] object. Nevertheless,
 @scheme[the-font-name-directory] is available so that programmers can
 query or modify the mapping manually. A programmer may also need to
 understand how the face-and-family mapping works.

To extract mapping information from @scheme[the-font-name-directory],
 first obtain a @defterm{font ID}, which is an index based on a family
 and optional face string. Font IDs are returned by
 @method[font-name-directory<%> find-or-create-font-id] and
 @method[font-name-directory<%> get-font-id] . A Font ID can be
 combined with a weight and style to obtain a specific mapping value
 via @method[font-name-directory<%> get-screen-name] or
 @method[font-name-directory<%> get-post-script-name].

For a family without a face string, the corresponding font ID has a
 useful built-in mapping for every platform and device. (The built-in
 mapping can be overridden through the user's preferences; see
 @secref["fontresources"] for information.) For a family with a
 face string, @scheme[the-font-name-directory] interprets the string
 (in a platform-specific way) to generate a mapping for ``screen''
 drawing (to a canvas's @scheme[dc<%>], a @scheme[bitmap-dc%], or a
 @scheme[printer-dc%]). When drawing to a @scheme[post-script-dc%]
 object, the face-specific mapping defaults to the family's mapping.

Under Windows and Mac OS X, a face name is interpreted simply as a
 system font name for drawing to the screen, bitmap, or printer. The
 mapping succeeds if the system provides a font with the given name,
 and fails otherwise. For example, under Windows, @scheme["MS Sans
 Serif"] maps to the font that is typically used for button
 labels. Under X, a face name has a more complex interpretation:

@itemize[

 @item{If the string begins with a space, then the remainder of the
       string is interpreted as a fontconfig/Xft font name, but only
       if fontconfig/Xft support is enabled at compile time (which is
       the default when available), and only if the RENDER extension
       is available at run time. Multiple fontconfig/Xft font names
       can appear after the initial space, separated by commas; the
       first available font is used to draw text, and later fonts are
       substituted for missing characters in earlier fonts.}

 @item{If the string begins with @litchar{+}, then the remainder of the
       string is interpreted as an X font name. These names are
       usually long, such as
       @litchar{+-b&h-lucidatypewriter-medium-r-normal-sans-24-240-75-75-m-140-iso8859-1}.
       As usual for X font names, asterisks may appear in the string
       as wildcards. Furthermore, the size of the font can be
       parameterized by using @litchar{%d} in the place of a specific
       point size; if an asterisk appears in place of the pixel size,
       the asterisk and @litchar{%d} are swapped when the font size is
       specified in pixels (otherwise the size is always interpreted
       as points). For rotated text, @litchar{%d} will be replaced by
       a transformation matrix.}

 @item{A string of the form
       @litchar{-}@nonterm{provider}@litchar{-}@nonterm{font} is
       equivalent to
       @litchar{+-}@nonterm{provider}@litchar{-}@nonterm{font}@litchar{-}@nonterm{weight}@litchar{-}@nonterm{style}@litchar{-normal-*-*-%d-*-*-*-*-*-*},
       where @nonterm{weight} is either @litchar{medium},
       @litchar{light}, or @litchar{bold} (depending on the requested
       weight mapping) and @nonterm{style} is either @litchar{r},
       @litchar{i}, or @litchar{i} (depending on the requested style
       mapping).}
       
 @item{A string of the form @litchar{-}@nonterm{font} is
       equivalent to @litchar{-*-}@nonterm{font}.}
       
 @item{A string of any other format is interpreted as an X font name,
       optionally parameterized with @litchar{%d}.}

]

The mapping for face names can be overridden (on all platforms)
 through the user's preferences, as described in
 @secref["fontresources"].



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
 combination. The PostScript font name is used both for the font name
 in PostScript output (sans character set) and as the @|AFM| file
 name; see also @secref["postscriptfonts"].

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

Under X, if the screen name contains @litchar{%d}, it is replaced by
 the size of the font (point size times 10) to obtain the full screen
 font name.

See @scheme[font%] for information about @scheme[weight] and
 @scheme[style].

}}

