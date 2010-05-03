#lang scribble/doc
@(require "common.ss"
          scribble/bnf)

@title[#:tag "fontresources"]{Font Configuration}

This chapter describes how to set up face mappings for screen and
 PostScript fonts via preferences (see @|mrprefsdiscuss|). The
 font-configuration system is overkill; it was designed to handle
 especially complex X font mappings before fontconfig/Xft solved the
 problem.

An implementor for a GRacket-based program may find it easier to use the
 @method[font-name-directory<%> set-screen-name] and
 @method[font-name-directory<%> set-post-script-name] methods
 provided by @scheme[the-font-name-directory]. As a user of a
 GRacket-based program, preferences provide a mechanism for setting
 default mappings.

Whether a programmer or a user, see @scheme[font-name-directory<%>] for
 an overview of the font mapping system.

To find a font name for a family, GRacket looks for a preference name by
 concatenating @litchar{MrEd:}, a @nonterm{dest}, a @nonterm{type},
 a @nonterm{weight}, and a @nonterm{style}, where

@itemize[

  @item{@nonterm{dest} is either @litchar{Screen} or @litchar{PostScript}.}

  @item{@nonterm{type} is either @litchar{Default}, @litchar{Decorative}, @litchar{Roman}, @litchar{Script},
         @litchar{Swiss}, @litchar{Modern}, @litchar{System}, or @litchar{Symbol} for a mapping
         defining the default font for a family. Otherwise, it is a
         face name prefixed with @litchar["@"].}

  @item{@nonterm{weight} is either @litchar{Medium}, @litchar{Bold}, or @litchar{Light}.}

  @item{@nonterm{style} is either @litchar{Straight}, @litchar{Italic}, or @litchar{Slant}.}

]

Furthermore, any of the latter three parts can be wildcarded with
 @litchar{_}, as described below. The concatenated string is converted
 to a symbol (preserving case), and the associated preference value
 must be a string.

The value of the preference is parsed as described in
 @scheme[font-name-directory<%>] for parsing face names, except that
 the string can contain references and other tricks described below.

@; ------------------------------------------------------------------------

@section[#:tag "exampleresources"]{Wildcards}

Building items names by concatenating @nonterm{dest}, @nonterm{type},
 @nonterm{weight}, and @nonterm{style} can create a large number of preference
 entries, and the @nonterm{weight} and @nonterm{style} parts are useful only
 for X screen fonts. To avoid an explosion of preferences, GRacket finds
 preferences via a wildcarding search.

The @nonterm{type}, @nonterm{weight}, and @nonterm{style} parts of a preference name
 can be wildcarded by using @litchar{_}. Thus, to set the default font
 in X for all types, weights, and styles, use the following preference
 entry:

@schemeblock[
(MrEd:Screen___ "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
]

Wildcarded preference entries are used only when un-wildcarded values
 cannot be found. If two preference names both match for some search,
 then the one with the ``earliest'' (i.e., closest to the beginning of
 the preference name) non-wildcarded part will prevail.

The default GRacket preferences for Windows uses wildcarding to specify
 the basic font mapping, as if written as:

@schemeblock[
(MrEd:ScreenSystem__ "MS Sans Serif")
(MrEd:ScreenRoman__ "Times New Roman")
(MrEd:ScreenDecorative__ "Modern")
....
]

Wildcarding in the preference name naturally leads to references,
 variables, and wildcarding references in the preference
 value. These features are described in the following few sections.

@; ------------------------------------------------------------------------

@section{References}

Suppose we define the mapping for variants of @scheme["Default"], and
 then we want @scheme["Roman"] to use this setting, too. We could copy
 the preference entry, as in the following example:

@schemeblock[
(|MrEd:ScreenDefault__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenRoman__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
]

but the GRacket font-reading system provides a better syntax for
 referencing another preference entry. When a preference value contains
 @litchar{${x}}, then the @litchar{${x}} fragment is replaced by the
 preference value of @litchar{x}. Thus, the above can be re-written:

@schemeblock[
(|MrEd:ScreenDefault__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenRoman__| "${ScreenDefault__}")
]

A mini-language of @litchar{${x}} is used within the string (instead
 of an S-expression format) for historical reasons.

@; ------------------------------------------------------------------------

@section{Variables}

Variables can be used with referencing to configure default values
 based on the weight and style that is needed.  When a preference
 value contains @litchar{$[weight]}, then @litchar{$[weight]} is
 replaced with a string for the desired font weight. Similarly,
 @litchar{$[style]} is replaced with the desired style. Variable
 expressions can be embedded within referencing expressions, as in the
 following example:

@schemeblock[
(|MrEd:ScreenDefault__|
 "+-*-*-${Def$[weight]}-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:DefMedium| "medium")
(|MrEd:DefBold| "bold")
(|MrEd:DefLight| "medium")
]

Now, when the @Resource{ScreenDefault__} value is used for different
 weights, it will return different values; the
 @litchar{${Def$[weight]}} expression will turn into
 @litchar{${DefMedium}} for a medium-weight lookup, or
 @litchar{${DefBold}} for a bold-weight lookup. These references
 will in turn give either @litchar{medium} or @litchar{bold}.

@; ------------------------------------------------------------------------

@section{Wildcarding References}

Consider the following preference configuration:

@schemeblock[
(|MrEd:ScreenDefault__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenDefaultBold_| "+-*-*-bold-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenRoman__| "${ScreenDefault__}")
]

The effect of this statement is probably not what was intended; when a
 bold version of the @litchar{Roman} font is needed, the
 @Resource{ScreenRoman__} value references the
 @Resource{ScreenDefault__} value, which does not specify a bold font.  We
 could try to remedy the situation as follows:

@schemeblock[
(|MrEd:ScreenDefault__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenDefaultBold_| "+-*-*-bold-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenRoman__| "${ScreenDefault$[weight]_}")
]

but this does not work either. It works fine for bold @litchar{Roman},
 now, but medium @litchar{Roman} will cause a reference to the
 @Resource{ScreenDefaultMedium_}, which doesn't exist. The problem is
 that our reference does not use wildcarding like the original medium
 @litchar{Roman} lookup did.

Wildcarding can be specified in a reference by separating each
 wildcardable field with a comma. The following preference specification
 does what we want:

@schemeblock[
(|MrEd:ScreenDefault__| "+-*-*-medium-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenDefaultBold_| "+-*-*-bold-r-normal-*-*-%d-*-*-*-*-*-*")
(|MrEd:ScreenRoman__| "${ScreenDefault,$[weight],_}")
]

Since @litchar{$[weight]} is between commas, it can be wildcarded if
 no name exactly matching @litchar{ScreenDefault$[weight]_} is
 found. In this case @litchar{ScreenDefault} and @litchar{_} can
 also be wildcarded, but this will have no effect.

The wildcarding used in references need not reflect the wildcarding
 GRacket initial uses for finding fonts. In other words, a number of
 comma-separated selects can appear between the curly braces.

@; ------------------------------------------------------------------------

@section{Internal Preferences}

The initial font setup is built into GRacket through a built-in preference
 table. The table is shown at the end of this section. When font
 information is computed, it is @italic{almost} as if this table were
 installed into your preferences file; the difference is that preference
 specifications in your file override specifications in the built-in
 table, even when the wildcarding of your preference provides a weaker
 match.

When no information is available for mapping a face name to a font,
 GRacket falls back to the system described in
 @scheme[font-name-directory<%>]. (Since a mapping is built into GRacket
 for every family, information is always available for the default
 font of a family.)

Internal preferences for all platforms:

@schemeblock[
(|MrEd:PostScriptMediumStraight| "")
(|MrEd:PostScriptMediumItalic| "-Oblique")
(|MrEd:PostScriptMediumSlant| "-Oblique")
(|MrEd:PostScriptLightStraight| "")
(|MrEd:PostScriptLightItalic| "-Oblique")
(|MrEd:PostScriptLightSlant| "-Oblique")
(|MrEd:PostScriptBoldStraight| "-Bold")
(|MrEd:PostScriptBoldItalic| "-BoldOblique")
(|MrEd:PostScriptBoldSlant| "-BoldOblique")

(|MrEd:PostScript___| "${PostScript$[family],$[weight],$[style]}")

(|MrEd:PostScriptSystem__| "${PostScriptTimes,$[weight],$[style]}")
(|MrEd:PostScriptRoman__| "${PostScriptTimes,$[weight],$[style]}")
(|MrEd:PostScriptDecorative__| "${PostScriptTimes,$[weight],$[style]}")
(|MrEd:PostScriptScript__| "ZapfChancery-MediumItalic")

(|MrEd:PostScriptTimesMedium| "")
(|MrEd:PostScriptTimesLight| "")
(|MrEd:PostScriptTimesBold| "Bold")

(|MrEd:PostScriptTimes__| "Times${PostScript$[weight]$[style]}")
(|MrEd:PostScriptTimesMediumStraight| "Times-Roman")
(|MrEd:PostScriptTimesLightStraight| "Times-Roman")
(|MrEd:PostScriptTimes_Slant| 
 "Times-${PostScriptTimes$[weight]}Italic")
(|MrEd:PostScriptTimes_Italic|
 "Times-${PostScriptTimes$[weight]}Italic")

(|MrEd:PostScriptDefault__| "Helvetica${PostScript$[weight]$[style]}")
(|MrEd:PostScriptSwiss__| "Helvetica${PostScript$[weight]$[style]}")
(|MrEd:PostScriptModern__| "Courier${PostScript$[weight]$[style]}")
(|MrEd:PostScriptSymbol__| "Symbol")
]

Internal preferences for X with fontconfig/Xft/RENDER only:

@schemeblock[
(|MrEd:ScreenSystem__| " Sans")
(|MrEd:ScreenDefault__| " Sans")
(|MrEd:ScreenRoman__| " Serif")
(|MrEd:ScreenDecorative__| " Nimbus Sans L")
(|MrEd:ScreenModern__| " Monospace")
(|MrEd:ScreenSwiss__| " Nimbus Sans L")
(|MrEd:ScreenScript__| " URW Chancery L")
(|MrEd:ScreenSymbolBase| " Standard Symbols L,Nimbus Sans L")
]

Internal preferences for X only (except those overridden for fontconfig/Xft/RENDER):

@schemeblock[
(|MrEd:ScreenMedium| "medium")
(|MrEd:ScreenBold| "bold")
(|MrEd:ScreenLight| "light")
(|MrEd:ScreenStraight| "r")
(|MrEd:ScreenItalic| "i")
(|MrEd:ScreenSlant| "o")

(|MrEd:ScreenSystemBase| "*-lucida")
(|MrEd:ScreenDefaultBase| "*-lucida")
(|MrEd:ScreenRomanBase| "*-times")
(|MrEd:ScreenDecorativeBase| "*-helvetica")
(|MrEd:ScreenModernBase| "*-courier")
(|MrEd:ScreenSwissBase| "*-lucida")
(|MrEd:ScreenScriptBase| "*-zapfchancery")
(|MrEd:ScreenSymbolBase| "*-symbol")

(|MrEd:ScreenStdSuffix|
 "-${Screen$[weight]}-${Screen$[style]}-normal-*-*-%d-*-*-*-*-*-*")

(|MrEd:ScreenSystem__| "+-${ScreenSystemBase}${ScreenStdSuffix}")
(|MrEd:ScreenDefault__| "+-${ScreenDefaultBase}${ScreenStdSuffix}")
(|MrEd:ScreenRoman__| "+-${ScreenRomanBase}${ScreenStdSuffix}")
(|MrEd:ScreenDecorative__|
 "+-${ScreenDecorativeBase}${ScreenStdSuffix}")
(|MrEd:ScreenModern__| "+-${ScreenModernBase}${ScreenStdSuffix}")
(|MrEd:ScreenSwiss__| "+-${ScreenSwissBase}${ScreenStdSuffix}")
(|MrEd:ScreenScript__| "+-${ScreenScriptBase}${ScreenStdSuffix}")
(|MrEd:ScreenSymbol__|
 "+-${ScreenSymbolBase}-medium-r-normal-*-*-%d-*-*-*-*-*-*")
]

Internal preferences for Windows only:

@schemeblock[
(|MrEd:ScreenSystem__| "MS Sans Serif")
(|MrEd:ScreenDefault__| "MS Sans Serif")
(|MrEd:ScreenRoman__| "Times New Roman")
(|MrEd:ScreenDecorative__| "Arial")
(|MrEd:ScreenModern__| "Courier New")
(|MrEd:ScreenSwiss__| "Arial")
(|MrEd:ScreenScript__| "Arial")
(|MrEd:ScreenSymbol__| "Symbol")
]

Internal preferences for Mac OS X only:

@schemeblock[
(|MrEd:ScreenDefault__| "Lucida Grande")
(|MrEd:ScreenSystem__| "Lucida Grande")
(|MrEd:ScreenRoman__| "Times")
(|MrEd:ScreenDecorative__| "Arial")
(|MrEd:ScreenModern__| "Courier New")
(|MrEd:ScreenSwiss__| "Helvetica")
(|MrEd:ScreenScript__| "Apple Chancery")
(|MrEd:ScreenSymbol__| "Symbol")
]

@; ------------------------------------------------------------------------

@section[#:tag "postscriptfonts"]{PostScript Fonts}

@section-index["fonts" "PostScript"]
@section-index["PostScript fonts"]
@section-index["AFM"]
@section-index["CID"]
@section-index["CMap"]

To generate PostScript output, GRacket must be able to find an @|AFM|
 (AFM) file corresponding to the PostScript font. An AFM file
 typically uses the suffix @indexed-file{.afm}, and several AFM files
 are distributed with GRacket in the @filepath{afm} collection.

GRacket finds an AFM file by adding a @filepath{.afm} suffix to the
 PostScript name of the font, and checking all directories specified
 by the @scheme[current-ps-afm-file-paths] parameter. The initial
 value of this parameter is determined by the
 @indexed-envvar{PLTAFMPATHS} environment variable; the environment
 variable's setting is parsed with
 @scheme[path-list-string->path-list] using @scheme[(list
 (collection-path "afm"))] as the default list.

Depending on whether the font is CID-based (typically for the Chinese,
 Japanese, Korean, and Vietnamese language families, and as indicated
 in the AFM file), GRacket must find additional files:

 @itemize[

 @item{@italic{Non-CID:} In addition to an AFM file
 @filepath{@nonterm{x}.afm}, GRacket looks for a
 @filepath{@nonterm{x}-glyphlist.txt} file (in the same directory as the
 AFM file) to map glyph names in the AFM file to Unicode character
 values. In addition to this font-specific file, GRacket looks for a
 @indexed-file{glyphlist.txt} file to supply a mapping for Adobe's
 standard glyph names, and this mapping is used when a font-specific
 mapping is not supplied, or when the mapping does not cover a name
 found in the AFM file. GRacket looks for @filepath{glyphlist.txt} in the
 same place as AFM files. Since @filepath{glyphlist.txt} is large, if a
 @indexed-file{glyphshortlist.txt} file is available, it is read first,
 and then @filepath{glyphlist.txt} is read only if a character name must
 be resolved that is not in @filepath{glyphshortlist.txt}.}

 @item{@italic{CID:} In addition to an AFM file, GRacket must find and
 read CMap files to convert glyph IDs for the font to Unicode
 characters. The character set name is used as the name of the CMap
 file to load, and GRacket checks all directories specified by the
 @scheme[current-ps-cmap-file-paths] parameter. The initial value of
 this parameter is determined by the @indexed-envvar{PLTCMAPPATHS}
 environment variable; the environment variable's setting is parsed
 with @scheme[path-list-string->path-list] using @scheme[(list
 (collection-path "afm" "CMap"))] as the default list. In addition to
 a CMap file for the font's character set, GRacket must find a
 @indexed-file{UniCNS-UTF32-H} CMap file to complete the mapping to
 Unicode. GRacket automatically adds the font's character set to the font
 name when producing PostScript with a CID-based font.}

 ]

When drawing or measuring text using a particular PostScript font, if
 the font does not contain a glyph for a character (or if a relevant
 AFM file cannot be found for the font), then GRacket attempts to
 substitute another PostScript font. A substitute font is selected by
 checking all @filepath{.afm} files in the directories specified
 by @scheme[current-ps-afm-file-paths] (in order), and choosing the
 first discovered match.
