#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@title[#:tag "mr:mredprefs"]{Preferences}

MrEd supports a number of preferences for global configuration. The
 MrEd preferences are stored in the common file reported by
 @scheme[find-system-path] for @indexed-scheme['pref-file], and
 preference values can be retrieved and changed through
 @scheme[get-preference] and @scheme[set-preference]. However, MrEd
 reads most preferences once at startup (all except the
 @Resource{playcmd}).

The following are the (case-sensitive) preference names used by MrEd:

@itemize{

 @item{@ResourceFirst{default-font-size} --- sets the default font size
 the basic style in a style list, and thus the default font size for
 an editor.}

 @item{@ResourceFirst{controlFontSize} --- sets the font size for
 control and menu labels (Windows, X); the font is the @scheme['system]
 font, which can be configured as described in
 @secref["mr:fontresources"].}

 @item{@ResourceFirst{defaultMenuPrefix} --- sets the prefix used by
 default for menu item shortcuts under X, one of @scheme['ctl],
 @scheme['meta], or @scheme['alt]. The default is
 @scheme['ctl]. When this preference is set to @scheme['meta] or
 @scheme['alt], underlined mnemonics (introduced by @litchar{&} in menu
 labels) are suppressed.}

 @item{@ResourceFirst{altUpSelectsMenu} --- a true value makes
 pressing and releasing the Alt key select the first menu in the menu
 bar under X.}

 @item{@ResourceFirst{emacsUndo} --- a true value makes undo in
 editors work as in Emacs (i.e., undo operations are themselves kept
 in the undo stack).}

 @item{@ResourceFirst{hiliteColor} --- a string to sets the color for
 highlighting text, menus, and other GUI elements under X; the
 preference string should contain six hexadecimal digits, two for each
 component of the color. For example, set @Resource{hiliteColor} to
 @scheme["0000A0"] and set @Resource{hiliteMenuBorder} to @scheme[#t]
 for a Bluecurve-like look.}

 @item{@ResourceFirst{hiliteMenuBorder} --- a true value causes a menu
 selection to be highlighted with a border (in addition to a color) under
 X.}

 @item{@ResourceFirst{wheelStep} --- sets the default mouse-wheel step
 size of @scheme[editor-canvas%] objects.}

 @item{@ResourceFirst{outlineInactiveSelection} --- a true value
 causes selections in text editors to be shown with an outline of the
 selected region when the editor does no have the keyboard focus.}

 @item{@ResourceFirst{playcmd} --- used to format a sound-playing
 command; see @scheme[play-sound] for details.}

 @item{@ResourceFirst{forceFocus} --- a true value enables extra
 effort in MrEd to move the focus to a top-level window that is shown
 or raised.}

 @item{@ResourceFirst{doubleClickTime} --- overrides the
 platform-specific default interval (in milliseconds) for double-click
 events.}

 @item{@ResourceFirst{gamma} --- sets the gamma value used in
 gamma-correcting PNG files.}

 @item{@ResourceFirst{selectionAsClipboard} --- under X, a true value
 causes @scheme[the-clipboard] to be an alias to
 @scheme[the-x-selection-clipboard], which means that cut and paste
 operations use the X selection instead of the X clipboard. See also
 @scheme[clipboard<%>].}


}

In addition, preference names built from font face names can provide
 or override default entries for the @scheme[font-name-directory<%>];
 see @secref["mr:fontresources"] for information.
