#lang scribble/doc
@(require "common.ss"
          (for-label scheme/file))

@title[#:tag "mredprefs"]{Preferences}

GRacket supports a number of preferences for global configuration. The
 GRacket preferences are stored in the common file reported by
 @scheme[find-system-path] for @indexed-scheme['pref-file], and
 preference values can be retrieved and changed through
 @scheme[get-preference] and @scheme[put-preferences]. However, GRacket
 reads most preferences once at startup (all except the
 @Resource{playcmd}).

The following are the (case-sensitive) preference names used by GRacket:

@itemize[

 @item{@ResourceFirst{default-font-size} --- sets the default font size
 the basic style in a style list, and thus the default font size for
 an editor.}

 @item{@ResourceFirst{defaultMenuPrefix} --- sets the prefix used by
 default for menu item shortcuts under X, one of @scheme['ctl],
 @scheme['meta], or @scheme['alt]. The default is
 @scheme['ctl]. When this preference is set to @scheme['meta] or
 @scheme['alt], underlined mnemonics (introduced by @litchar{&} in menu
 labels) are suppressed.}

 @item{@ResourceFirst{emacsUndo} --- a true value makes undo in
 editors work as in Emacs (i.e., undo operations are themselves kept
 in the undo stack).}

 @item{@ResourceFirst{wheelStep} --- sets the default mouse-wheel step
 size of @scheme[editor-canvas%] objects.}

 @item{@ResourceFirst{outlineInactiveSelection} --- a true value
 causes selections in text editors to be shown with an outline of the
 selected region when the editor does no have the keyboard focus.}

 @item{@ResourceFirst{playcmd} --- used to format a sound-playing
 command; see @scheme[play-sound] for details.}

 @item{@ResourceFirst{doubleClickTime} --- overrides the
 platform-specific default interval (in milliseconds) for double-click
 events.}

]
