#lang scribble/doc
@require["common.ss"]

@title{Fonts}


@defproc[(get-face-list [family (one-of/c 'mono 'all) 'all])
         (listof string?)]{

Returns a list of font face names available on the current system. If
 @scheme['mono] is provided as the argument, then only faces that are
 known to correspond to monospace fonts are included in the list.

}

@defproc[(get-family-builtin-face [family (one-of/c 'default 'decorative 'roman 'script 
                                                    'swiss 'modern 'symbol 'system)])
         string?]{

Returns the built-in default face mapping for a particular font
 family. The built-in default can be overridden via preferences, as
 described in @secref["fontresources"].

See  @scheme[font%] for information about @scheme[family].

}

@defthing[menu-control-font (is-a?/c font%)]{

This font is the default for @scheme[popup-menu%] objects.

Under Mac OS X, this font is slightly larger than
 @scheme[normal-control-font]. Under Windows and X, it is the same
 size as @scheme[normal-control-font].

}

@defthing[normal-control-font (is-a?/c font%)]{

This font is the default for most controls, except
 @scheme[list-box%] and @scheme[group-box-panel%] objects.


}

@defthing[small-control-font (is-a?/c font%)]{

This font is the default for @scheme[group-box-panel%] objects, and it is
 a suitable for controls in a floating window and other contexts that
 need smaller controls.

Under Windows, this font is the same size as
 @scheme[normal-control-font], since the Windows control font is
 already relatively small. Under X and Mac OS X, this font is slightly
 smaller than @scheme[normal-control-font].


}

@defthing[tiny-control-font (is-a?/c font%)]{

This font is for tiny controls, and it is smaller than
 @scheme[small-control-font] on all platforms.

}

@defthing[view-control-font (is-a?/c font%)]{

This font is the default for @scheme[list-box%] objects (but not
 list box labels, which use @scheme[normal-control-font]).

Under Mac OS X, this font is slightly smaller than
 @scheme[normal-control-font], and slightly larger than
 @scheme[small-control-font]. Under Windows and X, it is the same size
 as @scheme[normal-control-font].

}
