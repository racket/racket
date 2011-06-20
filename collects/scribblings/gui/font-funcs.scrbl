#lang scribble/doc
@(require "common.rkt")

@title{Fonts}

@defthing[menu-control-font (is-a?/c font%)]{

This font is the default for @scheme[popup-menu%] objects.

On Mac OS X, this font is slightly larger than
 @scheme[normal-control-font]. On Windows and Unix, it is the same
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

On Windows, this font is the same size as
 @scheme[normal-control-font], since the Windows control font is
 already relatively small. On Unix and Mac OS X, this font is slightly
 smaller than @scheme[normal-control-font].


}

@defthing[tiny-control-font (is-a?/c font%)]{

This font is for tiny controls, and it is smaller than
 @scheme[small-control-font] on all platforms.

}

@defthing[view-control-font (is-a?/c font%)]{

This font is the default for @scheme[list-box%] objects (but not
 list box labels, which use @scheme[normal-control-font]).

On Mac OS X, this font is slightly smaller than
 @scheme[normal-control-font], and slightly larger than
 @scheme[small-control-font]. On Windows and Unix, it is the same size
 as @scheme[normal-control-font].

}
