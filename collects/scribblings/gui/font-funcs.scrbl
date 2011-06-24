#lang scribble/doc
@(require "common.rkt")

@title{Fonts}

@defthing[menu-control-font (is-a?/c font%)]{

This font is the default for @racket[popup-menu%] objects.

On Mac OS X, this font is slightly larger than
 @racket[normal-control-font]. On Windows and Unix, it is the same
 size as @racket[normal-control-font].

}

@defthing[normal-control-font (is-a?/c font%)]{

This font is the default for most controls, except
 @racket[list-box%] and @racket[group-box-panel%] objects.

}

@defthing[small-control-font (is-a?/c font%)]{

This font is the default for @racket[group-box-panel%] objects, and it is
 a suitable for controls in a floating window and other contexts that
 need smaller controls.

On Windows, this font is the same size as
 @racket[normal-control-font], since the Windows control font is
 already relatively small. On Unix and Mac OS X, this font is slightly
 smaller than @racket[normal-control-font].


}

@defthing[tiny-control-font (is-a?/c font%)]{

This font is for tiny controls, and it is smaller than
 @racket[small-control-font] on all platforms.

}

@defthing[view-control-font (is-a?/c font%)]{

This font is the default for @racket[list-box%] objects (but not
 list box labels, which use @racket[normal-control-font]).

On Mac OS X, this font is slightly smaller than
 @racket[normal-control-font], and slightly larger than
 @racket[small-control-font]. On Windows and Unix, it is the same size
 as @racket[normal-control-font].

}
