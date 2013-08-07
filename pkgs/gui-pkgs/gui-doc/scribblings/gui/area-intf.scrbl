#lang scribble/doc
@(require "common.rkt")

@definterface/title[area<%> ()]{

An @racket[area<%>] object is either a window or a windowless
 container for managing the position and size of other areas. An
 @racket[area<%>] can be a container, a containee, or both. The only
 areas without a parent are top-level windows.

All @racket[area<%>] classes accept the following named instantiation
 arguments:
@itemize[

 @item{@indexed-racket[min-width] --- default is the initial graphical minimum width; passed to
@method[area<%> min-width]} 
 @item{@indexed-racket[min-height] --- default is the initial graphical minimum height; passed to
@method[area<%> min-height]} 
 @item{@indexed-racket[stretchable-width] --- default is class-specific; passed to
@method[area<%> stretchable-width]} 
 @item{@indexed-racket[stretchable-height] --- default is class-specific; passed to
@method[area<%> stretchable-height]} 
]



@defmethod[(get-graphical-min-size)
           (values dimension-integer?
                   dimension-integer?)]{

Returns the area's graphical minimum size as two values: the minimum
 width and the minimum height (in pixels).

See @|geomdiscuss| for more information. Note that the return value
 @italic{does not} depend on the area's
@method[area<%> min-width] and
@method[area<%> min-height] settings.

}

@defmethod[(get-parent)
           (or/c (is-a?/c area-container<%>) #f)]{

Returns the area's parent. A top-level window may have no parent (in
 which case @racket[#f] is returned), or it may have another top-level
 window as its parent.

}

@defmethod[(get-top-level-window)
           (or/c (is-a?/c frame%) (is-a?/c dialog%))]{

Returns the area's closest frame or dialog ancestor. For a frame or
 dialog area, the frame or dialog itself is returned.

}

@defmethod*[([(min-width)
              dimension-integer?]
             [(min-width [w dimension-integer?])
              void?])]{

Gets or sets the area's minimum width (in pixels) for geometry
 management.

The minimum width is ignored when it is smaller than the area's
 @tech{graphical minimum width}, or when it is smaller
 than the width reported by
@method[area-container<%> container-size] if the area is a container. See @|geomdiscuss| for more information.

An area's initial minimum width is its graphical minimum width. See
 also
@method[area<%> get-graphical-min-size] .

When setting the minimum width, if @racket[w] is smaller than the
 internal hard minimum, @|MismatchExn|.

}

@defmethod*[([(min-height)
              dimension-integer?]
             [(min-height [h dimension-integer?])
              void?])]{

Gets or sets the area's minimum height for geometry management.

The minimum height is ignored when it is smaller than the area's
 @tech{graphical minimum height}, or when it is smaller
 than the height reported by
@method[area-container<%> container-size] if the area is a container. See @|geomdiscuss| for more information.

An area's initial minimum height is its graphical minimum height. See
 also
@method[area<%> get-graphical-min-size] .

When setting the minimum height (in pixels); if @racket[h] is smaller
 than the internal hard minimum, @|MismatchExn|.

}

@defmethod*[([(stretchable-height)
              boolean?]
             [(stretchable-height [stretch? any/c])
              void?])]{

Gets or sets the area's vertical stretchability for geometry
 management. See @|geomdiscuss| for more information.

}

@defmethod*[([(stretchable-width)
              boolean?]
             [(stretchable-width [stretch? any/c])
              void?])]{

Gets or sets the area's horizontal stretchability for geometry
 management. See @|geomdiscuss| for more information.

}}
