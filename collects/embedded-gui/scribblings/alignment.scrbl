#lang scribble/doc
@(require "common.rkt")

@definterface/title[alignment<%> (dllist<%>)]{

@defmethod[(get-parent) (is-a?/c alignment-parent<%>)]{

The parent of the alignment in the tree.}

@defmethod[(set-min-sizes) void?]{

Tells the alignment that its sizes should be calculated.}

@defmethod[(align [x-offset (and/c real? (not/c negative?))]
                  [y-offset (and/c real? (not/c negative?))]
                  [width (and/c real? (not/c negative?))]
                  [height  (and/c real? (not/c negative?))]) void?]{
Tells itself to align its children on the pasteboard
in the given rectangle defined by @racket[width], @racket[height] and a top
left corner point given as offsets into the pasteboards top
left corner.}

@defmethod[(get-min-width) (and/c real? (not/c negative?))]{

The minimum width this alignment must be.}

@defmethod[(get-min-height) (and/c real? (not/c negative?))]{

The minimum height this alignment must be.}

@defmethod*[([(stretchable-width) boolean?]
             [(stretchable-width [value boolean?]) void?])]{

Gets/sets the property of stretchability in the x dimension.}

@defmethod*[([(stretchable-height) boolean?]
             [(stretchable-height [value boolean?]) void?])]{

Gets/sets the property of stretchability in the y dimension.}

@defmethod[(show/hide [show? boolean?]) void?]{

Tells the alignment to show or hide its children.}

@defmethod[(show [show? boolean?]) void?]{

Tells the alignment that its show state is the given value
and it should show or hide its children accordingly.}

}

