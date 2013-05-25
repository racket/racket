#lang scribble/doc
@(require "common.rkt")

@defclass/title[brush-list% object% ()]{

A @racket[brush-list%] object maintains a list of @racket[brush%]
 objects to avoid creating brushes repeatedly. A @racket[brush%]
 object in a brush list cannot be mutated.

A global brush list, @racket[the-brush-list], is created
 automatically.


@defconstructor[()]{

Creates an empty brush list.

}

@defmethod*[([(find-or-create-brush [color (is-a?/c color%)]
                                    [style (or/c 'transparent 'solid 'opaque
                                                 'xor 'hilite 'panel 
                                                 'bdiagonal-hatch 'crossdiag-hatch 
                                                 'fdiagonal-hatch 'cross-hatch 
                                                 'horizontal-hatch 'vertical-hatch)])
              (is-a?/c brush%)]
             [(find-or-create-brush [color-name string?]
                                    [style (or/c 'transparent 'solid 'opaque 
                                                 'xor 'hilite 'panel 
                                                 'bdiagonal-hatch 'crossdiag-hatch 
                                                 'fdiagonal-hatch 'cross-hatch 
                                                 'horizontal-hatch 'vertical-hatch)])
              (or/c (is-a?/c brush%) #f)])]{

Finds a brush of the given specification, or creates one and adds it
 to the list. See @racket[brush%] for a further explanation of the
 arguments, which are the same as @racket[brush%]'s initialization
 arguments.

}}
