#lang scribble/doc
@(require "common.rkt")

@definterface/title[editor-data-class-list<%> ()]{

Each eventspace has an instance of @racket[editor-data-class-list<%>],
 obtained with @racket[(get-the-editor-data-class-list)]. New
 instances cannot be created directly. This list keeps a list of
 editor data classes; this list is needed for loading snips from a
 file. See also @|editordatadiscuss|.


@defmethod[(add [snipclass (is-a?/c editor-data-class%)])
           void?]{
Adds a snip data class to the list. If a class with the same name already
exists in the list, this one will not be added.

}

@defmethod[(find [name string?])
           (or/c (is-a?/c snip-class%) #f)]{
Finds a snip data class from the list with the given name, returning
 @racket[#f] if none can be found.

}

@defmethod[(find-position [class (is-a?/c editor-data-class%)])
           exact-nonnegative-integer?]{
Returns an index into the list for the specified class.

}

@defmethod[(nth [n exact-nonnegative-integer?])
           (or/c (is-a?/c editor-data-class%) #f)]{
Returns the @racket[n]th class in the list (counting from 0), returning
 @racket[#f] if the list has @racket[n] or less classes.

}

@defmethod[(number)
           exact-nonnegative-integer?]{

Returns the number of editor data classes in the list.

}}

