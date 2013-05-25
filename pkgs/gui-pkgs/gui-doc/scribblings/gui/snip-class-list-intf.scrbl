#lang scribble/doc
@(require "common.rkt")

@definterface/title[snip-class-list<%> ()]{

Each eventspace has its own instance of @racket[snip-class-list<%>],
 obtained with @racket[(get-the-snip-class-list)]. New instances
 cannot be created directly. Each instance keeps a list of snip
 classes. This list is needed for loading snips from a file. See also
 @|snipclassdiscuss|.


@defmethod[(add [snipclass (is-a?/c snip-class%)])
           void?]{

Adds a snip class to the list. If a class with the same name already
 exists in the list, this one will not be added.

}

@defmethod[(find [name string?])
           (or/c (is-a?/c snip-class%) #f)]{

Finds a snip class from the list with the given name, returning
 @racket[#f] if none is found.

}

@defmethod[(find-position [class (is-a?/c snip-class%)])
           exact-nonnegative-integer?]{

Returns an index into the list for the specified class.

}

@defmethod[(nth [n exact-nonnegative-integer?])
           (or/c (is-a?/c snip-class%) #f)]{

Returns the @racket[n]th class in the list, or @racket[#f] if
 the list has @racket[n] classes or less.

}

@defmethod[(number)
           exact-nonnegative-integer?]{

Returns the number of snip classes in the list.

}}
