#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-data% object% ()]{

An @racket[editor-data%] object contains extra data associated to a
snip or region in an editor. See also @|editordatadiscuss|.



@defconstructor[()]{

The element returned by @method[editor-data% get-next] is initialized
to @racket[#f].

}

@defmethod[(get-dataclass)
           (or/c (is-a?/c editor-data-class%) #f)]{
Gets the class for this data.
}

@defmethod[(get-next)
           (or/c (is-a?/c editor-data%) #f)]{
Gets the next editor data element in a list of editor data elements.
A @racket[#f] terminates the list.
}

@defmethod[(set-dataclass [v (is-a?/c editor-data-class%)])
           void?]{Sets the class for this data.
}

@defmethod[(set-next [v (or/c (is-a?/c editor-data%) #f)])
           void?]{Sets the next editor data element in a list of editor data elements.
A @racket[#f] terminates the list.
}

@defmethod[(write [f (is-a?/c editor-stream-out%)])
           boolean?]{
@methspec{

Writes the data to the specified stream, returning @racket[#t] if data
is written successfully or @racket[#f] otherwise.

}
@methimpl{

Returns @racket[#f].

}}}

