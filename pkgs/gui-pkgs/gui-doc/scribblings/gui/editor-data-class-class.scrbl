#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-data-class% object% ()]{

An @racket[editor-data-class%] object defines a type for
@racket[editor-data%] objects. See also @|editordatadiscuss|.


@defconstructor[()]{

Creates a (useless) instance.

}

@defmethod[(get-classname)
           string?]{

Gets the name of the class. Names starting with @litchar{wx} are reserved for
internal use.

}

@defmethod[(read [f (is-a?/c editor-stream-in%)])
           (or/c (is-a?/c editor-data%) #f)]{

Reads a new data object from the given stream, returning @racket[#f] if
 there is an error.

}

@defmethod[(set-classname [v string?])
           void?]{

Sets the name of the class. Names starting with @litchar{wx} are
 reserved for internal use.

An editor data class name should usually have the form @racket["(lib
 ...)"]  to enable on-demand loading of the class; see
 @|editordatadiscuss| for details.

}}

