#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@defclass/title[editor-data% object% ()]{

An @scheme[editor-data%] object contains extra data associated to a
snip or region in an editor. See also @|editordatadiscuss|.



@defconstructor/make[()]{

The element returned by @method[editor-data% get-next] is initialized
to @scheme[#f].

}

@defmethod[(get-dataclass)
           (or/c (is-a?/c editor-data-class%) false/c)]{
Gets the class for this data.
}

@defmethod[(get-next)
           (or/c (is-a?/c editor-data%) false/c)]{
Gets the next editor data element in a list of editor data elements.
A @scheme[#f] terminates the list.
}

@defmethod[(set-dataclass [v (is-a?/c editor-data-class%)])
           void?]{Sets the class for this data.
}

@defmethod[(set-next [v (or/c (is-a?/c editor-data%) false/c)])
           void?]{Sets the next editor data element in a list of editor data elements.
A @scheme[#f] terminates the list.
}

@defmethod[(write [f (is-a?/c editor-stream-out%)])
           boolean?]{
@methspec{

Writes the data to the specified stream, returning @scheme[#t] if data
is written successfully or @scheme[#f] otherwise.

}
@methimpl{

Returns @scheme[#f].

}}}

