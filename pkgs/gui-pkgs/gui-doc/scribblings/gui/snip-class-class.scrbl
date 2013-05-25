#lang scribble/doc
@(require "common.rkt")

@defclass/title[snip-class% object% ()]{

Useful snip classes are defined by instantiating derived subclasses of
 @racket[snip-class%]. A class derived from @racket[snip-class%]
 serves as a kind of ``meta-class'' for snips; each snip is associated
 with an instance of @racket[snip-class%] as its snip class.

In deriving a new @racket[snip-class%] class, override the
 @method[snip-class% read] method. Then, for each instance of the
 derived class (where each instance corresponds to a single snip
 class):

@itemize[

 @item{Set the classname using @method[snip-class% set-classname].}

 @item{Set the version using 
       @method[snip-class% set-version].} 

 @item{Install the class into the list returned by
       @racket[get-the-snip-class-list] using the
       @method[snip-class-list<%> add] method. Note that if the same
       name is inserted into the same class list multiple times, all
       but the first insertion is ignored.}

]

See also @|snipclassdiscuss|.


@defconstructor[()]{

Creates a (useless) snip class.

}

@defmethod[(get-classname)
           string?]{

Returns the class's name, a string uniquely designating this snip
 class. For example, the standard text snip classname is
 @racket["wxtext"]. Names beginning with @litchar{wx} are reserved.

A snip class name should usually have the form @racket["((lib ...)
(lib ...))"]  to enable on-demand loading of the class. See
@|snipclassdiscuss| for details.

}

@defmethod[(get-version)
           exact-integer?]{

Returns the version of this snip class. When attempting to load a file
 containing a snip with the same class name but a different version,
 the user is warned.

}

@defmethod[(read [f (is-a?/c editor-stream-in%)])
           (or/c (is-a?/c snip%) #f)]{

@methspec{

Reads a snip from a given stream, returning a newly created snip as
 the result or @racket[#f] if there is an error.

}
@methimpl{

Returns @racket[#f].

}}

@defmethod[(read-header [f (is-a?/c editor-stream-in%)])
           boolean?]{

@methspec{

Called to read header information that may be useful for every snip
 read in this class. This method is only called once per editor read
 session, and only if the stream contains header information for this
 class.

The return value is @racket[#f] if a read error occurs or anything else
 otherwise.

See also @method[snip-class% write-header].

}
@methimpl{

Returns @racket[#t].

}}


@defmethod[(reading-version [stream (is-a?/c editor-stream-in%)])
           exact-integer?]{

Returns the version number specified for this snip class for snips
 currently being read from the given stream.

}


@defmethod[(set-classname [name string?])
           void?]{

Sets the class's name. See also @method[snip-class% get-classname].

}


@defmethod[(set-version [v exact-integer?])
           void?]{

Sets the version of this class. See @method[snip-class% get-version].

}

@defmethod[(write-header [stream (is-a?/c editor-stream-out%)])
           boolean?]{

@methspec{

Called to write header information that may be useful for every snip
 written for this class. This method is only called once per editor
 write session, and only if the editor contains snips in this class.

When reading the snips back in, @method[snip-class% read-header] will
 only be called if @method[snip-class% write-header] writes some data
 to the stream.

The return value is @racket[#f] if a write error occurs or anything else
 otherwise.

}
@methimpl{

Returns @racket[#t].

}}
}
