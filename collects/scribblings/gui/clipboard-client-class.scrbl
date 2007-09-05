#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@define-class-doc[clipboard-client% object% ()]{

A @scheme[clipboard-client%] object allows a program to take over
 the clipboard and service requests for clipboard data. See
 @scheme[clipboard<%>] for more information.

A @scheme[clipboard-client%] object is associated to an eventspace
 when it becomes the current client; see
@method[clipboard<%> set-clipboard-client] for more information.


@defconstructor[()]{

Creates a clipboard client that supports no data formats.

}

@defmethod[(add-type [format string])
           void?]{

Adds a new data format name to the list supported by the clipboard
 client.

@clipboardtypes[]

}

@defmethod[(get-data [format string])
           (or/c bytes? string? false/c)]{

Called when a process requests clipboard data while this client is the
 current one for the clipboard. The requested format is passed to the
 method, and the result should be a byte string matching the requested
 format, or @scheme[#f] if the request cannot be fulfilled.

Only data format names in the client's list will be passed to this
 method; see @method[clipboard-client% add-type].

When this method is called by the clipboard, the current eventspace is
 the same as the client's eventspace. If, at the point of the
 clipboard request, the current eventspace is not the client's
 eventspace, then current thread is guaranteed to be the handler
 thread of the client's eventspace.

}

@defmethod[(get-types)
           (listof string?)]{

Returns a list of names that are the data formats supported by the
 clipboard client.

}

@defmethod[(on-replaced)
           void?]{
Called when a clipboard client is dismissed as the clipboard owner
 (because the clipboard has be taken by another client or by an
 external application).

}

}
