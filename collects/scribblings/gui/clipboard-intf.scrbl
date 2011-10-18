#lang scribble/doc
@(require "common.rkt")

@definterface/title[clipboard<%> ()]{

A single @racket[clipboard<%>] object, @indexed-racket[the-clipboard],
 manages the content of the system-wide clipboard for cut and paste.

On Unix, a second @racket[clipboard<%>] object,
 @indexed-racket[the-x-selection-clipboard], manages the content of the
 system-wide X11 selection. If the @ResourceFirst{selectionAsClipboard}
 preference (see @|mrprefsdiscuss|) is set to a non-zero true value,
 however, then @racket[the-clipboard] is always the same as
 @racket[the-x-selection-clipboard], and the system-wide X11 clipboard
 is not used.

On Windows and Mac OS X, @racket[the-x-selection-clipboard] is
 always the same as @racket[the-clipboard].

Data can be entered into a clipboard in one of two ways: by setting
 the current clipboard string or byte string, or by installing a
 @racket[clipboard-client%] object. When a client is installed,
 requests for clipboard data are directed to the client.

Generic data is always retrieved from the clipboard as a byte
 string. When retrieving clipboard data, a data type string specifies
 the format of the data string. The availability of different
 clipboard formats is determined by the current clipboard owner.


@defmethod[(get-clipboard-bitmap [time exact-integer?])
           (or/c (is-a?/c bitmap%) #f)]{

Gets the current clipboard contents as a bitmap (Windows, Mac OS X),
 returning @racket[#f] if the clipboard does not contain a bitmap.

See
@method[clipboard<%> get-clipboard-data] for information on eventspaces and the current clipboard client.

See @|timediscuss| for a discussion of the @racket[time] argument.  If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(get-clipboard-data [format string?]
                               [time exact-integer?])
           (or/c bytes? string? #f)]{

Gets the current clipboard contents in a specific format, returning
 @racket[#f] if the clipboard does not contain data in the requested
 format.

If the clipboard client is associated to an eventspace that is not the
 current one, the data is retrieved through a callback event in the
 client's eventspace. If no result is available within one second, the
 request is abandoned and @racket[#f] is returned.

See @xmethod[clipboard-client% add-type] for information on
@racket[format].

See @|timediscuss| for a discussion of the @racket[time] argument.  If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(get-clipboard-string [time exact-integer?])
           string?]{

Gets the current clipboard contents as simple text, returning
 @racket[""] if the clipboard does not contain any text.

See @method[clipboard<%> get-clipboard-data] for information on
eventspaces and the current clipboard client.

See @|timediscuss| for a discussion of the @racket[time] argument.  If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}


@defmethod[(same-clipboard-client? [owner (is-a?/c clipboard-client%)])
           boolean?]{

Returns @racket[#t] if @racket[owner] currently owns the clipboard,
@racket[#f] otherwise.}


@defmethod[(set-clipboard-bitmap [new-bitmap (is-a?/c bitmap%)]
                                 [time exact-integer?])
           void?]{

Changes the current clipboard contents to @racket[new-bitmap] (Windows, Mac OS X)
 and releases the current clipboard client (if any).

See @|timediscuss| for
 a discussion of the @racket[time] argument.  If @racket[time] is outside
 the platform-specific range of times, @|MismatchExn|.

}

@defmethod[(set-clipboard-client [new-owner (is-a?/c clipboard-client%)]
                                 [time exact-integer?])
           void?]{

Changes the clipboard-owning client: sets the client to
 @racket[new-owner] and associates @racket[new-owner] with the current
 eventspace (as determined by @racket[current-eventspace]). The
 eventspace association is removed when the client is no longer the
 current one.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(set-clipboard-string [new-text string?]
                                 [time exact-integer?])
           void?]{

Changes the current clipboard contents to @racket[new-text],
 and releases the current clipboard client (if any).

See @|timediscuss| for
 a discussion of the @racket[time] argument.  If @racket[time] is outside
 the platform-specific range of times, @|MismatchExn|.
}
}

