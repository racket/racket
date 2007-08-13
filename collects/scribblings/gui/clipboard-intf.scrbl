#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[clipboard<%> ()]{

A single @scheme[clipboard<%>] object, @indexed-scheme[the-clipboard],
 manages the content of the system-wide clipboard for cut and paste.

Under X, a second @scheme[clipboard<%>] object,
 @indexed-scheme[the-x-selection-clipboard], manages the content of the
 system-wide X selection. If the @ResourceFirst{selectionAsClipboard}
 preference (see @|mrprefsdiscuss|) is set to a non-zero true value,
 however, then @scheme[the-clipboard] is always the same as
 @scheme[the-x-selection-clipboard], and the system-wide X clipboard
 is not used.

Under Windows and Mac OS X, @scheme[the-x-selection-clipboard] is
 always the same as @scheme[the-clipboard].

Data can be entered into a clipboard in one of two ways: by setting
 the current clipboard string or byte string, or by installing a
 @scheme[clipboard-client%] object. When a client is installed,
 requests for clipboard data are directed to the client.

Generic data is always retrieved from the clipboard as a byte
 string. When retrieving clipboard data, a data type string specifies
 the format of the data string. The availability of different
 clipboard formats is determined by the current clipboard owner.


@defmethod[(get-clipboard-bitmap [time (and/c exact? integer?)])
           (or/c (is-a?/c bitmap%) false/c)]{

Gets the current clipboard contents as a bitmap (Windows, Mac OS X),
 returning @scheme[#f] if the clipboard does not contain a bitmap.

See
@method[clipboard<%> get-clipboard-data] for information on eventspaces and the current clipboard client.

See @|timediscuss| for a discussion of the @scheme[time] argument.  If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(get-clipboard-data [format string]
                               [time (and/c exact? integer?)])
           (or/c byte string false/c)]{

Gets the current clipboard contents in a specific format, returning
 @scheme[#f] if the clipboard does not contain data in the requested
 format.

If the clipboard client is associated to an eventspace that is not the
 current one, the data is retrieved through a callback event in the
 client's eventspace. If no result is available within one second, the
 request is abandoned and @scheme[#f] is returned.

See @xmethod[clipboard-client% add-type] for information on
@scheme[format].

See @|timediscuss| for a discussion of the @scheme[time] argument.  If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(get-clipboard-string [time (and/c exact? integer?)])
           (or/c string false/c)]{

Gets the current clipboard contents as simple text, returning
 @scheme[#f] if the clipboard does not contain any text.

See @method[clipboard<%> get-clipboard-data] for information on
eventspaces and the current clipboard client.

See @|timediscuss| for a discussion of the @scheme[time] argument.  If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(set-clipboard-bitmap [new-bitmap (is-a?/c bitmap%)]
                                 [time (and/c exact? integer?)])
           void?]{

Changes the current clipboard contents to @scheme[new-bitmap] (Windows, Mac OS X)
 and releases the current clipboard client (if any).

See @|timediscuss| for
 a discussion of the @scheme[time] argument.  If @scheme[time] is outside
 the platform-specific range of times, @|MismatchExn|.

}

@defmethod[(set-clipboard-client [new-owner (is-a?/c clipboard-client%)]
                                 [time (and/c exact? integer?)])
           void?]{

Changes the clipboard-owning client: sets the client to
 @scheme[new-owner] and associates @scheme[new-owner] with the current
 eventspace (as determined by @scheme[current-eventspace]). The
 eventspace association is removed when the client is no longer the
 current one.

See @|timediscuss| for a discussion of the @scheme[time] argument. If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(set-clipboard-string [new-text string]
                                 [time (and/c exact? integer?)])
           void?]{

Changes the current clipboard contents to @scheme[new-text],
 and releases the current clipboard client (if any).

See @|timediscuss| for
 a discussion of the @scheme[time] argument.  If @scheme[time] is outside
 the platform-specific range of times, @|MismatchExn|.
}
}

