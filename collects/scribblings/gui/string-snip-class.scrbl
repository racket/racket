#lang scribble/doc
@(require "common.rkt")

@defclass/title[string-snip% snip% ()]{

An instance of @racket[string-snip%] is created automatically when
 text is inserted into a text editor. See also @xmethod[text%
 on-new-string-snip].


@defconstructor*/make[(([allocsize exact-nonnegative-integer? 0])
                       ([s string?]))]{

Creates a string snip whose initial content is @racket[s], if
 supplied, empty otherwise. In the latter case, the optional
 @racket[allocsize] argument is a hint about how much storage space
 for text should be initially allocated by the snip.

}


@defmethod[(insert [s string?]
                   [len exact-nonnegative-integer?]
                   [pos exact-nonnegative-integer? 0])
           void?]{

Inserts @racket[s] (with length @racket[len]) into the snip at relative
 @techlink{position} @racket[pos] within the snip.

}


@defmethod[(read [len exact-nonnegative-integer?]
                 [f (is-a?/c editor-stream-in%)])
           void?]{

Reads the snip's data from the given stream. 

The @racket[len] argument specifies the maximum length of the text to
 be read.  (When a text snip is written to a file, the very first
 field is the length of the text contained in the snip.)  This method
 is usually invoked by the text snip class's @method[snip-class% read]
 method.

}}

