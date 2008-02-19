#lang scribble/doc
@(require "common.ss")

@defclass/title[string-snip% snip% ()]{

An instance of @scheme[string-snip%] is created automatically when
 text is inserted into a text editor. See also @xmethod[text%
 on-new-string-snip].


@defconstructor*/make[(([allocsize nonnegative-exact-integer? 0])
                       ([s string?]))]{

Creates a string snip whose initial content is @scheme[s], if
 supplied, empty otherwise. In the latter case, the optional
 @scheme[allocsize] argument is a hint about how much storage space
 for text should be initially allocated by the snip.

}


@defmethod[(insert [s string?]
                   [len nonnegative-exact-integer?]
                   [pos nonnegative-exact-integer? 0])
           void?]{

Inserts @scheme[s] (with length @scheme[len]) into the snip at relative
 @techlink{position} @scheme[pos] within the snip.

}


@defmethod[(read [len nonnegative-exact-integer?]
                 [f (is-a?/c editor-stream-in%)])
           void?]{

Reads the snip's data from the given stream. 

The @scheme[len] argument specifies the maximum length of the text to
 be read.  (When a text snip is written to a file, the very first
 field is the length of the text contained in the snip.)  This method
 is usually invoked by the text snip class's @method[snip-class% read]
 method.

}}

