#lang scribble/doc
@(require "common.rkt")

@defclass/title[column-control-event% control-event% ()]{

A @racket[column-control-event%] object contains information about a
 event on an @racket[list-box%] column header.

@defconstructor[([column exact-nonnegative-integer?]
                 [event-type (or/c 'list-box-column)]
                 [time-stamp exact-integer? 0])]{

The @racket[column] argument indicates the column that was clicked.
}

@defmethod[(get-column) exact-nonnegative-integer?]{

Returns the column number (counting from 0) of the clicked column.
}

@defmethod[(set-column
            [column exact-nonnegative-integer?])
           void?]{


Sets the column number (counting from 0) of the clicked column.

}}
