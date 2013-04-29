#lang scribble/doc
@(require "common.rkt")

@definterface/title[dllist<%> ()]{

Defines a doubly-linked.

@defmethod*[([(next) (is-a?/c dllist<%>)]
             [(next [new-next (is-a?/c dllist<%>)]) void?])]{

Gets/sets the next field to be the given dllist.}

@defmethod*[([(prev) (is-a?/c dllist<%>)]
             [(prev [new-prev (is-a?/c dllist<%>)]) void?])]{

Gets/sets the previous item in the list.}

@defmethod[(for-each [f ((is-a?/c dllist<%>) . -> . void?)]) void?]{

Applies @racket[f] to every element of the dllist.}

@defmethod[(map-to-list [f ((is-a?/c dllist<%>) . -> . any/c)]) 
           (listof any/c)]{

Creates a Racket list by applying @racket[f] to every element
of @this-obj[].}

}


