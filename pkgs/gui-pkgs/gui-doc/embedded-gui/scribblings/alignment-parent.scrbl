#lang scribble/doc
@(require "common.rkt")

@definterface/title[alignment-parent<%> ()]{

@defmethod[(get-pasteboard) (is-a?/c pasteboard%)]{

The pasteboard that this alignment is being displayed to.}

@defmethod[(add-child [child (is-a?/c alignment<%>)]) void?]{

Add the given alignment as a child after the existing child.}

@defmethod[(delete-child [child (is-a?/c alignment<%>)]) void?]{

Deletes a child from the alignments.}

@defmethod[(is-shown?) boolean?]{

True if the alignment is being shown (accounting for its parent being
shown).}}

