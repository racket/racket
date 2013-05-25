#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "contmarks"]{Continuation Marks}

A mark can be attached to the current continuation frame using
@cppi{scheme_set_cont_mark}. To force the creation of a new frame
(e.g., during a nested function call within your function), use
@cppi{scheme_push_continuation_frame}, and then remove the frame with
@cppi{scheme_pop_continuation_frame}.

@function[(void scheme_set_cont_mark
           [Scheme_Object* key]
           [Scheme_Object* val])]{

Add/sets a continuation mark in the current continuation.}

@function[(void scheme_push_continuation_frame
           [Scheme_Cont_Frame_Data* data])]{

Creates a new continuation frame. The @var{data} record need not be
 initialized, and it can be allocated on the C stack. Supply @var{data} to
 @cpp{scheme_pop_continuation_frame} to remove the continuation frame.}

@function[(void scheme_pop_continuation_frame
           [Scheme_Cont_Frame_Data* data])]{

Removes a continuation frame created by  @cpp{scheme_push_continuation_frame}.}
