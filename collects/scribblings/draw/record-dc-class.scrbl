#lang scribble/doc
@(require "common.rkt")

@defclass/title[record-dc% object% (dc<%>)]{

A @racket[record-dc%] object records drawing actions for replay into
 another drawing context. The recorded drawing operations can be
 extracted as a procedure via @method[record-dc%
 get-recorded-procedure], or the actions can be extracted as a datum
 (that can be printed with @racket[write] and recreated with
 @racket[read]) via @method[record-dc% get-recorded-datum].

@defconstructor[([width (>=/c 0) 640]
                 [height (>=/c 0) 480])]{

Creates a new recording DC. The optional @racket[width] and
 @racket[height] arguments determine the result of @method[dc<%>
 get-size] on the recording DC; the @racket[width] and
 @racket[height] arguments do not clip drawing.}


@defmethod[(get-recorded-datum) any/c]{

Extracts a recorded drawing to a value that can be printed with
@racket[write] and re-read with @racket[read]. Use
@racket[recorded-datum->procedure] to convert the datum to a drawing
procedure.}


@defmethod[(get-recorded-procedure) ((is-a?/c dc<%>) . -> . void?)]{

Extracts a recorded drawing to a procedure that can be applied to
another DC to replay the drawing commands to the given DC.

The @method[record-dc% get-recorded-procedure] method can be more
efficient than composing @method[record-dc% get-recorded-datum] and
@racket[recorded-datum->procedure].}}
