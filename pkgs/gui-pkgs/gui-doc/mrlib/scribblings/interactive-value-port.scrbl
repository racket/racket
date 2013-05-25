#lang scribble/doc
@(require "common.rkt" (for-label mrlib/interactive-value-port scheme/pretty))

@title{Interactive Value Port}

@defmodule[mrlib/interactive-value-port]


@defproc[(set-interactive-display-handler [port output-port?]) void?]{

Sets @racket[port]'s display handler (via
@racket[port-display-handler]) so that when it encounters these
values:

@itemize[
 
 @item{exact, real, non-integral numbers}

 @item{syntax objects}

]

it uses @racket[write-special] to send snips to the port,
instead of those values. Otherwise, it behaves like the
default handler.

To show values embedded in lists and other compound object, it uses
@racket[pretty-print].}


@defproc[(set-interactive-write-handler [port output-port?]) void?]{

Like @racket[set-interactive-display-handler], but sets the
@racket[port-write-handler].}


@defproc[(set-interactive-print-handler [port output-port?]) void?]{

Like @racket[set-interactive-display-handler], but sets the
@racket[port-print-handler].}
