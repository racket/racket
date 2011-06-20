#lang scribble/doc
@(require "common.rkt" (for-label mrlib/interactive-value-port scheme/pretty))

@title{Interactive Value Port}

@defmodule[mrlib/interactive-value-port]


@defproc[(set-interactive-display-handler [port output-port?]) void?]{

Sets @scheme[port]'s display handler (via
@scheme[port-display-handler]) so that when it encounters these
values:

@itemize[
 
 @item{exact, real, non-integral numbers}

 @item{syntax objects}

]

it uses @scheme[write-special] to send snips to the port,
instead of those values. Otherwise, it behaves like the
default handler.

To show values embedded in lists and other compound object, it uses
@scheme[pretty-print].}


@defproc[(set-interactive-write-handler [port output-port?]) void?]{

Like @scheme[set-interactive-display-handler], but sets the
@scheme[port-write-handler].}


@defproc[(set-interactive-print-handler [port output-port?]) void?]{

Like @scheme[set-interactive-display-handler], but sets the
@scheme[port-print-handler].}
