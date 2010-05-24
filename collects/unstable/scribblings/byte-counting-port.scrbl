#lang scribble/manual
@(require "utils.rkt" (for-label racket unstable/byte-counting-port))

@title{Byte Counting Ports}

@defmodule[unstable/byte-counting-port]

@unstable[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

This library provides an output port constructor like @racket[open-output-nowhere], except it counts how many bytes have been written (available through @racket[file-position].)

@defproc[(make-byte-counting-port [name any/c 'byte-counting-port])
         output-port?]{
Creates and returns an output port that discards all output sent to it (without blocking.) The @racket[name] argument is used as the port's name. The total number bytes written is available through @racket[file-position].}
                      