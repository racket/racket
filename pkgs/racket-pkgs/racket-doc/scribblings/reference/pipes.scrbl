#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "pipeports"]{Pipes}

A Racket @deftech{pipe} is internal to Racket, and not related to
OS-level pipes for communicating between different
processes.@margin-note*{OS-level pipes may be created by
@racket[subprocess], opening an existing named file on a Unix
filesystem, or starting Racket with pipes for its original input,
output, or error port. Such pipes are @tech{file-stream ports},
unlike the pipes produced by @racket[make-pipe].}

@defproc[(make-pipe [limit exact-positive-integer? #f]
                    [input-name any/c 'pipe]
                    [output-name any/c 'pipe])
         (values input-port? output-port?)]{

Returns two port values: the first port is an input port and the
second is an output port. Data written to the output port is read from
the input port, with no intermediate buffering. Unlike some other
kinds of ports, pipe ports do not need to be explicitly closed to be
reclaimed by @seclink["gc-model"]{garbage collection}.

If @racket[limit] is @racket[#f], the new pipe holds an unlimited
number of unread bytes (i.e., limited only by the available
memory). If @racket[limit] is a positive number, then the pipe will
hold at most @racket[limit] unread/unpeeked bytes; writing to the
pipe's output port thereafter will block until a read or peek from the
input port makes more space available. (Peeks effectively extend the
port's capacity until the peeked bytes are read.)

The optional @racket[input-name] and @racket[output-name] are used
as the names for the returned input and output ports, respectively.}

@defproc[(pipe-content-length [pipe-port port?]) exact-nonnegative-integer?]{

Returns the number of bytes contained in a pipe, where
@racket[pipe-port] is either of the pipe's ports produced by
@racket[make-pipe]. The pipe's content length counts all bytes that
have been written to the pipe and not yet read (though possibly
peeked).}
