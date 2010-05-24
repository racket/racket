#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/port
                     racket/base))

@title[#:tag "port"]{Port}

@defmodule[unstable/port]

@unstable[@author+email["Vincent St-Amour" "stamourv@ccs.neu.edu"]]

@defthing[null-output-port output-port?]{
 Output port that discards everything written to it.
}
