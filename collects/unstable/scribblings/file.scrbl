#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/file
                     racket/file
                     racket/contract
                     racket/base))

@title[#:tag "file"]{Filesystem}
@unstable[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

@defmodule[unstable/file]

@defproc[(make-directory*/ignore-exists-exn [pth path-string?])
         void]{
 Like @racket[make-directory*], except it ignores errors when the path already exists. Useful to deal with race conditions on processes that create directories.
}
