#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.ss"
          (for-label unstable/file
                     scheme/file
                     scheme/contract
                     scheme/base))

@title[#:tag "file"]{Filesystem}

@defmodule[unstable/file]

@unstable[@author+email["Jay McCarthy" "jay@plt-scheme.org"]]

@defproc[(make-directory*/ignore-exists-exn [pth path-string?])
         void]{
 Like @scheme[make-directory*], except it ignores errors when the path already exists. Useful to deal with race conditions on processes that create directories.
}
              
@defproc[(rename-file-or-directory/ignore-exists-exn [from path-string?] [to path-string?])
         void]{
 Like @scheme[rename-file-or-directory], except it ignores errors when the path already exists. Useful to deal with race conditions on processes that create files.
}