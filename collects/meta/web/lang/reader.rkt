#lang s-exp syntax/module-reader

meta/web/common/main

;; Similar to `#lang scribble/html', but with a plain scribble reader
;; (not the inside one).

#:read        scribble:read
#:read-syntax scribble:read-syntax
#:info        (scribble-base-reader-info)

(require (prefix-in scribble: scribble/reader)
         (only-in scribble/base/reader scribble-base-reader-info))
