#lang scribble/doc
@(require scribble/manual
          (for-label racket/gui 
                     file/xpm))

@title[#:tag "xpm"]{XPM File Reading}

@defmodule[file/xpm]

The @racketmodname[file/xpm] library provides functions for
reading XPM files and converting them to @racket[bitmap%] objects.

@defproc[(xpm-read) xpm?]{Reads an XPM from the current input port.}

@defproc[(xpm->bitmap% [xpm xpm?]) (is-a?/c bitmap%)]{Converts an XPM to a @racket[bitmap%].}

@defstruct*[xpm ([var string?]
                 [width exact-integer?]
                 [height exact-integer?]
                 [color-ht (hash/c symbol? (hash/c symbol? string?))]
                 [x-hotspot (or/c false/c exact-integer?)]
                 [y-hotspot (or/c false/c exact-integer?)]
                 [pixels (listof (listof symbol?))]
                 [extensions (listof string?)])]
