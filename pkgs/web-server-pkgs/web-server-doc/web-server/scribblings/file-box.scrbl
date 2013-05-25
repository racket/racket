#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "lang/file-box"]{File Boxes}
@(require (for-label web-server/lang/file-box
                     racket/serialize))

@defmodule[web-server/lang/file-box]{

As mentioned earlier, it is dangerous to rely on the store in
Web Language servlets, due to the deployment scenarios available
to them. This module provides a simple API to replace
boxes in a safe way.

@defproc[(file-box? [v any/c])
         boolean?]{Checks if @racket[v] is a file-box.}

@defproc[(file-box [p path-string?]
                   [v serializable?])
         file-box?]{
 Creates a file-box that is stored at @racket[p], with the default
 contents of @racket[v].
}

@defproc[(file-unbox [fb file-box?])
         serializable?]{
 Returns the value inside @racket[fb]
}

@defproc[(file-box-set? [fb file-box?])
         boolean?]{
 Returns @racket[#t] if @racket[fb] contains a value.
}

@defproc[(file-box-set! [fb file-box?]
                        [v serializable?])
         void]{
 Saves @racket[v] in the file represented by @racket[fb].
}

@warning{If you plan on using a load-balancer, make sure your file-boxes
are on a shared medium.}

}
