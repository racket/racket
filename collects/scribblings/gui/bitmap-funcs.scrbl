#lang scribble/doc
@(require "common.ss")

@title{Bitmaps}


@defproc[(make-bitmap [width exact-positive-integer?]
                      [height exact-positive-integer?]
                      [alpha? any/c #t])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% width height #f alpha?)], but
this procedure is preferred because it defaults @racket[alpha?] in a
more useful way.}


@defproc[(make-monochrome-bitmap [width exact-positive-integer?]
                                 [height exact-positive-integer?]
                                 [bits (or/c bytes? #f) #f])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% width height #t)] if
@racket[bits] is @racket[#f], or @racket[(make-object bitmap% bits
width height)] otherwise. This procedure is preferred to using
@racket[make-object] on @racket[bitmap%] because it is less
overloaded.}


@defproc[(read-bitmap [in (or path-string? input-port?)]
                      [kind (one-of/c 'unknown 'unknown/mask 'unknown/alpha
                                      'gif 'gif/mask 'gif/alpha 
                                      'jpeg 'jpeg/alpha
                                      'png 'png/mask 'png/alpha
                                      'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                      'bmp 'bmp/alpha)
                            'unknown/alpha])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% in kind)], but this procedure is
preferred because it defaults @racket[kind] in a more useful way.}
