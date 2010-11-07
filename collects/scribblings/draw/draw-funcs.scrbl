#lang scribble/doc
@(require "common.ss")

@title{Drawing Functions}

@local-table-of-contents[]

@defparam[current-ps-setup pss (is-a?/c ps-setup%)]{

A parameter that determines the current PostScript configuration
 settings. See @scheme[post-script-dc%] and @scheme[printer-dc%].

}

@defproc[(get-face-list [family (one-of/c 'mono 'all) 'all])
         (listof string?)]{

Returns a list of font face names available on the current system. If
 @scheme['mono] is provided as the argument, then only faces that are
 known to correspond to monospace fonts are included in the list.

}

@defproc[(get-family-builtin-face [family (one-of/c 'default 'decorative 'roman 'script 
                                                    'swiss 'modern 'symbol 'system)])
         string?]{

Returns the built-in default face mapping for a particular font
 family.

See @scheme[font%] for information about @scheme[family].

}
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
                            'unknown/alpha]
                      [bg-color (or/c (is-a?/c color%) false/c) #f]
                      [complain-on-failure? any/c #t])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% in kind bg-color
complain-on-failure?)], but this procedure is preferred because it
defaults @racket[kind] and @racket[complain-on-failure?] in a more
useful way.}


@defthing[the-brush-list (is-a?/c brush-list%)]{

See @scheme[brush-list%].

}

@defthing[the-color-database (is-a?/c color-database<%>)]{

See @scheme[color-database<%>].

}

@defthing[the-font-list (is-a?/c font-list%)]{

See @scheme[font-list%].

}

@defthing[the-font-name-directory (is-a?/c font-name-directory<%>)]{

See @scheme[font-name-directory<%>].


}

@defthing[the-pen-list (is-a?/c pen-list%)]{

See @scheme[pen-list%].

}
