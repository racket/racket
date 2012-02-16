#lang scribble/doc
@(require "common.rkt")

@title{Drawing Functions}

@local-table-of-contents[]

@defparam[current-ps-setup pss (is-a?/c ps-setup%)]{

A parameter that determines the current PostScript configuration
 settings. See @racket[post-script-dc%] and @racket[printer-dc%].}


@defproc[(get-face-list [kind (one-of/c 'mono 'all) 'all]
                        [#:all-variants? all-variants? any/c #f])
         (listof string?)]{

Returns a list of font face names available on the current system.  If
 @racket[kind] is @racket['mono], then only faces that are known to
 correspond to monospace fonts are included in the list.

If @racket[all-variants?] is @racket[#f] (the default), then the
 result is in more standard terminology a list of font
 family names, which are combined with style and weight options to
 arrive at a face; if @racket[all-variants?] is true, then the result
 includes a string for each available face in the family.}


@defproc[(get-family-builtin-face [family (one-of/c 'default 'decorative 'roman 'script 
                                                    'swiss 'modern 'symbol 'system)])
         string?]{

Returns the built-in default face mapping for a particular font
 family.

See @racket[font%] for information about @racket[family].}


@defproc[(make-platform-bitmap [width exact-positive-integer?]
                               [height exact-positive-integer?])
         (is-a?/c bitmap%)]{
  Creates a bitmap that draws in a way that is the same as drawing to a
  @racket[canvas%]'s @racket[dc<%>] (in its default configuration)
  under Mac OS X and Windows, and creates a bitmap that draws the way 
  the result of @racket[make-bitmap] draws under Unix.
  
  In general, @racket[make-platform-bitmap] produces better looking
  bitmaps on more platforms than 
  @racket[make-bitmap], @racket[make-screen-bitmap], or creating
  a @racket[bitmap%] object directly. 
  Also, unlike @racket[make-screen-bitmap],
  @racket[make-platform-bitmap]'s implementation does not
  depend on @racketmodname[racket/gui/base], making it
  available in more contexts.
  Accordingly, in the absence
  of other constraints, @racket[make-platform-bitmap]
  should be used in preference to other ways of creating bitmaps.
    
  That said, there are two drawbacks to @racket[make-platform-bitmap].
  First, it will use more constrained resources than
  @racket[make-bitmap] does, especially under Windows. One possible
  approach to dealing with this problem for long-lived bitmaps
  is to draw into the result of a @racket[make-platform-bitmap]
  and then copy the contents of the drawing into the result
  of a @racket[make-bitmap]. This preserves the better quality
  drawing, but holds onto the constrained resources only during
  the drawing process.  
  
  The other drawback is that @racket[make-platform-bitmap] does not
  create bitmaps with an alpha channel under Windows 
  (instead, the bitmaps have a white, solid background). 
  If you need bitmaps with alpha channels, use @racket[make-bitmap]
  instead.
}
                 
@defproc[(make-bitmap [width exact-positive-integer?]
                      [height exact-positive-integer?]
                      [alpha? any/c #t])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% width height #f alpha?)], but
this procedure is preferred because it defaults @racket[alpha?] in a
more useful way.

See also @racket[make-platform-bitmap].
}


@defproc[(make-font [#:size size (integer-in 1 1024) 12]
                    [#:face face (or/c string? #f) #f]
                    [#:family family (one-of/c 'default 'decorative 'roman 'script 
                                               'swiss 'modern 'symbol 'system)
                              'default]
                    [#:style style (one-of/c 'normal 'italic 'slant) 'normal]
                    [#:weight weight (one-of/c 'normal 'bold 'light) 'normal]
                    [#:underlined? underlined? any/c #f]
                    [#:smoothing smoothing (one-of/c 'default 'partly-smoothed 
                                                      'smoothed 'unsmoothed) 
                                 'default]
                    [#:size-in-pixels? size-in-pixels? any/c #f])
         (is-a?/c font%)]{

Creates a @racket[font%] instance. This procedure provides an
equivalent but more convenient interface compared to using
@racket[make-object] with @racket[font%].
}


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

See @racket[brush-list%].

}

@defthing[the-color-database (is-a?/c color-database<%>)]{

See @racket[color-database<%>].

}

@defthing[the-font-list (is-a?/c font-list%)]{

See @racket[font-list%].

}

@defthing[the-font-name-directory (is-a?/c font-name-directory<%>)]{

See @racket[font-name-directory<%>].


}

@defthing[the-pen-list (is-a?/c pen-list%)]{

See @racket[pen-list%].

}
