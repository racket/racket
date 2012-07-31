#lang scribble/doc
@(require "common.rkt")

@title{Drawing Functions}

@local-table-of-contents[]

@defparam[current-ps-setup pss (is-a?/c ps-setup%)]{

A parameter that determines the current PostScript configuration
 settings. See @racket[post-script-dc%] and @racket[printer-dc%].}


@defproc[(get-face-list [kind (or/c 'mono 'all) 'all]
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


@defproc[(get-family-builtin-face [family (or/c 'default 'decorative 'roman 'script 
                                                'swiss 'modern 'symbol 'system)])
         string?]{

Returns the built-in default face mapping for a particular font
 family.

See @racket[font%] for information about @racket[family].}


@defproc[(make-bitmap [width exact-positive-integer?]
                      [height exact-positive-integer?]
                      [alpha? any/c #t])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% width height #f alpha?)], but
this procedure is preferred because it defaults @racket[alpha?] in a
more useful way.

See also @racket[make-platform-bitmap] and @secref["Portability"].
}


@defproc[(make-brush
          [#:color color (or/c string? (is-a?/c color%))  (make-color 0 0 0)]
          [#:style style (or/c 'transparent 'solid 'opaque
                               'xor 'hilite 'panel
                               'bdiagonal-hatch 'crossdiag-hatch
                               'fdiagonal-hatch 'cross-hatch
                               'horizontal-hatch 'vertical-hatch)
                   'solid]
          [#:stipple stipple (or/c #f (is-a?/c bitmap%))
                     #f]
          [#:gradient gradient (or/c #f
                                    (is-a?/c linear-gradient%)
                                    (is-a?/c radial-gradient%))
                      #f]
          [#:transformation
           transformation (or/c #f (vector/c (vector/c real? real? real?
                                                       real? real? real?)
                                              real? real? real? real? real?))
                          #f]
          [#:immutable? immutable? any/c #t])
         (is-a?/c brush%)]{

Creates a @racket[brush%] instance. This procedure provides a
nearly equivalent interface compared to using
@racket[make-object] with @racket[brush%], but it also supports
the creation of immutable brushes (and creates immutable burshes by default).

When @racket[stipple] is @racket[#f], @racket[gradient] is
@racket[#f], @racket[transformation] is @racket[#f],
@racket[immutable?] is true, and @racket[color] is either a
@racket[color%] object or a string in @racket[the-color-database], the
result brush is created via @method[brush-list% find-or-create-brush] of
@racket[the-brush-list].}


@defproc[(make-color [red byte?] [green byte?] [blue byte?]
                     [alpha (real-in 0 1) 1.0])
         (is-a?/c color%)]{

Creates a @racket[color%] instance. This procedure provides a
nearly equivalent interface compared to using
@racket[make-object] with @racket[color%], but it creates
an immutable @racket[color%] object.

To create an immutable color based on a color string, use @method[color-database<%> find-color]
or @racket[the-color-database].}


@defproc[(make-font [#:size size (integer-in 1 1024) 12]
                    [#:face face (or/c string? #f) #f]
                    [#:family family (or/c 'default 'decorative 'roman 'script 
                                           'swiss 'modern 'symbol 'system)
                              'default]
                    [#:style style (or/c 'normal 'italic 'slant) 'normal]
                    [#:weight weight (or/c 'normal 'bold 'light) 'normal]
                    [#:underlined? underlined? any/c #f]
                    [#:smoothing smoothing (or/c 'default 'partly-smoothed 
                                                 'smoothed 'unsmoothed) 
                                 'default]
                    [#:size-in-pixels? size-in-pixels? any/c #f]
                    [#:hinting hinting (or/c 'aligned 'unaligned) 'aligned])
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


@defproc[(make-pen
          [#:color color (or/c string? (is-a?/c color%)) (make-color 0 0 0)]
          [#:width width (real-in 0 255) 0]
          [#:style style (or/c 'transparent 'solid 'xor 'hilite
                               'dot 'long-dash 'short-dash 'dot-dash
                               'xor-dot 'xor-long-dash 'xor-short-dash
                               'xor-dot-dash)
                   'solid]
          [#:cap cap (or/c 'round 'projecting 'butt)
                     'round]
          [#:join join (or/c 'round 'bevel 'miter)
                  'round]
          [#:stipple stipple (or/c #f (is-a?/c bitmap%))
                     #f]
          [#:immutable? immutable? any/c #t])
         (is-a?/c pen%)]{

Creates a @racket[pen%] instance. This procedure provides a
nearly equivalent interface compared to using
@racket[make-object] with @racket[pen%], but it also supports
the creation of immutable pens (and creates immutable pens by default).

When @racket[stipple] is @racket[#f], @racket[immutable?] is true, and
@racket[color] is either a @racket[color%] object or a string in
@racket[the-color-database], the result pen is created via
@method[pen-list% find-or-create-pen] of @racket[the-pen-list].}


@defproc[(make-platform-bitmap [width exact-positive-integer?]
                               [height exact-positive-integer?])
         (is-a?/c bitmap%)]{

Creates a bitmap that uses platform-specific drawing operations
as much as possible, which is different than a @racket[make-bitmap] result
on Windows and Mac OS X. See @secref["Portability"] for more information.}
                 

@defproc[(read-bitmap [in (or path-string? input-port?)]
                      [kind (or/c 'unknown 'unknown/mask 'unknown/alpha
                                  'gif 'gif/mask 'gif/alpha 
                                  'jpeg 'jpeg/alpha
                                  'png 'png/mask 'png/alpha
                                  'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                  'bmp 'bmp/alpha)
                            'unknown/alpha]
                      [bg-color (or/c (is-a?/c color%) #f) #f]
                      [complain-on-failure? any/c #t])
         (is-a?/c bitmap%)]{

Returns @racket[(make-object bitmap% in kind bg-color
complain-on-failure?)], but this procedure is preferred because it
defaults @racket[kind] and @racket[complain-on-failure?] in a more
useful way.}


@defproc[(recorded-datum->procedure [datum any/c]) ((is-a?/c dc<%>) . -> . void?)]{

Converts a value from @xmethod[record-dc% get-recorded-datum] to a drawing procedure.}


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
