#lang scribble/manual

@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label images/icons/arrow
                     mrlib/switchable-button
                     racket
                     racket/draw)
          images/icons/arrow)

@(define (author-email) "neil.toronto@gmail.com")

@title{Icons}
@author{@(author+email "Neil Toronto" (author-email))}


@(define icons-eval (make-base-eval))
@interaction-eval[#:eval icons-eval (require racket/math racket/list images/icons/style)]

@;{
@section{Introduction (What is an icon, really?)}
@margin-note*{This introduction describes an ideal, not necessarily the current state of things.}

As a first approximation, an icon is just a small bitmap with an alpha channel.
But the icons in this collection are not simply loaded from disk.
They are generated programmatically by drawing on a @racket[dc<%>], 
Icons can also be rendered , resized, generated programmatically by drawing on a , or composed using @racket[pict] functions.

The @racketmodname[icons] module is intended to make it possible to do all of these things, and to make it easy to get common icons in different colors, heights and styles.

An icon communicates. Its shape and color are a visual metaphor for an action or a message. Icons should be easily recognizable, distinguishable, visually consistent, and metaphorically appropriate for the actions and messages they are used with. This is difficult to do well, but good examples, good abstractions, and an existing icon library help considerably.

@(define (hash-quote) (hash-quote-flomap (solid-icon-color '(41 128 38)) 16))
@(define (step) (step-flomap (solid-icon-color "blue") 16 'diffuse))
@(define (macro-stepper) (ht-append (hash-quote) (step)))
   
Example: The Macro Stepper tool composes a new icon @(hash-quote) and an existing icon @(step), resulting in @(macro-stepper) for its toolbar icon.
The @(hash-quote) icon connotes syntax, and is the color of a syntax-quote as rendered by DrRacket by default.
The @(step) icon is colored like DrRacket colors identifier syntax by default, and is shaped using metaphors used in debugger toolbars, TV remotes, and music players around the world.
It is composed of @(go-icon (solid-icon-color "blue") 16 'diffuse) to connote starting and @(bar-icon (solid-icon-color "blue") 16 'diffuse) to connote immediately stopping---a ``step.''

The author of this collection is available to adapt or create SVG icons for DrRacket tools, and charges no more than your immortal soul.

@section{Icon Parameters}

@doc-apply[toolbar-icon-height]{
The height of DrRacket toolbar icons.

Use @racket[(toolbar-icon-height)] as the @racket[height] argument when loading common icons that will be used in DrRacket toolbars and buttons, or in the toolbars and buttons of DrRacket tools.

(When making an icon for DrRacket's main toolbar, try to keep it nearly square so that it will not take up too much horizontal space when the toolbar is docked vertically.
If you cannot, as with the Macro Stepper, send a thinner icon as the @racket[alternate-bitmap] argument to a @racket[switchable-button%].)
}

@doc-apply[default-icon-height]{
The height of standard (e.g. not toolbar) DrRacket icons, used as a default argument through the @racketmodname[icons] module.
}

@doc-apply[default-icon-style]{
The style of DrRacket icons, used as a default argument throughout the @racketmodname[icons] module.
}
}

@section[#:tag "arrows"]{Arrow Icons}

@defmodule[images/icons/arrow]

@interaction-eval[#:eval icons-eval (require images/icons/arrow)]

@doc-apply[right-arrow-icon]
@doc-apply[left-arrow-icon]
@doc-apply[up-arrow-icon]
@doc-apply[down-arrow-icon]{
Cardinal direction arrows.

@interaction[#:eval icons-eval
                    (list (right-arrow-icon syntax-icon-color (toolbar-icon-height))
                          (left-arrow-icon run-icon-color)
                          (up-arrow-icon halt-icon-color 37)
                          (down-arrow-icon "lightblue" 44 glass-icon-material))]
}

@doc-apply[right-over-arrow-icon]
@doc-apply[left-over-arrow-icon]
@doc-apply[right-under-arrow-icon]
@doc-apply[left-under-arrow-icon]{
@interaction[#:eval icons-eval
                    (list (right-over-arrow-icon metal-icon-color (toolbar-icon-height))
                          (left-over-arrow-icon dark-metal-icon-color)
                          (right-under-arrow-icon run-icon-color 37)
                          (left-under-arrow-icon "lightgreen" 44 glass-icon-material))]
}

@section[#:tag "control"]{Control Icons}

@section[#:tag "file"]{File Icons}

@section[#:tag "tool"]{Tool Icons}

@section[#:tag "stickman"]{Stickman Icons}

@section[#:tag "misc"]{Miscellaneous Icons}

@;{
@subsection{Control Icons}

@doc-apply[go-icon]
@doc-apply[bar-icon]
@doc-apply[back-icon]
@doc-apply[stop-icon]
@doc-apply[record-icon]
@doc-apply[step-icon]
@doc-apply[step-back-icon]
@doc-apply[continue-icon]
@doc-apply[continue-back-icon]
@doc-apply[fast-forward-icon]
@doc-apply[rewind-icon]
@doc-apply[pause-icon]{
These return typical ``playback'' icons.

@interaction[#:eval icons-eval 
                    (for/list ([make-icon  (list rewind-icon continue-back-icon
                                                 step-back-icon back-icon
                                                 pause-icon stop-icon
                                                 go-icon step-icon
                                                 continue-icon fast-forward-icon
                                                 record-icon)]
                               [style  (in-cycle icon-styles)])
                      (make-icon (solid-icon-color "darkseagreen") 32 style))]

The remaining icon @(bar-icon #f 16), returned by @racket[bar-icon], is used to build the others.
}

@subsection{Arrow Icons}

@doc-apply[up-arrow-icon]
@doc-apply[down-arrow-icon]
@doc-apply[left-arrow-icon]
@doc-apply[right-arrow-icon]{
@examples[#:eval icons-eval
                 (for/list ([make-icon  (list up-arrow-icon down-arrow-icon
                                              left-arrow-icon right-arrow-icon)])
                   (for/list ([style  (in-list icon-styles)])
                     (make-icon (solid-icon-color "brown") (default-icon-height) style)))]
}

@subsection{Sign Icons}

@doc-apply[stop-sign-icon]{
@examples[#:eval icons-eval (list (stop-sign-icon (default-icon-height) 'diffuse)
                                 (stop-sign-icon (default-icon-height) 'shiny))]
}

@subsection{Check Icons}

@doc-apply[check-icon]{
@examples[#:eval icons-eval
                 (list (check-icon (solid-icon-color "green") 29 'diffuse)
                       (check-icon (solid-icon-color "green") 29 'shiny))]
}

@doc-apply[x-icon]{
@examples[#:eval icons-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (x-icon color 29 style))]
}

@subsection{Miscellaneous Icons}

@doc-apply[magnifying-glass-icon]{
@examples[#:eval icons-eval (list (magnifying-glass-icon 31 'diffuse)
                                 (magnifying-glass-icon 31 'shiny))]
Note that the uncolorized magnifying glass has a brown handle.
}

@doc-apply[magnifying-glass-left-icon]{
@examples[#:eval icons-eval (list (magnifying-glass-left-icon 31 'diffuse)
                                 (magnifying-glass-left-icon 31 'shiny))]
}

@doc-apply[disk-icon]{
@examples[#:eval icons-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (disk-icon color 33 style))]
}

@doc-apply[earth-icon]{
@examples[#:eval icons-eval (list (earth-icon 48 'diffuse)
                                 (earth-icon 48 'shiny))]
}

@doc-apply[moon-icon]{
@examples[#:eval icons-eval (list (moon-icon 48 'diffuse)
                                 (moon-icon 48 'shiny))]
}

@subsection{Symbols}

@doc-apply[hash-quote-icon]{
@examples[#:eval icons-eval (list (hash-quote-icon (toolbar-icon-height) 'diffuse)
                                 (hash-quote-icon (toolbar-icon-height) 'shiny))]
}

@doc-apply[plus-icon]{
@examples[#:eval icons-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (plus-icon color 24 style))]
}

@doc-apply[times-icon]{
@examples[#:eval icons-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (times-icon color 24 style))]
}

@subsection{Logos}

@doc-apply[plt-logo]{
@examples[#:eval icons-eval
                 (list (plt-logo 128 'diffuse) (plt-logo 128 'shiny))]
}

@doc-apply[planet-logo]{
@examples[#:eval icons-eval (list (planet-logo 128 'diffuse)
                                 (planet-logo 128 'shiny))]
}


@section{Icon Constants and Contracts}

@;{
@doc-apply[icon-colors]{
A list containing the names of allowed icon colors.

When an SVG icon source file is rendered, it is rendered once directly. Then, for each color corresponding to a symbol in @racket[icon-colors], it is colorized by replacing gradients, and then rendered.

When loading an icon, a @racket[#f] color name loads an uncolorized rendering.
Every icon can be loaded with a @racket[#f] color name.
An icon can be loaded using any name in @racket[icon-colors] only if its SVG source has gradients that can be colorized.
See @secref["new-icons"] for details.

The actual hues associated with the color names are the hues of the first seven @racketmodname[plot] color numbers.
The following example illustrates the correspondence:
@interaction[#:eval icons-eval
                    (require plot)
                    (for/list ([color  (rest icon-colors)])
                      (stop-flomap color 48))
                    (parameterize ([plot-width 48]
                                   [plot-height 48]
                                   [plot-decorations? #f]
                                   [plot-background-alpha 0])
                      (for/list ([n  (in-range 7)])
                        (plot3d-pict (surface3d (λ (x y) (- (sqr x) (sqr y))) -1 1 -1 1
                                                #:color n #:line-color n
                                                #:samples 11 #:line-width 1))))]
This example also shows how to use @racketmodname[plot] to create icon @racket[pict]s from mathematical functions.
}}

@doc-apply[icon-color/c]{
A contract that identifies color names.
}

@doc-apply[icon-styles]{
Typical icon styles.

It is not necessary to have a version of each icon in each style.
But if an icon has different styles, it should have these.
}

@doc-apply[icon-style/c]{
A contract that identifies icon styles.
}


@section{Icon @racket[pict]s}

@interaction-eval[#:eval icons-eval (require slideshow/pict)]

It is more flexible, but a little more complicated, to load icons as @racket[pict]s.
As picts, icons can easily be appended, inset, superimposed, blurred, and more.
For example, it is easy to make modern-looking media player controls using @racket[cc-superimpose] and the @racket['shiny] style:
@interaction[#:eval icons-eval
                    (define media-icon-background (record-flomap 'blue 64 'shiny))
                    (list (cc-superimpose media-icon-background
                                          (step-back-flomap 'white 32 'shiny))
                          (cc-superimpose media-icon-background
                                          (pause-flomap 'white 32 'shiny))
                          (cc-superimpose media-icon-background
                                          (step-flomap 'white 32 'shiny)))]

Almost all of the functions in preceeding sections are defined in terms of the @racket[pict]-producing functions documented in this section.

To use these functions effectively, you should require @racketmodname[icons] and @racketmodname[slideshow/pict] together.
Use @racket[bitmap] to convert a @racket[bitmap%] (e.g. an icon) to a @racket[pict], and @racket[pict->bitmap] to convert back.

Converting from @racket[pict]s to bitmaps can be lossy. For example, converting text can look especially horrible:
@interaction[#:eval icons-eval
                    (scale (text "Hello" null 10) 5)
                    (scale (bitmap (pict->bitmap (text "Hello" null 10))) 5)]

Therefore, when composing icons from parts, try to work only with @racket[pict]s, and convert to an icon using @racket[pict->bitmap] as the last step.

When composing icons from parts, it is fine to use @racket[pict]s converted from @racket[bitmap%]s.
Without scaling or rotating, the conversion is lossless:
@interaction[#:eval icons-eval
                    (define not-blurry (magnifying-glass-icon 64 'shiny))
                    not-blurry
                    (for/fold ([icon not-blurry]) ([i  (in-range 30)])
                      (pict->bitmap (bitmap icon)))]

Avoid converting between @racket[pict]s and @racket[bitmap%]s more than once if bitmap-backed @racket[pict]s are scaled, rotated by angles that are not multiples of 90 degrees, or superimposed or appended at non-integer coordinates.
Avoid scaling up in general.

@doc-apply[load-flomap]{
Corresponds to @racket[load-icon]. In fact, @racket[load-icon] uses @racket[load-flomap] to load the icon as a @racket[pict], and passes it to @racket[pict->bitmap].
}

@doc-apply[go-flomap]
@doc-apply[bar-flomap]
@doc-apply[back-flomap]
@doc-apply[stop-flomap]
@doc-apply[record-flomap]
@doc-apply[step-flomap]
@doc-apply[step-back-flomap]
@doc-apply[continue-flomap]
@doc-apply[continue-back-flomap]
@doc-apply[fast-forward-flomap]
@doc-apply[rewind-flomap]
@doc-apply[pause-flomap]{
These return typical ``playback'' icons, as @racket[pict]s.

@interaction[#:eval icons-eval
                    (for/fold ([icon (blank)])
                      ([make-flomap  (list rewind-flomap continue-back-flomap
                                              step-back-flomap back-flomap
                                              pause-flomap stop-flomap
                                              go-flomap step-flomap
                                              continue-flomap fast-forward-flomap
                                              record-flomap)])
                      (hc-append icon (make-flomap 'black 32 'shiny) (blank 12)))]
}

@doc-apply[up-arrow-flomap]{ Corresponds to @racket[up-arrow-icon]. }
@doc-apply[down-arrow-flomap]{ Corresponds to @racket[down-arrow-icon]. }
@doc-apply[left-arrow-flomap]{ Corresponds to @racket[left-arrow-icon]. }
@doc-apply[right-arrow-flomap]{ Corresponds to @racket[right-arrow-icon]. }

@doc-apply[stop-sign-flomap]{ Corresponds to @racket[stop-sign-icon]. }
@doc-apply[check-flomap]{ Corresponds to @racket[check-icon]. }
@doc-apply[x-flomap]{ Corresponds to @racket[x-icon]. }
@doc-apply[magnifying-glass-flomap]{ Corresponds to @racket[magnifying-glass-icon]. }
@doc-apply[magnifying-glass-left-flomap]{ Corresponds to @racket[magnifying-glass-left-icon]. }
@doc-apply[disk-flomap]{ Corresponds to @racket[disk-icon]. }
@doc-apply[earth-flomap]{ Corresponds to @racket[earth-icon]. }
@doc-apply[moon-flomap]{ Corresponds to @racket[moon-icon]. }
@doc-apply[hash-quote-flomap]{ Corresponds to @racket[hash-quote-icon]. }
@doc-apply[plus-flomap]{ Corresponds to @racket[plus-icon]. }
@doc-apply[times-flomap]{ Corresponds to @racket[times-icon]. }
@doc-apply[plt-logo-pict]{ Corresponds to @racket[plt-logo]. }
@doc-apply[planet-logo-pict]{ Corresponds to @racket[planet-logo]. }

}