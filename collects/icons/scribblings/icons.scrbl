#lang scribble/manual

@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label icons
                     slideshow/pict
                     mrlib/switchable-button
                     racket
                     plot
                     racket/draw)
          icons/private/doc
          icons
          slideshow/pict)

@(define (author-email) "neil.toronto@gmail.com")

@title{Icons}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[icons]

@(define icon-eval (make-base-eval))
@interaction-eval[#:eval icon-eval (require icons racket/math racket/list)]


@section{Introduction (What is an icon, really?)}
@margin-note*{This introduction describes an ideal, not necessarily the current state of things.}

As a first approximation, an icon is just a small bitmap with an alpha channel. But most icons are not simply loaded from disk.
Icons can also be rendered from colorized SVG sources, resized, generated programmatically by drawing on a @racket[dc<%>], or composed using @racket[pict] functions.

The @racketmodname[icons] module is intended to make it possible to do all of these things, and to make it easy to get common icons in different colors, heights and styles.

An icon communicates. Its shape and color are a visual metaphor for an action or a message. Icons should be easily recognizable, distinguishable, visually consistent, and metaphorically appropriate for the actions and messages they are used with. This is difficult to do well, but good examples, good abstractions, and an existing icon library help considerably.

@(define (hash-quote) (hash-quote-icon-pict 16))
@(define (step) (step-icon-pict 'blue 16 'diffuse))
@(define (macro-stepper) (ht-append (hash-quote) (step)))
   
Example: The Macro Stepper tool composes a new icon @(hash-quote) and an existing icon @(step), resulting in @(macro-stepper) for its toolbar icon.
The @(hash-quote) icon connotes syntax, and is the color of a syntax-quote as rendered by DrRacket by default.
The @(step) icon is colored like DrRacket colors identifier syntax by default, and is shaped using metaphors used in debugger toolbars, TV remotes, and music players around the world.
It is composed of @(go-icon 'blue 16 'diffuse) to connote starting and @(bar-icon 'blue 16 'diffuse) to connote immediately stopping---a ``step.''

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

@section{Loading Icons}

@doc-apply[load-icon]

Loads an icon with the given @racket[name] from the given @racket[category].

Before using this general loading function, check @secref["common-icons"] for a function that loads the specific icon you need.

The icon is looked up in a cache of colorized SVG source files rendered as PNGs, and then resized to be @racket[height] pixels tall.
Icon sizes are given as heights to make it easier to append them horizontally.

The following is equivalent to @racket[(plt-logo 100 'diffuse)]:
@interaction[#:eval icon-eval (load-icon "logo" "plt" #f 100 'diffuse)]

@doc-apply[icon-categories]{
Returns a list of all the icon categories.

@examples[#:eval icon-eval (icon-categories)]
}

@doc-apply[icon-names]{
Returns a list of all the names of icons in the given @racket[category].

@examples[#:eval icon-eval (icon-names "logo")]
}


@section[#:tag "common-icons"]{Common Icon Constructors}

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

@interaction[#:eval icon-eval 
                    (for/list ([make-icon  (list rewind-icon continue-back-icon
                                                 step-back-icon back-icon
                                                 pause-icon stop-icon
                                                 go-icon step-icon
                                                 continue-icon fast-forward-icon
                                                 record-icon)]
                               [color  (in-cycle icon-colors)]
                               [style  (in-cycle icon-styles)])
                      (make-icon color 32 style))]

The remaining icon @(bar-icon #f 16), returned by @racket[bar-icon], is used to build the others.
}

@subsection{Arrow Icons}

@doc-apply[up-arrow-icon]
@doc-apply[down-arrow-icon]
@doc-apply[left-arrow-icon]
@doc-apply[right-arrow-icon]{
@examples[#:eval icon-eval
                 (for/list ([make-icon  (list up-arrow-icon down-arrow-icon
                                              left-arrow-icon right-arrow-icon)])
                   (for/list ([color  icon-colors]
                              [style  (in-cycle icon-styles)])
                     (make-icon color (default-icon-height) style)))]
}

@subsection{Sign Icons}

@doc-apply[stop-sign-icon]{
@examples[#:eval icon-eval (list (stop-sign-icon (default-icon-height) 'diffuse)
                                 (stop-sign-icon (default-icon-height) 'shiny))]
}

@doc-apply[stop-signs-icon]{
@examples[#:eval icon-eval (list (stop-signs-icon (default-icon-height) 'diffuse)
                                 (stop-signs-icon (default-icon-height) 'shiny))]
}

@subsection{Check Icons}

@doc-apply[check-icon]{
@examples[#:eval icon-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (check-icon color 29 style))]
}

@doc-apply[x-icon]{
@examples[#:eval icon-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (x-icon color 29 style))]
}

@subsection{Miscellaneous Icons}

@doc-apply[magnifying-glass-icon]{
@examples[#:eval icon-eval (list (magnifying-glass-icon 31 'diffuse)
                                 (magnifying-glass-icon 31 'shiny))]
Note that the uncolorized magnifying glass has a brown handle.
}

@doc-apply[magnifying-glass-left-icon]{
@examples[#:eval icon-eval (list (magnifying-glass-left-icon 31 'diffuse)
                                 (magnifying-glass-left-icon 31 'shiny))]
}

@doc-apply[disk-icon]{
@examples[#:eval icon-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (disk-icon color 33 style))]
}

@doc-apply[earth-icon]{
@examples[#:eval icon-eval (list (earth-icon 48 'diffuse)
                                 (earth-icon 48 'shiny))]
}

@doc-apply[moon-icon]{
@examples[#:eval icon-eval (list (moon-icon 48 'diffuse)
                                 (moon-icon 48 'shiny))]
}

@subsection{Symbols}

@doc-apply[hash-quote-icon]{
@examples[#:eval icon-eval (list (hash-quote-icon (toolbar-icon-height) 'diffuse)
                                 (hash-quote-icon (toolbar-icon-height) 'shiny))]
}

@doc-apply[plus-icon]{
@examples[#:eval icon-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (plus-icon color 24 style))]
}

@doc-apply[times-icon]{
@examples[#:eval icon-eval
                 (for/list ([color  icon-colors]
                            [style  (in-cycle icon-styles)])
                   (times-icon color 24 style))]
}

@subsection{Logos}

@doc-apply[plt-logo]{
@examples[#:eval icon-eval
                 (list (plt-logo 128 'diffuse) (plt-logo 128 'shiny))]
}

@doc-apply[planet-logo]{
@examples[#:eval icon-eval (list (planet-logo 128 'diffuse)
                                 (planet-logo 128 'shiny))]
}


@section{Icon Constants and Contracts}

@doc-apply[icon-colors]{
A list containing the names of allowed icon colors.

When an SVG icon source file is rendered, it is rendered once directly. Then, for each color corresponding to a symbol in @racket[icon-colors], it is colorized by replacing gradients, and then rendered.

When loading an icon, a @racket[#f] color name loads an uncolorized rendering.
Every icon can be loaded with a @racket[#f] color name.
An icon can be loaded using any name in @racket[icon-colors] only if its SVG source has gradients that can be colorized.
See @secref["new-icons"] for details.

The actual hues associated with the color names are the hues of the first seven @racketmodname[plot] color numbers.
The following example illustrates the correspondence:
@interaction[#:eval icon-eval
                    (require plot)
                    (for/list ([color  (rest icon-colors)])
                      (stop-icon-pict color 48))
                    (parameterize ([plot-width 48]
                                   [plot-height 48]
                                   [plot-decorations? #f]
                                   [plot-background-alpha 0])
                      (for/list ([n  (in-range 7)])
                        (plot3d-pict (surface3d (λ (x y) (- (sqr x) (sqr y))) -1 1 -1 1
                                                #:color n #:line-color n
                                                #:samples 11 #:line-width 1))))]
This example also shows how to use @racketmodname[plot] to create icon @racket[pict]s from mathematical functions.
}

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

@interaction-eval[#:eval icon-eval (require slideshow/pict)]

It is more flexible, but a little more complicated, to load icons as @racket[pict]s.
As picts, icons can easily be appended, inset, superimposed, blurred, and more.
For example, it is easy to make modern-looking media player controls using @racket[cc-superimpose] and the @racket['shiny] style:
@interaction[#:eval icon-eval
                    (define media-icon-background (record-icon-pict 'green 64 'shiny))
                    (list (cc-superimpose media-icon-background
                                          (step-back-icon-pict 'white 32 'shiny))
                          (cc-superimpose media-icon-background
                                          (pause-icon-pict 'white 32 'shiny))
                          (cc-superimpose media-icon-background
                                          (step-icon-pict 'white 32 'shiny)))]

Almost all of the functions in preceeding sections are defined in terms of the @racket[pict]-producing functions documented in this section.

To use these functions effectively, you should require @racketmodname[icons] and @racketmodname[slideshow/pict] together.
Use @racket[bitmap] to convert a @racket[bitmap%] (e.g. an icon) to a @racket[pict], and @racket[pict->bitmap] to convert back.

Converting from @racket[pict]s to bitmaps can be lossy. For example, converting text can look especially horrible:
@interaction[#:eval icon-eval
                    (scale (text "Hello" null 10) 5)
                    (scale (bitmap (pict->bitmap (text "Hello" null 10))) 5)]

Therefore, when composing icons from parts, try to work only with @racket[pict]s, and convert to an icon using @racket[pict->bitmap] as the last step.

When composing icons from parts, it is fine to use @racket[pict]s converted from @racket[bitmap%]s.
Without scaling or rotating, the conversion is lossless:
@interaction[#:eval icon-eval
                    (define not-blurry (magnifying-glass-icon 64 'shiny))
                    not-blurry
                    (for/fold ([icon not-blurry]) ([i  (in-range 30)])
                      (pict->bitmap (bitmap icon)))]

Avoid converting between @racket[pict]s and @racket[bitmap%]s more than once if bitmap-backed @racket[pict]s are scaled, rotated by angles that are not multiples of 90 degrees, or superimposed or appended at non-integer coordinates.
Avoid scaling up in general.

@doc-apply[load-icon-pict]{
Corresponds to @racket[load-icon]. In fact, @racket[load-icon] uses @racket[load-icon-pict] to load the icon as a @racket[pict], and passes it to @racket[pict->bitmap].
}

@doc-apply[go-icon-pict]
@doc-apply[bar-icon-pict]
@doc-apply[back-icon-pict]
@doc-apply[stop-icon-pict]
@doc-apply[record-icon-pict]
@doc-apply[step-icon-pict]
@doc-apply[step-back-icon-pict]
@doc-apply[continue-icon-pict]
@doc-apply[continue-back-icon-pict]
@doc-apply[fast-forward-icon-pict]
@doc-apply[rewind-icon-pict]
@doc-apply[pause-icon-pict]{
These return typical ``playback'' icons, as @racket[pict]s.

@interaction[#:eval icon-eval
                    (for/fold ([icon (blank)])
                      ([make-icon-pict  (list rewind-icon-pict continue-back-icon-pict
                                              step-back-icon-pict back-icon-pict
                                              pause-icon-pict stop-icon-pict
                                              go-icon-pict step-icon-pict
                                              continue-icon-pict fast-forward-icon-pict
                                              record-icon-pict)])
                      (hc-append icon (make-icon-pict 'black 32 'shiny) (blank 12)))]
}

@doc-apply[up-arrow-icon-pict]{ Corresponds to @racket[up-arrow-icon]. }
@doc-apply[down-arrow-icon-pict]{ Corresponds to @racket[down-arrow-icon]. }
@doc-apply[left-arrow-icon-pict]{ Corresponds to @racket[left-arrow-icon]. }
@doc-apply[right-arrow-icon-pict]{ Corresponds to @racket[right-arrow-icon]. }

@doc-apply[stop-sign-icon-pict]{ Corresponds to @racket[stop-sign-icon]. }
@doc-apply[stop-signs-icon-pict]{ Corresponds to @racket[stop-signs-icon]. }
@doc-apply[check-icon-pict]{ Corresponds to @racket[check-icon]. }
@doc-apply[x-icon-pict]{ Corresponds to @racket[x-icon]. }
@doc-apply[magnifying-glass-icon-pict]{ Corresponds to @racket[magnifying-glass-icon]. }
@doc-apply[magnifying-glass-left-icon-pict]{ Corresponds to @racket[magnifying-glass-left-icon]. }
@doc-apply[disk-icon-pict]{ Corresponds to @racket[disk-icon]. }
@doc-apply[earth-icon-pict]{ Corresponds to @racket[earth-icon]. }
@doc-apply[moon-icon-pict]{ Corresponds to @racket[moon-icon]. }
@doc-apply[hash-quote-icon-pict]{ Corresponds to @racket[hash-quote-icon]. }
@doc-apply[plus-icon-pict]{ Corresponds to @racket[plus-icon]. }
@doc-apply[times-icon-pict]{ Corresponds to @racket[times-icon]. }
@doc-apply[plt-logo-pict]{ Corresponds to @racket[plt-logo]. }
@doc-apply[planet-logo-pict]{ Corresponds to @racket[planet-logo]. }

@section[#:tag "new-icons"]{Making New SVG Icons}

This section is intended for Racket developers, who have permission to add SVG icon sources to the repository.

Take the following steps to add an SVG icon to the @racketmodname[icons] collection.

@bold{1. Create an SVG file.}
The author of this module uses @link["http://inkscape.org"]{Inkscape}, a free software vector graphics program.
Another option is of course @link["http://www.adobe.com/illustrator"]{Adobe Illustrator}.

If the icon (or part of it) can be generated by a mathematical function, use Racket's @racketmodname[plot] library.
Specifically, use @racket[plot] or @racket[plot3d], passing an @racket[#:out-file] argument with a @racket[".svg"] file name extension.
Use @racket[plot-file] or @racket[plot3d-file] in scripts.
Set @racket[plot-decorations?] to @racket[#f] and @racket[plot-background-alpha] to @racket[0].

If the icon (or part of it) can be otherwise generated programmatically, use Racket to draw on an @racket[svg-dc%], or use @link["http://www.imagemagick.org"]{ImageMagick} and its MVG or MagickWand languages.

Use @tt{collects/icons/private/svg/control/stop-diffuse.svg} and @tt{collects/icons/private/svg/control/stop-shiny.svg} as simple examples of icons in diffuse and shiny styles.
Note that the light source is apparently above and slightly to the right.
However, to make it easy to center the icons, and to keep them looking symmetric at very small sizes, the blurry shadow is directly underneath.

@bold{2. Make SVG gradients colorizable.}
Do this by editing the SVG file to change the names of gradients.
It cannot currently be done in Inkscape, but a text editor's find-and-replace-all works perfectly.

If part of the icon should be colorizable, find the gradient for that part and change its name to @tt{diffuseColorGradient}.
Make sure the first stop is a dark version of the default color and the second stop is the default color.
For specular highlights, do not use @tt{diffuseColorGradient}.
Instead, overlay the image with partially transparent gradients.
Keeping the lighting and coloring in separate layers is good practice besides.

For shiny icons, change undershine gradients (the bright spots opposite the light source that make icons look like candy or transparent plastic) to have the name @tt{undershineGradient}.
Make sure the first stop is fully opaque with a hue close to the default color's, and the second stop is fully transparent.

@bold{3. Place the SVG file in the @racketmodname[icons] collection.}
Put it in the directory @tt{collects/icons/private/svg/<category>} where @tt{<category>} is a category appropriate for the icon.
Feel free to make new category directories to keep things organized.

If the icon has diffuse and shiny styles, name them @tt{<name>-diffuse.svg} and @tt{<name>-shiny.svg}.

@bold{4. Re-render PNGs.}
Install Inkscape, then run the program @tt{collects/icons/private/svg/render-png.rkt}.
This will render every modified SVG source file in @tt{collects/icons/private/svg}.
Be aware that it will @italic{not} delete any PNGs whose source files are deleted.

Because Cairo, which backs Racket's drawing library, draws bitmaps downscaled past half size poorly, renderings are done at power-of-two heights.
For non-logo icons, the heights are currently 16, 32 and 64.
Logos are rendered at 32, 64, 128, 256 and 512.

Uncolorized renders are put in a subdirectory named after their size.
For example, a render of @tt{silly/walks-diffuse.svg} at height 64 will have the name @tt{silly/64/walks-diffuse.png}.

Colorized renders are put in a further subdirectory named after their color.
For example, a red render of @tt{silly/walks-diffuse.svg} at height 64 will have the name @tt{silly/64/red/walks-diffuse.png}.

@bold{5. Load the icon.}
For example, to load a red render of @tt{walks-diffuse.svg} at height 50, do

@racketblock[(load-icon-pict "silly" "walks" 'red 50 'diffuse)]

For this, @racket[load-icon-pict] finds the first rendered height not less than @racket[50], which is @racket[64], loads the bitmap from @racket["silly/64/red/walks-diffuse.png"], converts it to a @racket[pict], and scales it by @racket[(/ 50 64)].

For convenience, write functions to load the icon; for example,
@racketblock[
(define (silly-walk-icon-pict color
                              [height (default-icon-height)]
                              [style (default-icon-style)])
  (load-icon-pict "silly" "walk" color height style))

(define (silly-walk-icon color
                         [height (default-icon-height)]
                         [style (default-icon-style)])
  (pict->bitmap (silly-walk-icon-pict color height style)))]
Please export them with a contract.
