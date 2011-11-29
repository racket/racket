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

@(define (hash-quote) (load-icon "misc" (format-icon-name "hash-quote" #f 'diffuse) 14))
@(define (step) (step-icon 'blue 14 'diffuse))

Example: The Macro Stepper tool composes a new icon @(hash-quote) and an existing icon @(step), resulting in @(macro-stepper-icon 14 'diffuse) for its toolbar icon.
The @(hash-quote) icon connotes syntax, and is the color of a syntax-quote as rendered by DrRacket by default.
The @(step) icon is colored like DrRacket colors identifier syntax by default, and is shaped using metaphors used in debugger toolbars, TV remotes, and music players around the world.
It is composed of @(go-icon 'blue 14 'diffuse) to connote starting and @(bar-icon 'blue 14 'diffuse) to connote immediately stopping---a ``step.''

The author of this collection is available to adapt or create SVG icons for DrRacket tools, and charges no more than your immortal soul.

@section{Icon Parameters}

@doc-apply[toolbar-icon-height]{
The height of DrRacket toolbar icons.

Use @racket[(toolbar-icon-height)] as the @racket[height] argument when loading common icons that will be used in DrRacket toolbars and buttons, or in the toolbars and buttons of DrRacket tools.

(When making an icon for DrRacket's main toolbar, try to keep it nearly square so that it will not take up too much horizontal space when the toolbar is docked vertically.
If you cannot, as with the Macro Stepper, send a thinner icon as the @racket[alternate-bitmap] argument to a @racket[switchable-button%].)
}

@doc-apply[default-icon-style]{
The style of DrRacket icons. Its value is the default style argument for the functions in @secref["common-icons"].

If you use @racket[load-icon] to load icons in a DrRacket tool, to keep the tool's icons visually consistent with DrRacket's, format the file name using @racket[format-icon-name] without supplying a @racket[style] argument.
}

@section{Loading Icons}

@doc-apply[load-icon]

Loads an icon with the given @racket[name] from the given @racket[category].

Before using this general loading function, check @secref["common-icons"] for a function that loads the specific icon you need.

The icon is looked up in a cache of colorized SVG source files rendered as PNGs, and then resized to be @racket[height] pixels tall.
Icon sizes are given as heights to make it easier to append them horizontally.

In the following example, applying @racket[load-icon] is equivalent to @racket[(plt-logo 100 'diffuse)]:
@interaction[#:eval icon-eval (load-icon "logo" "plt-logo-diffuse" 100)]

(In the interactions window, you would have to send the result of applying @racket[load-icon] to @racket[icon->pict] to see it.)

@doc-apply[icon-categories]{
Returns a list of all the icon categories.

@examples[#:eval icon-eval (icon-categories)]
}

@doc-apply[icon-names]{
Returns a list of all the names of icons in the given @racket[category].

@examples[#:eval icon-eval (icon-names "logo")]
}

@doc-apply[format-icon-name]

Formats an icon file name.

@examples[#:eval icon-eval
                 (format-icon-name "go" 'red 'diffuse)
                 (format-icon-name "go" #f 'diffuse)
                 (format-icon-name "go" 'red #f)
                 (format-icon-name "go" #f #f)]

The functions in @secref["common-icons"] use this to turn their arguments into file names.

@section[#:tag "common-icons"]{Common Icon Constructors}

@subsection{Playback Control Icons}

@doc-apply[go-icon]
@doc-apply[bar-icon]
@doc-apply[back-icon]
@doc-apply[stop-icon]
@doc-apply[step-icon]
@doc-apply[step-back-icon]
@doc-apply[continue-icon]
@doc-apply[continue-back-icon]
@doc-apply[fast-forward-icon]
@doc-apply[rewind-icon]
@doc-apply[pause-icon]{
These return typical ``playback control'' icons.

@interaction[#:eval icon-eval 
                    (for*/list ([color  '(blue orange)]
                                [style  icon-styles])
                      (for/list ([make-icon  (list rewind-icon continue-back-icon
                                                   step-back-icon back-icon
                                                   pause-icon stop-icon
                                                   go-icon step-icon
                                                   continue-icon fast-forward-icon)])
                        (make-icon color 32 style)))]

The remaining icon @(bar-icon 'red 14 'diffuse), returned by @racket[bar-icon], is not a playback icon @italic{per se}, but is used to build the others.
}

@doc-apply[stop-sign-icon]{
@examples[#:eval icon-eval (map (λ (color) (stop-sign-icon color 23 'diffuse)) icon-colors)]
}

@doc-apply[check-icon]{
@examples[#:eval icon-eval (map (λ (color) (check-icon color 29 'shiny)) icon-colors)]
}

@doc-apply[magnifying-glass-icon]{
@examples[#:eval icon-eval
                 (map (λ (color) (magnifying-glass-icon color 31 'diffuse))
                      icon-colors)]
Note that the uncolorized magnifying glass has a brown handle.
}

@doc-apply[magnifying-glass-left-icon]{
@examples[#:eval icon-eval
                 (map (λ (color) (magnifying-glass-left-icon color 31 'shiny))
                      icon-colors)]
}

@subsection{Tool Icons and Other Special Icons}

@doc-apply[stop-signs-icon]{
@examples[#:eval icon-eval (stop-signs-icon 24 'diffuse)]
}

@doc-apply[macro-stepper-icon]{
@examples[#:eval icon-eval (macro-stepper-icon (toolbar-icon-height) 'diffuse)]
}

@doc-apply[check-syntax-icon]
@doc-apply[check-syntax-small-icon]{
@examples[#:eval icon-eval (list (check-syntax-icon (toolbar-icon-height) 'diffuse)
                                 (check-syntax-small-icon (toolbar-icon-height) 'diffuse))]
}

@doc-apply[plt-logo]{
@examples[#:eval icon-eval (plt-logo 256 'shiny)]
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

It is more flexible, but a little more complicated, to load icons as @racket[pict]s.
As picts, icons can easily be appended, inset, superimposed, blurred, and more.

To use these functions effectively, you should require @racketmodname[icons] and @racketmodname[slideshow/pict] together.

Almost all of the functions in preceeding sections are defined in terms of the functions documented in this section.

@interaction-eval[#:eval icon-eval (require slideshow/pict)]

@doc-apply[load-icon-pict]{
Corresponds to @racket[load-icon]. In fact, @racket[load-icon] uses @racket[load-icon-pict] to load the icon as a @racket[pict], and passes it to @racket[pict->icon].
}

@doc-apply[icon->pict]
@doc-apply[pict->icon]{
Convert from an icon to a @racket[pict], and back.

The conversion from @racket[pict]s to icons can be lossy because it renders vector graphics as a bitmap. For example, converting text can look especially horrible:
@interaction[#:eval icon-eval
                    (scale (text "Hello" null 10) 5)
                    (scale (icon->pict (pict->icon (text "Hello" null 10))) 5)]

Therefore, when composing icons from parts, work only with @racket[pict]s, and convert to an icon as the last step.

On the other hand, @racket[(compose pict->icon icon->pict)] always returns an equivalent icon:
@interaction[#:eval icon-eval
                    (define not-blurry (magnifying-glass-icon 'green 64 'shiny))
                    not-blurry
                    (define still-not-blurry
                      (for/fold ([icon not-blurry]) ([i  (in-range 30)])
                        (pict->icon (icon->pict icon))))
                    still-not-blurry]
}

@doc-apply[go-icon-pict]
@doc-apply[bar-icon-pict]
@doc-apply[back-icon-pict]
@doc-apply[stop-icon-pict]
@doc-apply[step-icon-pict]
@doc-apply[step-back-icon-pict]
@doc-apply[continue-icon-pict]
@doc-apply[continue-back-icon-pict]
@doc-apply[fast-forward-icon-pict]
@doc-apply[rewind-icon-pict]
@doc-apply[pause-icon-pict]{
These return typical ``playback control'' icons, as @racket[pict]s.

@interaction[#:eval icon-eval
                    (for/fold ([icon (blank)])
                      ([make-icon-pict  (list rewind-icon-pict continue-back-icon-pict
                                              step-back-icon-pict back-icon-pict
                                              pause-icon-pict stop-icon-pict
                                              go-icon-pict step-icon-pict
                                              continue-icon-pict fast-forward-icon-pict)])
                      (hc-append icon (make-icon-pict 'black 32 'shiny) (blank 16)))]
}

@doc-apply[stop-sign-icon-pict]{ Corresponds to @racket[stop-sign-icon]. }
@doc-apply[check-icon-pict]{ Corresponds to @racket[check-icon]. }
@doc-apply[magnifying-glass-icon-pict]{ Corresponds to @racket[magnifying-glass-icon]. }
@doc-apply[magnifying-glass-left-icon-pict]{ Corresponds to @racket[magnifying-glass-left-icon]. }

@doc-apply[stop-signs-icon-pict]{ Corresponds to @racket[stop-signs-icon]. }
@doc-apply[macro-stepper-icon-pict]{ Corresponds to @racket[macro-stepper-icon]. }
@doc-apply[check-syntax-icon-pict]{ Corresponds to @racket[check-syntax-icon]. }
@doc-apply[check-syntax-small-icon-pict]{ Corresponds to @racket[check-syntax-small-icon]. }
@doc-apply[plt-logo-pict]{ Corresponds to @racket[plt-logo]. }


@section[#:tag "new-icons"]{Making New SVG Icons}

This section is intended for Racket developers, who have permission to add SVG icon sources to the repository.

Take the following steps to add an SVG icon to Racket's repository.

@bold{1. Create an SVG file.}
The author of this module uses @link["http://inkscape.org"]{Inkscape}, a free software vector graphics program.
Another option is of course @link["http://www.adobe.com/illustrator"]{Adobe Illustrator}.

If the icon (or part of it) can be generated by a mathematical function, use Racket's @racketmodname[plot] library.
Specifically, use @racket[plot] or @racket[plot3d], passing an @racket[#:out-file] argument with a @racket[".svg"] file name extension.
Use @racket[plot-file] or @racket[plot3d-file] in scripts.
Set @racket[plot-decorations?] to @racket[#f] and @racket[plot-background-alpha] to @racket[0].

If the icon (or part of it) can be otherwise generated programmatically, use Racket to draw on an @racket[svg-dc%], or use @link["http://www.imagemagick.org"]{ImageMagick} and its MVG or MagickWand languages.

Use @tt{collects/icons/private/svg/run/stop-diffuse.svg} and @tt{collects/icons/private/svg/run/stop-shiny.svg} as simple examples of icons in diffuse and shiny styles.
Note that the light source is apparently above and slightly to the left.

@bold{2. Make SVG gradients colorizable.}
Do this by editing the SVG file to change the names of gradients.
It cannot currently be done in Inkscape, but a text editor's find-and-replace-all works perfectly.

If part of the icon should be colorizable, find the gradient for that part and change its name to @tt{diffuseColorGradient}.
Make sure the first stop is a dark version of the default color and the second stop is the default color.
For specular highlights, do not use @tt{diffuseColorGradient}, but overlay the image with partially transparent gradients.

For shiny icons, change undershine gradients (the bright spots opposite the light source that make icons look like candy or transparent plastic) to have the name @tt{undershineGradient}.
Make sure the first stop is fully opaque with a hue close to the default color's, and the second stop is fully transparent.

@bold{3. Place the SVG file in the @racketmodname[icons] collection.}
Put it in the directory @tt{collects/icons/private/svg/<category>} where @tt{<category>} is a category appropriate for the icon.

Feel free to make new category directories or even subcategories (by making subdirectories).
For example, to make a subcategory of @racket["run"] called @racket["silly"], create a subdirectory @tt{silly} in the @tt{run} directory.
The icon can then be loaded from category @racket["run/silly"].

If the icon has diffuse and shiny versions, name them @tt{<name>-diffuse.svg} and @tt{<name>-shiny.svg}.

@bold{4. Re-render PNGs.}
Install Inkscape, then run the program @tt{collects/icons/private/svg/render-png.rkt}.
This will delete every PNG rendering, and re-render every SVG source file in @tt{collects/icons/private/svg}.

Renderings are done at specific power-of-two heights.
For non-logo icons, the heights are currently 16, 32 and 64.
Logos are rendered at 16, 32, 64, 128, 256 and 512.

Uncolorized renders are put in a subdirectory named after their size.
For example, a render of @tt{run/silly/walks-diffuse.svg} at height 64 will have the name @tt{run/silly/64/walks-diffuse.png}.

Colorized renders are put in a further subdirectory named after their color.
For example, a red render of @tt{run/silly/walks-diffuse.svg} at height 64 will have the name @tt{run/silly/64/red/walks-diffuse.png}.

@bold{5. Load the icon.}
For example, to load a red render of @tt{walks-diffuse.svg} at height 50, do

@racketblock[(load-icon-pict "run/silly"
                             (format-icon-name "walks" 'red 'diffuse)
                             50)]

For this, @racket[format-icon-name] returns @racket["red/walks-diffuse"].
Then @racket[load-icon-pict] finds the first rendered height not less than @racket[50], which is @racket[64], loads a bitmap from @racket["run/silly/64/red/walks-diffuse.png"], converts it to a @racket[pict], and scales it by @racket[(/ 50 64)].

For convenience, write functions to load the icon; for example,
@racketblock[
(define (silly-walk-icon-pict color height [style (default-icon-style)])
  (load-icon-pict "run/silly"
                  (format-icon-name "walk" color style)
                  height))
                                                                            
(define (silly-walk-icon color height [style (default-icon-style)])
  (pict->icon (silly-walk-icon-pict color height style)))
]
