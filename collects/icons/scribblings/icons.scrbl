#lang scribble/manual

@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label icons
                     slideshow/pict
                     mrlib/switchable-button
                     racket)
          icons/private/doc
          icons
          slideshow/pict)

@(define (author-email) "neil.toronto@gmail.com")

@title{Icons}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[icons]

@(define icon-eval (make-base-eval))
@interaction-eval[#:eval icon-eval (require icons)]


@section{Introduction (What is an icon, really?)}
@margin-note*{This introduction describes an ideal, not necessarily the current state of things.}

An icon is just a bitmap with an alpha channel, but most icons are not simply loaded from disk.
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
The height of Racket toolbar icons.

As much as possible, please use @racket[(toolbar-icon-height)] as the @racket[height] argument when loading common icons that will be used in DrRacket toolbars and buttons, or in the toolbars and buttons of DrRacket tools.

(When making an icon for DrRacket's main toolbar, please try to keep it nearly square so that it will not take up too much horizontal space when the toolbar is docked vertically.
If you cannot, as with the Macro Stepper, send a thinner icon as the @racket[alternate-bitmap] argument to a @racket[switchable-button%].)
}

@doc-apply[racket-icon-style]{
The style of Racket icons. Its value is the default style argument for the functions in @secref["common-icons"].

If you use @racket[load-icon] to load icons in a DrRacket tool, to keep the tool's icons visually consistent with DrRacket's, please format the file name using @racket[format-icon-name], passing @racket[(racket-icon-style)] as the style.
}

@section{Loading Icons}

@doc-apply[load-icon]

Loads an icon with the given @racket[name] from the given @racket[category].

Before using this general loading function, check @secref["common-icons"] for a function that loads the specific icon you need.

The icon is looked up in a cache of colorized SVG source files rendered as PNGs, and then resized to be @racket[height] pixels tall.
Icon sizes are given as heights to make it easier to append them horizontally.

In the following example, applying @racket[load-icon] is equivalent to @racket[(plt-logo 100 'diffuse)]:
@interaction[#:eval icon-eval (load-icon "logo" "plt-logo-diffuse" 100)]

(In the interactions window, you would have to send the result of applying @racket[load-icon] to @racket[icon->pict] to see it.
The rules for printing images are different in documentation.)

@doc-apply[icon-categories]{
Returns a list of all the icon categories.

@examples[#:eval icon-eval (icon-categories)]
}

@doc-apply[icon-names]{
Returns a list of all the names of icons in a given @racket[category].

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
                 (list (magnifying-glass-icon #f 32 'shiny)
                       (magnifying-glass-icon 'orange 32 'diffuse)
                       (magnifying-glass-icon 'green 32 'diffuse))]
}

@doc-apply[magnifying-glass-left-icon]{
@examples[#:eval icon-eval
                 (list (magnifying-glass-left-icon #f 32 'shiny)
                       (magnifying-glass-left-icon 'red 32 'diffuse))]
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

@doc-apply[icon-colors]
@doc-apply[icon-color/c]

@doc-apply[icon-styles]
@doc-apply[icon-style/c]


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

The conversion from @racket[pict]s to icons can be lossy:
@interaction[#:eval icon-eval
                    (scale (text "Hello" null 10) 5)
                    (scale (icon->pict (pict->icon (text "Hello" null 10))) 5)]

Therefore, when composing icons from parts, work only with @racket[pict]s, and convert to an icon as the last step.
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


@section{Making New SVG Icons}

This section is intended for Racket developers.

In the future, this section will tell where to install SVG sources and how to render them so they can be used as icons.
