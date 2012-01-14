#lang scribble/manual

@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label images/icons/arrow
                     images/icons/control
                     images/icons/file
                     images/icons/misc
                     images/icons/stickman
                     mrlib/switchable-button
                     racket
                     racket/draw)
          images/icons/arrow
          images/icons/control
          images/icons/file
          images/icons/misc
          images/icons/stickman)

@(define (author-email) "neil.toronto@gmail.com")

@title{Icons}
@author{@(author+email "Neil Toronto" (author-email))}


@(define icons-eval (make-base-eval))
@interaction-eval[#:eval icons-eval (require racket/class racket/draw racket/math racket/list
                                             images/icons/style)]

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

@interaction[#:eval icons-eval
                    (require slideshow/pict)
                    (cc-superimpose (bitmap (record-icon "forestgreen" 96 glass-icon-material))
                                    (bitmap (step-icon "azure" 48 plastic-icon-material)))]

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
@examples[#:eval icons-eval
                 (list (right-arrow-icon syntax-icon-color (toolbar-icon-height))
                       (left-arrow-icon run-icon-color)
                       (up-arrow-icon halt-icon-color 37)
                       (down-arrow-icon "lightblue" 44 glass-icon-material))]
}

@doc-apply[right-over-arrow-icon]
@doc-apply[left-over-arrow-icon]
@doc-apply[right-under-arrow-icon]
@doc-apply[left-under-arrow-icon]{
@examples[#:eval icons-eval
                 (list (right-over-arrow-icon metal-icon-color (toolbar-icon-height))
                       (left-over-arrow-icon dark-metal-icon-color)
                       (right-under-arrow-icon run-icon-color 37)
                       (left-under-arrow-icon "lightgreen" 44 glass-icon-material))]
}

@section[#:tag "control"]{Control Icons}

@defmodule[images/icons/control]
@interaction-eval[#:eval icons-eval (require images/icons/control)]

@doc-apply[play-icon]
@doc-apply[back-icon]
@doc-apply[fast-forward-icon]
@doc-apply[rewind-icon]
@doc-apply[bar-icon]
@doc-apply[stop-icon]
@doc-apply[record-icon]
@doc-apply[pause-icon]
@doc-apply[step-icon]
@doc-apply[step-back-icon]
@doc-apply[continue-icon]
@doc-apply[continue-back-icon]{
Typical ``playback control'' icons.
For example, a colorful tape deck:
@interaction[#:eval icons-eval
                    (for/list ([make-icon  (list rewind-icon continue-back-icon
                                                 step-back-icon back-icon
                                                 pause-icon stop-icon
                                                 play-icon step-icon
                                                 continue-icon fast-forward-icon
                                                 record-icon)]
                               [color  (list run-icon-color halt-icon-color
                                             syntax-icon-color metal-icon-color
                                             dark-metal-icon-color dark-metal-icon-color
                                             metal-icon-color syntax-icon-color
                                             halt-icon-color run-icon-color
                                             "red")]
                               [material  (in-cycle (list plastic-icon-material
                                                          glass-icon-material))])
                      (make-icon color 32 material))]
The remaining icon @(bar-icon "red" 16), returned by @racket[bar-icon], is used to build the others.
}

@section[#:tag "file"]{File Icons}

@defmodule[images/icons/file]
@interaction-eval[#:eval icons-eval (require images/icons/file)]

@doc-apply[floppy-disk-icon]{
@examples[#:eval icons-eval (floppy-disk-icon "gold" 32 glass-icon-material)]
}

@doc-apply[save-icon]
@doc-apply[small-save-icon]
@doc-apply[load-icon]
@doc-apply[small-load-icon]{
@examples[#:eval icons-eval
                 (for/list ([make-icon  (list save-icon small-save-icon
                                              load-icon small-load-icon)]
                            [color  (list run-icon-color halt-icon-color
                                          metal-icon-color dark-metal-icon-color)])
                   (make-icon syntax-icon-color color 32))]
}

@section[#:tag "misc"]{Miscellaneous Icons}

@defmodule[images/icons/misc]
@interaction-eval[#:eval icons-eval (require images/icons/misc)]

@doc-apply[text-icon]{
Renders a text string as an icon. For example,
@interaction[#:eval icons-eval
                    (text-icon "An Important Point!"
                               (make-object font% 48 'decorative 'normal 'bold #t)
                               "lightskyblue" #t 2 48)]

Before rendering, the drawn text is scaled so that it is exactly @racket[height] pixels tall.
Make sure the font is large enough that scaling does not create blurry and jagged edge artifacts, as in the following example:
@interaction[#:eval icons-eval
                    (text-icon "Q" (make-object font% 32 'default 'normal 'bold)
                               "green" #t 0 96)]
When @racket[str] contains tall letters or @racket[trim?] is @racket[#f], using @racket[height] as the font size should be sufficient.

To make it easy to create a large enough font, @racket[text-icon] always interpets font sizes as being in pixels, never points.
See @racket[font%] for details on font sizes.

If @racket[trim?] is @racket[#f], the drawn text is not cropped before rendering.
Otherwise, it is cropped to the smallest rectangle containing all the non-zero-alpha pixels.
Rendering very small glyphs shows the difference dramatically:
@interaction[#:eval icons-eval
                    (define font (make-object font% 32 'default))
                    (list (text-icon "." font "white")
                          (text-icon "." font "white" #f))]
Note that both icons are @racket[(default-icon-height)] pixels tall.

When @racket[outline] is @racket['auto], the outline drawn around the text is @racket[(/ height 32)] pixels wide.

Because different platforms have slightly different fonts, @racket[text-icon] cannot guarantee the icons it returns have a consistent look or width across all platforms.
}

@doc-apply[recycle-icon]{
Returns the universal recycling symbol, rendered as an icon.
Its implementation calls @racket[text-icon] with the string @racket["\u267b"].
@examples[#:eval icons-eval (recycle-icon "forestgreen" 48)]
}

@doc-apply[x-icon]{
Returns an ``x'' icon that is guaranteed to look the same on all platforms.
(Anything similar that would be constructed by @racket[text-icon] would differ at least slightly across platforms.)
@examples[#:eval icons-eval (x-icon "red" 32)]
}

@doc-apply[check-icon]{
@examples[#:eval icons-eval (check-icon "darkgreen" 32)]
}

@doc-apply[regular-polygon-icon]{
Renders the largest regular polygon with @racket[sides] sides, with the first vertex at angle @racket[start], that can be centered in a @racket[height] × @racket[height] box.
@examples[#:eval icons-eval (for/list ([sides  (in-range 1 9)]
                                       [material  (in-cycle (list plastic-icon-material
                                                                  glass-icon-material))])
                              (regular-polygon-icon sides (* 1/4 pi) "cornflowerblue" 32
                                                    material))]
}

@doc-apply[octagon-icon]{
Equivalent to @racket[(regular-polygon-icon 8 (/ (* 2 pi) 16) color height material)].
@examples[#:eval icons-eval (octagon-icon halt-icon-color 32)]
}

@doc-apply[stop-sign-icon]{
@examples[#:eval icons-eval
                 (stop-sign-icon halt-icon-color 32 glass-icon-material)]
}

@doc-apply[stop-signs-icon]{
@examples[#:eval icons-eval
                 (stop-signs-icon halt-icon-color 32 plastic-icon-material)]
}

@doc-apply[magnifying-glass-icon]{
@examples[#:eval icons-eval
                 (magnifying-glass-icon "azure" "lightblue" 32 glass-icon-material)]
}

@doc-apply[left-magnifying-glass-icon]{
@examples[#:eval icons-eval
                 (left-magnifying-glass-icon metal-icon-color "red" 32)]
}

@doc-apply[bomb-icon]{
@examples[#:eval icons-eval
                 (bomb-icon "azure" "black" 32 glass-icon-material)]
}

@doc-apply[left-bomb-icon]{
@examples[#:eval icons-eval
                 (left-bomb-icon metal-icon-color dark-metal-icon-color 32)]
}

@section[#:tag "stickman"]{Stickman Icons}

@defmodule[images/icons/stickman]
@interaction-eval[#:eval icons-eval (require images/icons/stickman)]

@doc-apply[standing-stickman-icon]{
Returns the icon displayed in DrRacket's lower-right corner when no program is running.
@examples[#:eval icons-eval (standing-stickman-icon run-icon-color "white" run-icon-color 64)]
}

@doc-apply[running-stickman-icon]{
Returns a frame of the icon animated in DrRacket's lower-right corner when a program is running.
The frame returned is for time @racket[t] of a run cycle with a one-second period.

It is difficult to put a code example in the API documentation that produces an animation.
However, we might use code similar to the following to sample from the run cycle:
@interaction[#:eval icons-eval
                    (for/list ([t  (in-range 0 1 1/12)])
                      (running-stickman-icon t run-icon-color "white" run-icon-color 32))]
If instead of putting the icons in a list, we call their @racket[save-file] methods and hand-assemble the files into a GIF, we get something like this:

@centered[@image["scribblings/running-stickman.gif"]]

Here, the run cycle is sampled and played back at 30 Hz.
The previous example samples the run cycle at 12 Hz, or every @racket[1/12] second.
DrRacket samples it at 12 Hz and plays it back at 5 Hz at the most.

The stickman's joint angles are defined by continuous periodic functions, so the run cycle can be sampled at any resolution, or at any real-valued time @racket[t].
The cycle is modeled after the run cycle of the player's avatar in the Commodore 64 game @link["http://en.wikipedia.org/wiki/Impossible_Mission"]{Impossible Mission}.
}

@section[#:tag "tool"]{Tool Icons}

@section[#:tag "const"]{Icon Constants and Contracts}
