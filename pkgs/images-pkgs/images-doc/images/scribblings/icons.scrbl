#lang scribble/manual
 
@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label racket racket/draw
                     images/icons/arrow
                     images/icons/control
                     images/icons/file
                     images/icons/symbol
                     images/icons/misc
                     images/icons/stickman
                     images/icons/tool
                     images/icons/style
                     images/logos
                     mrlib/switchable-button
                     pict)
          racket/class racket/draw
          images/icons/arrow
          images/icons/control
          images/icons/file
          images/icons/misc
          images/icons/stickman
          images/icons/symbol
          images/icons/tool
          images/icons/style)

@(define (author-email) "neil.toronto@gmail.com")

@title{Icons}
@author{@(author+email "Neil Toronto" (author-email))}


@(define icons-eval (make-base-eval))
@interaction-eval[#:eval icons-eval (require racket/class racket/draw racket/math racket/list)]

@;====================================================================================================

@section{What is an icon?}
@margin-note*{This section describes an ideal that DrRacket and its tools are steadily approaching.}

As a first approximation, an icon is just a small @racket[bitmap%], usually with an alpha channel.

But an icon also communicates.
Its shape and color are a visual metaphor for an action or a message.
Icons should be @bold{easily recognizable}, @bold{distinguishable}, @bold{visually consistent}, and @bold{metaphorically appropriate} for the actions and messages they are used with.
It can be difficult to meet all four requirements at once (``distinguishable'' and ``visually consistent' are often at odds), but good examples, good abstractions, and an existing icon library help considerably.

@(define (hash-quote) (hash-quote-icon #:color macro-stepper-hash-color #:height 16))
@(define (step) (step-icon #:color syntax-icon-color #:height 16))
@(define (play) (play-icon #:color syntax-icon-color #:height 16))
@(define (bar) (bar-icon #:color syntax-icon-color #:height 16))
@(define (macro-stepper) (macro-stepper-icon #:height 16))

Example: The Macro Stepper icon is composed by appending a text icon @(hash-quote) and a step icon @(step) to get @(macro-stepper).
The syntax quote icon @(hash-quote) is the color that DrRacket colors syntax quotes by default.
The step icon @(step) is colored like DrRacket colors identifier syntax by default, and is shaped using metaphors used in debugger toolbars, TV remotes, and music players around the world.
It is composed of @(play) to connote starting and @(bar) to connote immediately stopping.

It would not do to have just @(step) as the Macro Stepper icon: it would be too easily confused with the Debugger icon @(step-icon #:color run-icon-color #:height 16),
especially for new users and people with certain forms of color-blindness, and thus fail to be distinguishable enough.

As another example, the Check Syntax icon @(check-syntax-icon #:height 16) connotes inspecting and passing.
Notice that the check mark is also the color of syntax.

@;====================================================================================================

@section{About These Icons}

The icons in this collection are designed to be composed to create new ones: they are simple, thematically consistent, and can be constructed in any size and color.
Further, slideshow's @racket[pict] combiners offer a way to compose them almost arbitrarily.
For example, a media player application might create a large ``step'' button by superimposing a @racket[record-icon] and a @racket[step-icon]:
@interaction[#:eval icons-eval
                    (require pict images/icons/control images/icons/style)
                    (pict->bitmap
                     (cc-superimpose
                      (bitmap (record-icon #:color "forestgreen" #:height 96
                                           #:material glass-icon-material))
                      (bitmap (step-icon #:color light-metal-icon-color #:height 48
                                         #:material metal-icon-material))))]

All the icons in this collection are first drawn using standard @racket[dc<%>] drawing commands.
Then, to get lighting effects, they are turned into 3D objects and @link["http://en.wikipedia.org/wiki/Ray_tracing_%28graphics%29"]{ray traced}.
Many are afterward composed to create new icons; for example, the @racket[stop-signs-icon] @(stop-signs-icon #:height 16) superimposes three @racket[stop-sign-icon]s, and the @racket[magnifying-glass-icon] @(magnifying-glass-icon #:height 16) is composed of three others (frame, glass and handle).

The ray tracer helps keep icons visually consistent with each other and with physical objects in day-to-day life.
As an example of the latter, the @racket[record-icon], when rendered in clear glass, looks like the clear, round button on a @link["http://en.wikipedia.org/wiki/Wiimote"]{Wii Remote}.
See the @racket[plt-logo] and @racket[planet-logo] functions for more striking examples.

When the rendering API is stable enough to publish, it will allow anyone who can draw a shape to turn that shape into a visually consistent icon.

As with any sort of rendering (such as @link["http://en.wikipedia.org/wiki/Scalable_Vector_Graphics"]{SVG} rendering), ray tracing takes time.
For icons, this usually happens during tool or application start up.
You can reduce the portion of start-up time taken by rendering to almost nothing by using the @racketmodname[images/compile-time] library to embed bitmaps directly into compiled modules.

@;====================================================================================================

@section{Icon Style}

@defmodule[images/icons/style]
@interaction-eval[#:eval icons-eval (require images/icons/style)]

Use these constants and parameters to help keep icon sets visually consistent.

@doc-apply[light-metal-icon-color]
@doc-apply[metal-icon-color]
@doc-apply[dark-metal-icon-color]{
Good colors to use with @racket[metal-icon-material]. See @racket[bomb-icon] and @racket[magnifying-glass-icon] for examples.
}

@doc-apply[syntax-icon-color]
@doc-apply[halt-icon-color]
@doc-apply[run-icon-color]{
Standard toolbar icon colors.

Use @racket[syntax-icon-color] in icons that connote macro expansion or syntax. Example:
@interaction[#:eval icons-eval (step-icon #:color syntax-icon-color #:height 32)]

Use @racket[halt-icon-color] in icons that connote stopping or errors. Example:
@interaction[#:eval icons-eval (stop-icon #:color halt-icon-color #:height 32)]

Use @racket[run-icon-color] in icons that connote executing programs or evaluation. Examples:
@interaction[#:eval icons-eval
                    (play-icon #:color run-icon-color #:height 32)
                    (require images/icons/stickman)
                    (running-stickman-icon 0.9 #:height 32
                                           #:body-color run-icon-color
                                           #:arm-color "white"
                                           #:head-color run-icon-color)]

For new users and for accessibility reasons, do not try to differentiate icons for similar functions only by color.
}

@doc-apply[default-icon-height]{
The height of DrRacket's standard icons.}

@doc-apply[toolbar-icon-height]{
The height of DrRacket toolbar icons.

Use @racket[(toolbar-icon-height)] as the @racket[height] argument for common icons that will be used in toolbars, status bars, and buttons.

(When making an icon for DrRacket's main toolbar, try to keep it nearly square so that it will not take up too much horizontal space when the toolbar is docked vertically.
If you cannot, as with the Macro Stepper, send a thinner icon as the @racket[alternate-bitmap] argument to a @racket[switchable-button%].)
}

@doc-apply[default-icon-backing-scale]{
The backing scale of DrRacket icons.

A backing scale of 2 means that the icon bitmap internally has two
pixels per drawing unit, so it it renders well a double resolution,
such as Retina display mode for Mac OS X.

@history[#:added "1.1"]}

@doc-apply[plastic-icon-material]
@doc-apply[rubber-icon-material]
@doc-apply[glass-icon-material]
@doc-apply[metal-icon-material]{
Materials for icons.

Plastic is opaque and reflects a little more than glass.

Rubber is also opaque, reflects more light than plastic, but diffuses less.

Glass is transparent but frosted, so it scatters refracted light.
It has the high refractive index of @link["http://en.wikipedia.org/wiki/Cubic_zirconia"]{cubic zirconia}, or fake diamond.
The ``glassy look'' cannot actually be achieved using glass.

Metal reflects the most, its @link["http://en.wikipedia.org/wiki/Specular_highlight"]{specular highlight} is nearly the same color as the material (in the others, the highlight is white),
and it diffuses much more ambient light than directional.
This is because while plastic and glass mostly reflect light directly, metal mostly absorbs light and re-emits it.

@examples[#:eval icons-eval
                 (require images/icons/misc)
                 (for/list ([material  (list plastic-icon-material
                                             rubber-icon-material
                                             glass-icon-material
                                             metal-icon-material)])
                   (bomb-icon #:height 32 #:material material))]
}

@doc-apply[default-icon-material]{
The material used for rendering most icons and icon parts.
There are exceptions; for example, the @racket[floppy-disk-icon] always renders the sliding cover in metal.
}

@doc-apply[bitmap-render-icon]{
Makes a 3D object out of @racket[bitmap] and renders it as an icon.

The @racket[z-ratio] argument only makes a difference when @racket[material] is transparent, such as @racket[glass-icon-material].
It controls what fraction of @racket[bitmap]'s height the icon is raised, which in turn affects the refracted shadow under the icon:
the higher the @racket[z-ratio], the lower the shadow.

@examples[#:eval icons-eval
                 (define bitmap
                   (pict->bitmap (colorize (filled-ellipse 64 64) "tomato")))
                 (for/list ([z-ratio  (in-range 0 2 1/3)])
                   (bitmap-render-icon bitmap z-ratio glass-icon-material))]

More complex shapes than ``embossed and rounded'' are possible with the full rendering API, which will be made public in a later release.
Still, most of the simple icons (such as those in @racketmodname[images/icons/arrow] and @racketmodname[images/icons/control]) can be rendered using only @racket[bitmap-render-icon].
}

@doc-apply[icon-color->outline-color]{
For a given icon color, returns the proper outline @racket[color%].

As an example, here is how to duplicate the @racket[record-icon] using @racketmodname[pict]:
@interaction[#:eval icons-eval
                    (define outline-color (icon-color->outline-color "forestgreen"))
                    (define brush-pict (colorize (filled-ellipse 62 62) "forestgreen"))
                    (define pen-pict (linewidth 2 (colorize (ellipse 62 62) outline-color)))
                    (bitmap-render-icon
                     (pict->bitmap (inset (cc-superimpose brush-pict pen-pict) 1))
                     5/8 glass-icon-material)
                    
                    (record-icon #:color "forestgreen" #:height 64
                                 #:material glass-icon-material)]

The outline width is usually @racket[(/ height 32)] (in this case, @racket[2]), but not always.
(For example, @racket[recycle-icon] is an exception, as are parts of @racket[floppy-disk-icon].)
}

@;====================================================================================================

@section[#:tag "arrows"]{Arrow Icons}

@defmodule[images/icons/arrow]
@interaction-eval[#:eval icons-eval (require images/icons/arrow)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[right-arrow-icon]
@doc-apply[left-arrow-icon]
@doc-apply[up-arrow-icon]
@doc-apply[down-arrow-icon]{
Standard directional arrows.
@examples[#:eval icons-eval
                 (list (right-arrow-icon #:color syntax-icon-color
                                         #:height (toolbar-icon-height))
                       (left-arrow-icon #:color run-icon-color)
                       (up-arrow-icon #:color halt-icon-color #:height 37)
                       (down-arrow-icon #:color "lightblue" #:height 44
                                        #:material glass-icon-material))]}

@doc-apply[right-over-arrow-icon]
@doc-apply[left-over-arrow-icon]
@doc-apply[right-under-arrow-icon]
@doc-apply[left-under-arrow-icon]{
Standard bent arrows.
@examples[#:eval icons-eval
                 (list (right-over-arrow-icon #:color metal-icon-color
                                              #:height (toolbar-icon-height))
                       (left-over-arrow-icon #:color dark-metal-icon-color)
                       (right-under-arrow-icon #:color run-icon-color #:height 37)
                       (left-under-arrow-icon #:color "lightgreen" #:height 44
                                              #:material glass-icon-material))]
}

@;====================================================================================================

@section[#:tag "control"]{Control Icons}

@defmodule[images/icons/control]
@interaction-eval[#:eval icons-eval (require images/icons/control)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[bar-icon]{
@examples[#:eval icons-eval (bar-icon #:color run-icon-color #:height 32)]
This is not a ``control'' icon @italic{per se}, but is used to make many others.
}
@doc-apply[play-icon]{ @examples[#:eval icons-eval (play-icon #:color run-icon-color #:height 32)] }
@doc-apply[back-icon]{ @examples[#:eval icons-eval (back-icon #:color run-icon-color #:height 32)] }
@doc-apply[fast-forward-icon]{ @examples[#:eval icons-eval (fast-forward-icon #:color syntax-icon-color #:height 32)] }
@doc-apply[rewind-icon]{ @examples[#:eval icons-eval (rewind-icon #:color syntax-icon-color #:height 32)] }
@doc-apply[stop-icon]{ @examples[#:eval icons-eval (stop-icon #:color halt-icon-color #:height 32)] }
@doc-apply[record-icon]{ @examples[#:eval icons-eval (record-icon #:color "red" #:height 32)] }
@doc-apply[pause-icon]{ @examples[#:eval icons-eval (pause-icon #:color halt-icon-color #:height 32)] }
@doc-apply[step-icon]{ @examples[#:eval icons-eval (step-icon #:color run-icon-color #:height 32)] }
@doc-apply[step-back-icon]{ @examples[#:eval icons-eval (step-back-icon #:color run-icon-color #:height 32)] }
@doc-apply[continue-forward-icon]{ @examples[#:eval icons-eval (continue-forward-icon #:color run-icon-color #:height 32)] }
@doc-apply[continue-backward-icon]{ @examples[#:eval icons-eval (continue-backward-icon #:color run-icon-color #:height 32)] }
@doc-apply[search-forward-icon]{ @examples[#:eval icons-eval (search-forward-icon #:color syntax-icon-color #:height 32)] }
@doc-apply[search-backward-icon]{ @examples[#:eval icons-eval (search-backward-icon #:color syntax-icon-color #:height 32)] }

@;====================================================================================================

@section[#:tag "file"]{File Icons}

@defmodule[images/icons/file]
@interaction-eval[#:eval icons-eval (require images/icons/file)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[floppy-disk-icon]{ @examples[#:eval icons-eval (floppy-disk-icon #:height 32 #:material glass-icon-material)] }
@doc-apply[save-icon]{ @examples[#:eval icons-eval (save-icon #:height 32)] }
@doc-apply[load-icon]{ @examples[#:eval icons-eval (load-icon #:height 32)] }
@doc-apply[small-save-icon]{ @examples[#:eval icons-eval (small-save-icon #:height 32)] }
@doc-apply[small-load-icon]{ @examples[#:eval icons-eval (small-load-icon #:height 32)] }

@;====================================================================================================

@section[#:tag "symbol"]{Symbol and Text Icons}

@defmodule[images/icons/symbol]
@interaction-eval[#:eval icons-eval (require images/icons/symbol)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[text-icon]{
Renders a text string as an icon. For example,
@interaction[#:eval icons-eval
                    (text-icon "An Important Point!"
                               (make-font #:weight 'bold #:underlined? #t)
                               #:color "lightskyblue" #:height 44)]

The size of @racket[font] is ignored. If @racket[trim?] is @racket[#f], the drawn text is not cropped before rendering.
Otherwise, it is cropped to the smallest rectangle containing all the non-zero-alpha pixels.
Rendering very small glyphs shows the difference dramatically:
@interaction[#:eval icons-eval
                    (list (text-icon "." #:trim? #t)
                          (text-icon "." #:trim? #f))]
Notice that both icons are @racket[(default-icon-height)] pixels tall.

Because different platforms have different fonts, @racket[text-icon] cannot guarantee the icons it returns have a consistent look or width across all platforms, or that any unicode characters in @racket[str] will exist.
}

@doc-apply[recycle-icon]{
Returns the universal recycling symbol, rendered as an icon.
@examples[#:eval icons-eval (recycle-icon #:height 48)]
}

@doc-apply[x-icon]{
Returns an ``x'' icon that is guaranteed to look the same on all platforms.
(Anything similar that would be constructed by @racket[text-icon] would differ at least slightly across platforms.)
@examples[#:eval icons-eval (x-icon #:height 32)]

@history[#:changed "1.1" @elem{Added optional @racket[#:thickness] argument.}]}

@doc-apply[check-icon]{
@examples[#:eval icons-eval (check-icon #:height 32)]
}

@doc-apply[lambda-icon]{
@examples[#:eval icons-eval
                 (lambda-icon #:height 32 #:material metal-icon-material)]
}

@doc-apply[hash-quote-icon]{
@examples[#:eval icons-eval
                 (require (only-in images/icons/tool macro-stepper-hash-color))
                 (hash-quote-icon #:color macro-stepper-hash-color #:height 32)]
}

@;====================================================================================================

@section[#:tag "misc"]{Miscellaneous Icons}

@defmodule[images/icons/misc]
@interaction-eval[#:eval icons-eval (require images/icons/misc)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[regular-polygon-icon]{
Renders the largest regular polygon with @racket[sides] sides, with the first vertex at angle @racket[start], that can be centered in a @racket[height] × @racket[height] box.
The default @racket[start] angle is chosen so that the polygon has a horizontal bottom edge.
@examples[#:eval icons-eval (for/list ([sides  (in-range 1 9)]
                                       [material  (in-cycle (list plastic-icon-material
                                                                  glass-icon-material))])
                              (regular-polygon-icon sides #:color "cornflowerblue" #:height 32
                                                    #:material material))]
}

@doc-apply[stop-sign-icon]{
@examples[#:eval icons-eval
                 (stop-sign-icon #:height 32 #:material glass-icon-material)]
}

@doc-apply[stop-signs-icon]{
@examples[#:eval icons-eval
                 (stop-signs-icon #:height 32 #:material plastic-icon-material)]
}

@doc-apply[foot-icon]{
@examples[#:eval icons-eval
                 (foot-icon #:color "chocolate" #:height 32
                            #:material glass-icon-material)]
}

@doc-apply[magnifying-glass-icon]{
@examples[#:eval icons-eval
                 (magnifying-glass-icon #:height 32)]
}

@doc-apply[left-magnifying-glass-icon]{
@examples[#:eval icons-eval
                 (left-magnifying-glass-icon #:height 32)]
}

@doc-apply[bomb-icon]{
@examples[#:eval icons-eval
                 (bomb-icon #:height 48 #:material glass-icon-material)]
}

@doc-apply[left-bomb-icon]{
@examples[#:eval icons-eval
                 (left-bomb-icon #:height 48)]
}

@doc-apply[clock-icon]{
@examples[#:eval icons-eval
                 (clock-icon #:height 96)
                 (clock-icon 3 21 #:height 48
                             #:face-color "lightblue"
                             #:hand-color "darkblue")]
}

@doc-apply[stopwatch-icon]{
@examples[#:eval icons-eval (stopwatch-icon #:height 96)]
}

@doc-apply[stethoscope-icon]{
@examples[#:eval icons-eval (stethoscope-icon #:height 96)]
}

@doc-apply[short-stethoscope-icon]{
@examples[#:eval icons-eval (short-stethoscope-icon #:color "purple" #:height 96)]
}

@doc-apply[lock-icon]{
@examples[#:eval icons-eval
                 (lock-icon #:height 32)
                 (lock-icon #t #:height 48
                            #:body-color "navajowhite"
                            #:shackle-color "lemonchiffon"
                            #:material glass-icon-material)]
}

@doc-apply[close-icon]{
@examples[#:eval icons-eval
                 (close-icon #:height 32 #:material glass-icon-material)]
@history[#:added "1.1"]}

@;====================================================================================================

@section[#:tag "stickman"]{Stickman Icons}

@defmodule[images/icons/stickman]
@interaction-eval[#:eval icons-eval (require images/icons/stickman)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[standing-stickman-icon]{
Returns the icon displayed in DrRacket's lower-right corner when no program is running.
@examples[#:eval icons-eval (standing-stickman-icon #:height 64)]
}

@doc-apply[running-stickman-icon]{
Returns a frame of the icon animated in DrRacket's lower-right corner when a program is running.
The frame returned is for time @racket[t] of a run cycle with a one-second period.

The following example samples the run cycle at 12 Hz, or every @racket[1/12] second:
@interaction[#:eval icons-eval
                    (for/list ([t  (in-range 0 1 1/12)])
                      (running-stickman-icon t #:height 32))]

The stickman's joint angles are defined by continuous periodic functions, so the run cycle can be sampled at any resolution, or at any real-valued time @racket[t].
The cycle is modeled after the run cycle of the player's avatar in the Commodore 64 game @link["http://en.wikipedia.org/wiki/Impossible_Mission"]{Impossible Mission}.
}

@;====================================================================================================

@section[#:tag "tool"]{Tool Icons}

@defmodule[images/icons/tool]
@interaction-eval[#:eval icons-eval (require images/icons/tool)]

@history[#:changed "1.1" @elem{Added optional @racket[#:backing-scale] arguments.}]

@doc-apply[check-syntax-icon]
@doc-apply[small-check-syntax-icon]{
Icons for Check Syntax. The @racket[small-check-syntax-icon] is used when the toolbar is on the side.
@examples[#:eval icons-eval (list (check-syntax-icon #:height 32)
                                  (small-check-syntax-icon #:height 32))]
}

@doc-apply[macro-stepper-icon]
@doc-apply[small-macro-stepper-icon]{
Icons for the Macro Stepper. The @racket[small-macro-stepper-icon] is used when the toolbar is on the side.
@examples[#:eval icons-eval (list (macro-stepper-icon #:height 32)
                                  (small-macro-stepper-icon #:height 32))]
}

@doc-apply[debugger-icon]
@doc-apply[small-debugger-icon]{
Icons for the Debugger. The @racket[small-debugger-icon] is used when the toolbar is on the side.
@examples[#:eval icons-eval (list (debugger-icon #:height 32)
                                  (small-debugger-icon #:height 32))]
}

@doc-apply[debugger-bomb-color]
@doc-apply[macro-stepper-hash-color]
@doc-apply[small-macro-stepper-hash-color]{
Constants used within @racketmodname[images/icons/tool].
}


@close-eval[icons-eval]
