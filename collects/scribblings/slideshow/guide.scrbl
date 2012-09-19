#lang scribble/doc
@(require "ss.rkt" scribble/struct)

@(define (control-table . l)
   (make-table
    #f
    (map (lambda (p)
           (list (make-flow (list (make-paragraph (list (hspace 2)))))
                 (make-flow (list (make-paragraph (list (car p)))))
                 (make-flow (list (make-paragraph (list (hspace 1) ":" (hspace 1)))))
                 (make-flow (list (make-paragraph (list (cadr p)))))))
         l)))


@title{Creating Slide Presentations}

The @racketmodname[slideshow] module acts as a language that includes:

@itemize[

 @item{all of @racketmodname[racket];}

 @item{pict-creating functions from @racketmodname[slideshow/pict]; and}

 @item{slide-composing functions from @racketmodname[slideshow/base].}

]

The @racketmodname[slideshow] and @racketmodname[slideshow/base]
module initialization also check the
@racket[current-command-line-arguments] parameter to configure the
slide mode (e.g., printing).

The rest of this section repeats information that is presented by the
tutorial slideshow, which can be viewed by running the
@exec{slideshow} executable and clicking the @onscreen{Run Tutorial}
link.

@section{Slide Basics}

The main Slideshow function is @racket[slide], which adds a slide to
the presentation with a given content. For example, the ``Hello
World'' presentation can be defined by the following module:

@racketmod[
slideshow

(slide
 #:title "How to Say Hello"
 (t "Hello World!"))
]
 
The @racket[t] function in this example creates a pict containing the
given text using the default font and style.

Executing the above module pops up a slide-presentation window. Type
Alt-q (or Meta-q) to end the slides. Here are more controls:

@control-table[
  (list "Alt-q, Meta-q, or Cmd-q"             "end slide show")
  (list "Esc"                                 "if confirmed, end show")
  (list "Right arrow, Space, f, n, or click"  "next slide")
  (list "Left arrow, Backspace, Delete, or b" "previous slide")
  (list "g"                                   "last slide")
  (list "1"                                   "first slide")
  (list "s"                                   "next slide with a different title/name")
  (list "a"                                   "previous slide starting different title/name")
  (list "Alt-g, Cmd-g, or Meta-g"             "select a slide")
  (list "Alt-p, Cmd-p, or Meta-p"             "show/hide slide number")
  (list "Alt-c, Cmd-c, or Meta-c"             "show/hide commentary")
  (list "Alt-d, Cmd-d, or Meta-d"             "show/hide preview")
  (list "Alt-m, Cmd-m, or Meta-m"             "show/hide mouse cursor")
  (list "Shift with arrow"                    "move window 1 pixel")
  (list "Alt, Meta, or Cmd with arrow"        "move window 10 pixels")
]

The @racket[slide] function accepts any number of arguments. Each
argument is a pict to be centered on the slide. The picts are stacked
vertically with @racket[(current-gap-size)] separation between each pict, and
the total result is centered (as long as there's a gap of at least
@racket[(* 2 (current-gap-size))] between the title and content).

@racketmod[
slideshow

(slide
 #:title "How to Say Hello"
 (t "Hello World!")
 (t "Goodbye Dlrow!"))
]

Various functions format paragraphs and generate bulleted items for
lists. For example, @racket[item] creates a bulleted paragraph that
spans (by default) the middle @math{2/3} of the slide:

@racketmod[
slideshow

(slide
 #:title "How to Say Hello"
 (item "If you want to create an example, you"
      "can always do something with" (bt "Hello World!"))
 (item "It's a bit silly, but a follow-up example"
       "could be" (bt "Goodbye Dlrow!")))
]

As the example illustrates, the @racket[item] function accepts a
mixture of strings and picts, and it formats them as a paragraph.

@; ------------------------------------------------------------------------

@section[#:tag "staging"]{Staging Slides}

The @racket[slide] function creates a slide as a side effect. It can
be put inside a function to abstract over a slide:

@racketmod[
slideshow

(define (slide-n n)
  (slide
   #:title "How to Generalize Slides"
   (item "This is slide number" (number->string n))))

(slide-n 1)
(slide-n 2)
(slide-n 3)]

The @racket[slide] function also has built-in support for some common
multi-slide patterns. Each element argument to @racket[slide] is
usually a pict, but there are a few other possibilities:

@itemize[

  @item{If an element is @racket['next], then a slide is generated
    containing only the preceding elements, and then the elements are
    re-processed without the @racket['next]. Multiple @racket['next]
    elements generate multiple slides.}

  @item{If an element is @racket['alts], then the next element must be
    a list of element lists. Each list up to the last one is appended
    to the elements before @racket['alts] and the resulting list of
    elements is processed. The last lists is appended to the preceding
    elements along with the remaining elements (after the list of
    lists) and the result is re-processed.}

 @item{A @racket['nothing] element is ignored (useful as a result of a
    branching expression).}

 @item{A @racket['next!] element is like @racket['next], except that
    it is preserved when condensing (via the @DFlag{condense} flag).}

 @item{A @racket['alts~] element is like @racket['alts], except that
    it is @italic{not} preserved when condensing.}

  @item{A comment produced by @racket[comment] is ignored, except when
    commentary is displayed.}

]

Here's an example to illustrate how @racket['next] and @racket['alts]
work:

@racketmod[
slideshow

(slide
 #:title "Example"
 (item "First step")
 'next
 (item "Second step")
 'next
 'alts
 (list (list (item "Tentative third step")
             'next
             (item "This isn't working... back up"))
       (list (item "Third step that works")))
 'next
 (item "Fourth step"))
]

@; ------------------------------------------------------------------------

@section[#:tag "display-size"]{Display Size and Fonts}

Slideshow is configured for generating slides in @math{1024} by
@math{768} pixel format. When the current display has a different
size as Slideshow is started, the Slideshow display still
occupies the entire screen, and pictures are scaled just before
they are displayed. Thus, one picture unit reliably corresponds
to a ``pixel'' that occupies @math{1/1024} by @math{1/768} of the
screen.

The @racket[text] form for generating text pictures takes into
account any expected scaling for the display when measuring
text. (All Slideshow text functions, such as @racket[t] and
@racket[item] are built on @racket[text].) In particular, scaling
the picture causes a different font size to be used for drawing
the slide---rather than bitmap-scaling the original font---and
changing the font size by a factor of @math{k} does not
necessarily scale all text dimensions equally by a factor of
@math{k}---because, for most devices, each character must have
integer dimensions. Nevertheless, especially if you use the
@racket[current-expected-text-scale] parameter, Slideshow is
usually able to produce good results when the slide is scaled.

More generally, different font sets on different platforms can
change the way a slide is rendered. For example, the @racket[tt]
font on one platform might be slightly wider than on another,
causing different line breaks, and so on. Beware.

Beware also of using bitmaps in slides when the presentation
screen is not @math{1024} by @math{768}. In that case, consider
using @racket[size-in-pixels] (with the caveat that the resulting
picture will take up different amounts of the slide on different
displays).

@; ------------------------------------------------------------------------

@section{Command-line Options}

@defmodule[slideshow/start]

The @exec{slideshow} executable instantiates the
@racketmodname[slideshow/start] module, which inspects the command
line as reported by @racket[current-command-line-arguments] to get
another module to @racket[require] for the slide content.The @racketmodname[slideshow/start]
module also initializes
variables like @racket[printing?] and @racket[condense?] based on
flags supplied on the command line.

Thus, if the above example is in @filepath{multi-step.rkt}, then the
command

@commandline{slideshow multi-step.rkt}

runs the slides.

If the module given to @exec{slideshow} has a @racketidfont{slideshow}
submodule, then @racketmodname[slideshow/start] @racket[require]s the
@racketidfont{slideshow} submodule after @racket[require]ing the
module. If the module has no @racketidfont{slideshow} but has a
@racketidfont{main} submodule, then the @racketidfont{main} submodule
is @racket[require]d.

The @exec{slideshow} executable accepts a number of command-line
flags.  Use the @DFlag{help} flag to obtain a list of other
flags.


@; ------------------------------------------------------------------------

@section{Printing}

The @Flag{p} or @DFlag{print} command-line flag causes Slideshow to
print slides instead of showing them on the screen using the current
platform's printing system. The @Flag{P} or @DFlag{ps} generates
PostScript directly, while @Flag{D} or @DFlag{pdf} generates
PDF directly.

PS-to-PDF converters vary on how well they handle landscape
mode. Here's a Ghostscript command that converts slides reliably
(when you replace @filepath{src.ps} and @filepath{dest.pdf} with your
file names):

@commandline{gs -q -dAutoRotatePages=/None -dSAFER -dNOPAUSE -dBATCH 
     -sOutputFile=dest.pdf -sDEVICE=pdfwrite -c .setpdfwrite 
     -c "<</Orientation 3>> setpagedevice" -f src.ps}
