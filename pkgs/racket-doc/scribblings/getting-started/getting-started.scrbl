#lang scribble/doc
@(require scribble/manual scribblings/private/docname)

@title{Getting Started}

To get started with Racket, @hyperlink["http://racket-lang.org/download/"]{download it}
from the web page and install it. If you are a beginner or would like to use a
graphical environment to run programs, run the @exec{DrRacket} executable.
@margin-note*{If you prefer, you can also work with your favorite text editor
(see @secref["other-editors" #:doc '(lib "scribblings/guide/guide.scrbl")]).}
Otherwise, the @exec{racket} executable will run a command-line Read-Eval-Print-Loop
(@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{REPL}).

On Windows, you can start DrRacket from the @onscreen{Racket} entry in the
Start menu. In Windows Vista or newer, you can just type @exec{DrRacket}.  You can
also run it from its folder, which you can find in @onscreen{Program Files} →
@onscreen{Racket} → @onscreen{DrRacket}.

On Mac OS, double click on the @onscreen{DrRacket} icon. It is probably in a
@onscreen{Racket} folder that you dragged into your
@onscreen{Applications} folder. If you want to use command-line tools, instead,
Racket executables are in the @filepath{bin} directory of the @onscreen{Racket}
folder (see @hyperlink["https://github.com/racket/racket/wiki/Configure-Command-Line-for-Racket"]{Configure
Command Line for Racket} to set your @envvar{PATH} environment variable).

On Unix (including Linux), double click on the @onscreen{DrRacket} icon if your
distribution creates one, which is case for many environments. The
@exec{drracket} executable can also be run directly from the command-line if it
is in your path, which is probably the case if you chose a Unix-style distribution
when installing. Otherwise, navigate to the directory where the Racket distribution
is installed, and the @exec{drracket} executable will be in the @filepath{bin}
subdirectory.

If you are new to programming or if you have the patience to work
through a textbook:

@itemize[

 @item{@italic{@link["https://htdp.org/"]{How to Design Programs, Second Edition}}
       is the best place to start.}

 @item{The @Continue[Continue-title] tutorial introduces you to modules and
       building web applications.}

 @item{@other-manual['(lib "scribblings/guide/guide.scrbl")] describes
       the rest of the Racket language, which is much bigger than
       the learning-oriented languages of the textbook. Since you
       learned functional programming from the textbook, you'll be
       able to skim chapters 1 and 2 of the Guide.}

]


If you're already a programmer and you're in more of a hurry:

@itemize[

 @item{@Quick[Quick-title] gives you
       a taste of Racket.}

 @item{@other-manual['(lib "scribblings/more/more.scrbl")] dives much
       deeper and much faster. If it's too much, just skip to the
       Guide.}

 @item{@other-manual['(lib "scribblings/guide/guide.scrbl")] starts
       with a tutorial on Racket basics, and then it describes the rest
       of the Racket language.}

]

Of course, you should feel free to mix and match the above two tracks,
since there is information in each that is not in the other.
