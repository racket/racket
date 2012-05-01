#lang scribble/doc
@(require scribble/manual)

@title{Getting Started with Racket}

To get started with Racket,
@link["http://www.racket-lang.org/download/"]{download it} from the webpage and
install it. If you are a beginner or would like to use a graphical environment
to run programs, run the @tt{DrRacket} executable. Otherwise, the @tt{racket}
executable will run a command-line Read-Eval-Print-Loop
(@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{REPL}).

On Windows, you can access Racket through the Start menu. By default it is
installed in @onscreen{Program Files} → @onscreen{Racket} →
@onscreen{DrRacket}.  On Mac OS, click on the @tt{DrRacket} app either in the
@tt{.dmg} file or where you installed Racket.  On Linux or Unix, run the
@tt{drracket} binary.

If you are new to programming or if you have the patience to work
through a textbook:

@itemize[

 @item{@italic{@link["http://htdp.org/"]{How to Design Programs}}
       is the best place to start. Whenever the book says ``Scheme,''
       you can read it as ``Racket.''}

 @item{@other-manual['(lib "web-server/scribblings/tutorial/continue.scrbl")]
       introduces you to modules and building web applications.}

 @item{@other-manual['(lib "scribblings/guide/guide.scrbl")] describes
       the rest of the Racket language, which is much bigger than
       the learning-oriented languages of the textbook. Since you
       learned functional programming from the textbook, you'll be
       able to skim chapters 1 and 2 of the Guide.}

]


If you're already a programmer and you're in more of a hurry:

@itemize[

 @item{@other-manual['(lib "scribblings/quick/quick.scrbl")] gives you
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
