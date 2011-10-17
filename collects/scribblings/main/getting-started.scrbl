#lang scribble/doc
@(require scribble/manual)

@title{Getting Started with Racket}

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
