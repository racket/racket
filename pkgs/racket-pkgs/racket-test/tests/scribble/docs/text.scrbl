#lang scribble/manual
@(require (for-label racket/base))

@title{Document}

This document exercises various constructs to check text output.

@section{Part A}

Scribble is a collection of tools for creating prose documents---papers, books, library documentation, etc.---in HTML or PDF (via Latex)
form. More generally, Scribble helps you 
write programs that are rich
in textual content, whether the content is prose to be typeset or any
other form of text to be generated 
programmatically.

@subsection{A Subsection}

Here's some Racket code:

@racketblock[
 (define half (lambda (x) 
                (x x)))
 (x x)
]

@subsection{Another Subsection}

@defmodule[racket/base]

@defproc[(cons [car (or/c #f 
                          other?)]
               [cdr any?])
         stuff?]{

Ok?}

@section{B}

       @itemlist[

         @item{Run
                @commandline{scribble --pdf mouse.scrbl}
               to generate PDF as @filepath{mouse.pdf}. This will
               work only if you have @exec{pdflatex} installed.
               If you'd like to see the intermediate Latex, try
                @commandline{scribble --latex mouse.scrbl}
               to generate @filepath{mouse.tex}.}

         @item{Run
               @commandline{scribble --html mouse.scrbl}
              to generate HTML as @filepath{mouse.html}.  You may
              notice that the apostrophe in ``he's'' turned into a
              curly apostrophe.}

         @item{Run
               @commandline{scribble --htmls mouse.scrbl}
              to generate HTML as @filepath{mouse/index.html}.
              Sub-sections (which we add next) will appear as separate
              HTML files in the @filepath{mouse} directory.}

          ]

Run the @exec{scribble} command(s) from the old section
again. You may notice the curly double-quotes in the output, and
the @litchar{---} turned into an em dash.

@section{C}

@subsection{Inside C}

Section C had no text before its subsections.

@subsection{Inside C, Again}

But the subsections have text.
