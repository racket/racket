#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scheme/base))

@title[#:tag "preprocessor"]{Text Preprocessor}

@defmodulelang[scribble/text]{The @schememodname[scribble/text]
language provides everything from @scheme[scheme/base] with a few
changes that make it suitable as a preprocessor language:

@itemize{

  @item{It uses @scheme[read-inside-syntax] to read the body of the
        module, similar to @secref["docreader"].}

  @item{It has a custom printer (@scheme[current-print]) that displays
        all values.  The printer is also installed as the
        @scheme[port-display-handler] so it can be used through
        @scheme[display] as well as @litchar{~a} in format strings.
        The printer displays most values (as is usual for
        @scheme[display]), except for
        @itemize{@item{@scheme[void] and @scheme[#f] are not
                       displayed,}
                 @item{pairs are displayed recursively (just their
                       contents, no parentheses),}
                 @item{promises are forced, thunks are invoked.}}}}

}

This means that to write a text file that has scheme code, you simply
write it as a module in the @scheme[scribble/text] language, and run
it through @exec{mzscheme}.  Here is a sample file:

@verbatim[#:indent 2]|{
  #lang scribble/text
  @(define (angled . body) (list "<" body ">"))@;
  @(define (shout . body) @angled[(map string-upcase body)])@;
  blah @angled{blah @shout{blah} blah} blah
}|

(Note how @litchar["@;"] is used to avoid empty lines in the output.)
