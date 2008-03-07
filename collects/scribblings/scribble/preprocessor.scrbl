#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scheme/base))

@title[#:tag "preprocessor"]{Text Preprocessor}

@defmodulelang[scribble/text]{The @schememodname[scribble/text]
language provides everything from @scheme[scheme/base] with a few
changes that make it suitable as a preprocessor language:

@itemize{

  @item{It uses @scheme[read-syntax-inside] to read the body of the
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


@;--------------------------------------------------------------------
@section{Using External Files}

Using additional files that contain code for your preprocessing is
trivial: the preprocessor source is a plain Scheme file, so you can
@scheme[require] additional files as usual.

However, things can become tricky if you want to include an external
file that should also be preprocessed.  Using @scheme[require] with a
text file (that uses the @scheme[scribble/text] language) almost
works, but when a module is required, it is invoked before the current
module, which means that the required file will be preprocessed before
the current file regardless of where the @scheme[require] expression
happens to be.  Alternatively, you can use @scheme[dynamic-require]
with @scheme[#f] for the last argument (which makes it similar to a
plain @scheme[load])---but remember that the path will be relative to
the current directory, not to the source file.

Finally, there is a convenient syntax for including text files to be
processed:

@defform[(include filename)]{

Preprocess the @scheme[filename] using the same syntax as
@scheme[scribble/text].  This is similar to using @scheme[load] in a
namespace that can access names bound in the current file so included
code can refer to bindings from the including module.  Note, however,
that the including module cannot refer to names that are bound the
included file because it is still a plain scheme module---for such
uses you should still use @scheme[require] as usual.}
