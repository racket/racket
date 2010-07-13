#lang scribble/manual
@(require (for-label scribble/eval scriblib/gui-eval))

@title[#:tag "gui-eval"]{Writing Examples with Pict Results}

@defmodule[scriblib/gui-eval]{The
@schememodname[scriblib/gui-eval] library support example
evaluations with results that are @schememodname[slideshow] picts.}

The trick is that @schememodname[racket/gui] is not generally
available when rendering documentation, because it requires a GUI
context. The picture output is rendered to an image file when the
@envvar{MREVAL} environment variable is set, so run the enclosing
document once with the environment varibale to generate the
images. Future runs (with the environment variable unset) use the
generated image.

@deftogether[(
@defform*[((gui-interaction datum ...)
               (gui-interaction 
                #:eval+opts the-eval get-predicate? get-render 
                            get-get-width get-get-height
                datum ...))
               ]
@defform*[((gui-interaction-eval datum ...)
           (gui-interaction-eval 
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ... ))]
@defform*[((gui-interaction-eval-show datum ...)
           (gui-interaction-eval-show
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ...))]
@defform*[((gui-schemeblock+eval datum ...)
           (gui-schemeblock+eval
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ...))]
@defform*[((gui-schememod+eval datum ...)
           (gui-schememod+eval
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ...))]
@defform*[((gui-def+int datum ...)
           (gui-def+int
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ...))]
@defform*[((gui-defs+int datum ...)
           (gui-defs+int
            #:eval+opts the-eval get-predicate? get-render 
                        get-get-width get-get-height
            datum ...))]
)]{

The first option of each of the above is
like @scheme[interaction], etc., but actually evaluating the forms
only when the @envvar{MREVAL} environment variable is set, and then in
an evaluator that is initialized with @schememodname[racket/gui/base]
and @schememodname[slideshow]. 

The second option of each allows you to specify your own evaluator via
the @scheme[the-eval] argument and then to specify four thunks that
return functions for finding and rendering graphical objects:
@itemize[
         @item{@scheme[get-predicate? : (-> (-> any/c boolean?))]
                Determines if a value is a graphical object (and thus handled by the other operations)}
         @item{@scheme[get-render : (-> (-> any/c (is-a?/c dc<%>) number? number? void?))]
                Draws a graphical object (only called if the predicate returned @scheme[#t]; the first
                argument will be the value for which the predicate holds).}
         @item{@scheme[get-get-width : (-> (-> any/c number?))]
                Gets the width of a graphical object (only called if the predicate returned @scheme[#t]; the first
                argument will be the value for which the predicate holds).}
         @item{@scheme[get-get-height : (-> (-> any/c number?))]
                Gets the height of a graphical object (only called if the predicate returned @scheme[#t]; the first
                argument will be the value for which the predicate holds).}
          ]

}
