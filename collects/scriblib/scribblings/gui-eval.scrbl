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
@defform[(gui-interaction datum ...)]
@defform[(gui-interaction-eval datum ...)]
@defform[(gui-interaction-eval-show datum ...)]
@defform[(gui-schemeblock+eval datum ...)]
@defform[(gui-schememod+eval datum ...)]
@defform[(gui-def+int datum ...)]
@defform[(gui-defs+int datum ...)]
)]{

Like @scheme[interaction], etc., but actually evaluating the forms
only when the @envvar{MREVAL} environment variable is set, and then in
an evaluator that is initialized with @schememodname[racket/gui/base]
and @schememodname[slideshow]. }
