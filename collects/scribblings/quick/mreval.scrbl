#lang scribble/manual
@(require (for-label scribble/eval "mreval.ss"))

@title{Writing Examples with Pict Results}

@defmodule[scribblings/quick/mreval]{The
@schememodname[scribblings/quick/mreval] library support example
evaluations with results that are @schememodname[slideshow] picts.}

@defform[(mr-interaction datum ...)]{

Like @scheme[interaction], but using an evaluator that includes
@schememodname[scheme/gui/base] and @schememodname[slideshow]. 

The trick is that @schememodname[scheme/gui] is not generally
available when rendering documentation, because it requires a GUI
context. The picture output is rendered to an image file when the
@envvar{MREVAL} environment variable is set, so run the enclosing
document once with the environment varibale to generate the
images. Future runs (with the environment variable unset) use the
generated image.}
