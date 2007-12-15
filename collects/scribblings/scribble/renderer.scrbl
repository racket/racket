#lang scribble/doc
@require[scribble/manual]
@require["utils.ss"]
@require[(for-label scheme/class)]

@(define-syntax-rule (defmodule/local lib . content)
   (begin
     (define-syntax-rule (intro)
       (begin
         (require (for-label lib))
         (defmodule lib)
         . content))
     (intro)))

@title[#:tag "renderer"]{Renderer}

A renderer is an object that provides two main methods:
@scheme[collect] and @scheme[render]. The first method is called to
collect global information about the document, including information
that spans multiple documents rendered together; the collection pass
tends to be format-independent, and it usually implemented completely
by the base renderer. The latter method generates the actual output,
which is naturally specific to a particular format.

@section{Base Renderer}

@defmodule[scribble/base-render]{The
@schememodname[scribble/base-render] module provides @scheme[render%],
which implements the core of a renderer. This rendering class must be
refined with a mixin from @schememodname[scribble/text-render],
@schememodname[scribble/html-render], or
@schememodname[scribble/latex-render].}

The mixin structure is meant to support document-specific extensions
to the renderers. For example, the @exec{scribble} command-line tool
might, in the future, extract rendering mixins from a document module
(in addition to the document proper).

See the @filepath{base-render.ss} source for more information about
the methods of the renderer. Documents built with higher layers, such
as @schememodname[scribble/manual], generally do not call the render
object's methods directly.

@defclass[render% object% ()]{

Represents a renderer.

@defconstructor[([dest-dir path-string?])]{

Creates a renderer whose output goes to @scheme[dest-dir].

}


@defmethod[(collect [srcs (listof path-string?)]
                    [dests (listof path-string?)])
           collect-info?]{

Performs the @techlink{collect pass}.

}

@defmethod[(resolve [srcs (listof path-string?)]
                    [dests (listof path-string?)]
                    [ci collect-info?])
           resolve-info?]{

Performs the @techlink{resolve pass}.

}

@defmethod[(render [srcs (listof path-string?)]
                   [dests (listof path-string?)]
                   [ri resolve-info?])
           void?]{

Produces the final output.

}

@defmethod[(serialize-info [ri resolve-info?])
           any/c]{

Serializes the collected info in @scheme[ri].

}

@defmethod[(deserialize-info [v any/c]
                              [ci collect-info?])
           void?]{

Adds the deserialized form of @scheme[v] to @scheme[ci].

}

}

@; ----------------------------------------

@section{Text Renderer}

@defmodule/local[scribble/text-render]{

@defthing[render-mixin ((subclass?/c render%) . -> . (subclass?/c render%))]{

Specializes @scheme[render%] for generating plain text.}}

@; ----------------------------------------

@section{HTML Renderer}

@defmodule/local[scribble/html-render]{

@defthing[render-mixin ((subclass?/c render%) . -> . (subclass?/c render%))]{

Specializes @scheme[render%] for generating a single HTML file.}

@defthing[render-multi-mixin ((subclass?/c render%) . -> . (subclass?/c render%))]{

Further specializes @scheme[render%] for generating a multiple HTML
files. The input class must be first extended with @scheme[render-mixin].}}

@; ----------------------------------------

@section{Latex Renderer}

@defmodule/local[scribble/latex-render]{

@defthing[render-mixin ((subclass?/c render%) . -> . (subclass?/c render%))]{

Specializes @scheme[render%] for generating Latex input.}}
