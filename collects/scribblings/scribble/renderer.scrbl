#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label racket/class))

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

@defconstructor[([dest-dir path-string?]
                 [refer-to-existing-files any/c #f]
                 [root-path (or/c path-string? false/c) #f])]{

Creates a renderer whose output will go to @scheme[dest-dir]. For
example, @scheme[dest-dir] could name the directory containing the
output Latex file, the HTML file for a single-file output, or the
output sub-directory for multi-file HTML output.

If @scheme[root-path] is not @scheme[#f], it is normally the same as
@scheme[dest-dir] or a parent of @scheme[dest-dir]. It causes
cross-reference information to record destination files relative to
@scheme[root-path]; when cross-reference information is serialized, it
can be deserialized via @method[render% deserialize-info] with a
different root path (indicating that the destination files have
moved).}


@defmethod[(collect [srcs (listof part?)]
                    [dests (listof path-string?)])
           collect-info?]{

Performs the @techlink{collect pass}. See @method[render% render] for
information on the @scheme[dests] argument.}

@defmethod[(resolve [srcs (listof part?)]
                    [dests (listof path-string?)]
                    [ci collect-info?])
           resolve-info?]{

Performs the @techlink{resolve pass}. See @method[render% render] for
information on the @scheme[dests] argument.}

@defmethod[(render [srcs (listof part?)]
                   [dests (listof path-string?)]
                   [ri resolve-info?])
           void?]{

Produces the final output.

The @scheme[dests] provide names of files for Latex or single-file
HTML output, or names of sub-directories for multi-file HTML output.
If the @scheme[dests] are relative, they're relative to the current
directory; normally, they should indicates a path within the
@scheme[_dest-dir] supplied on initialization of the @scheme[render%]
object.}

@defmethod[(serialize-info [ri resolve-info?])
           any/c]{

Serializes the collected info in @scheme[ri].}

@defmethod[(deserialize-info [v any/c]
                             [ci collect-info?]
                             [#:root root-path (or/c path-string? false/c) #f])
           void?]{

Adds the deserialized form of @scheme[v] to @scheme[ci].

If @scheme[root-path] is not @scheme[#f], then file paths that are
recorded in @scheme[ci] as relative to an instantiation-supplied
@scheme[root-path] are deserialized as relative instead to the given
@scheme[root-path].}

}

@; ----------------------------------------

@section{Text Renderer}

@defmodule/local[scribble/text-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @scheme[render%] class for generating plain text.}}

@; ----------------------------------------

@section{HTML Renderer}

@defmodule/local[scribble/html-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @scheme[render%] class for generating HTML output.

@defmethod[(set-external-tag-path [url string?]) void?]{

Configures the renderer to redirect links to external via
@scheme[url], adding a @scheme[tag] query element to the end of the
URL that contains the Base64-encoded, @scheme[print]ed, serialized
original tag (in the sense of @scheme[link-element]) for the link.}

}

@defmixin[render-multi-mixin (render%) ()]{

Further specializes a rendering class produced by
@scheme[render-mixin] for generating multiple HTML
files.}

}

@; ----------------------------------------

@section{Latex Renderer}

@defmodule/local[scribble/latex-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @scheme[render%] class for generating Latex input.}}
