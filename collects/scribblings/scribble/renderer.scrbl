#lang scribble/doc
@(require scribble/manual 
          "utils.rkt" 
          (for-label racket/class
                     scribble/render
                     scribble/xref))

@(define-syntax-rule (defmodule/local lib . content)
   (begin
     (define-syntax-rule (intro)
       (begin
         (require (for-label lib))
         (defmodule lib)
         . content))
     (intro)))

@(begin
  (define-syntax-rule (def-html-render-mixin id)
    (begin
      (require (for-label scribble/html-render))
      (define id @racket[render-mixin])))
  (def-html-render-mixin html:render-mixin))
@(begin
  (define-syntax-rule (def-latex-render-mixin id)
    (begin
      (require (for-label scribble/latex-render))
      (define id @racket[render-mixin])))
  (def-latex-render-mixin latex:render-mixin))

@title[#:tag "renderer"]{Renderers}

A renderer is an object that provides four main methods:
@racket[traverse], @racket[collect], @racket[resolve], and
@racketidfont{render}. Each method corresponds to a pass described in
@secref["core"], and they are chained together by the @racket[render]
function to render a document.

@section{Rendering Driver}

@defmodule[scribble/render]

@defproc[(render [docs (listof part?)]
                 [names (listof path-string?)]
                 [#:render-mixin render-mixin (class? . -> . class?) @#,html:render-mixin]
                 [#:dest-dir dest-dir (or/c #f path-string?) #f]
                 [#:prefix-file prefix-file (or/c #f path-string?) #f]
                 [#:style-file style-file (or/c #f path-string?) #f]
                 [#:style-extra-files style-extra-files (listof path-string?) #f]
                 [#:extra-files extra-files (listof path-string?) #f]
                 [#:xrefs xrefs (listof xref?) null]
                 [#:info-in-files info-in-files (listof path-string?) null]
                 [#:info-out-file info-out-file (or/c #f path-string?) #f]
                 [#:redirect redirect (or/c #f string?) #f]
                 [#:redirect-main redirect-main (or/c #f string?) #f]
                 [#:quiet? quiet? any/c #t]
                 [#:warn-undefined? warn-undefined? any/c (not quiet?)])
          void?]{

Renders the given @racket[docs], each with an output name derived from
the corresponding element of @racket[names]. A directory path (if any)
for a name in @racket[names] is discarded, and the file suffix is
replaced (if any) with a suitable suffix for the output format.

The @racket[render-mixin] argument determines the output format. By
default, it is @html:render-mixin from @racketmodname[scribble/html-render].

The @racket[dest-dir] argument determines the output directory, which
is created using @racket[make-directory*] if it is non-@racket[#f] and
does not exist already.

The @racket[prefix-file], @racket[style-file],
@racket[extra-style-files], and @racket[extra-files] arguments are
passed on to the @racket[render%] constructor.

The @racket[xrefs] argument provides extra cross-reference information
to be used during the documents' @tech{resolve pass}. The
@racket[info-in-files] arguments supply additional cross-reference
information in serialized form. When the @racket[info-out-file]
argument is not @racket[#f], cross-reference information for the
rendered documents is written in serialized for to the specified file.

The @racket[redirect] and @racket[redirect-main] arguments correspond
to the @racket[set-external-tag-path] and
@racket[set-external-root-url] methods of @|html:render-mixin| from
@racketmodname[scribble/html-render], so they should be
non-@racket[#f] only for HTML rendering.

If @racket[quiet?] is a false value, output-file information is
written to the current output port.

If @racket[warn-undefined?] is a true value, then references to
missing cross-reference targets trigger a warning message on the
current error port.}


@section{Base Renderer}

@defmodule[scribble/base-render]{The
@racketmodname[scribble/base-render] module provides @racket[render%],
which implements the core of a renderer. This rendering class must be
refined with a mixin from @racketmodname[scribble/text-render],
@racketmodname[scribble/html-render], or
@racketmodname[scribble/latex-render].}

The mixin structure is meant to support document-specific extensions
to the renderers. For example, the @exec{scribble} command-line tool
might, in the future, extract rendering mixins from a document module
(in addition to the document proper).

See the @filepath{base-render.rkt} source for more information about
the methods of the renderer. Documents built with higher layers, such
as @racketmodname[scribble/manual], generally do not call the render
object's methods directly.

@defclass[render% object% ()]{

Represents a renderer.

@defconstructor[([dest-dir path-string?]
                 [refer-to-existing-files any/c #f]
                 [root-path (or/c path-string? #f) #f]
                 [prefix-file (or/c path-string? #f) #f]
                 [style-file (or/c path-string? #f) #f]
                 [style-extra-files (listof path-string?) null]
                 [extra-files (listof path-string?) null])]{

Creates a renderer whose output will go to @racket[dest-dir]. For
example, @racket[dest-dir] could name the directory containing the
output Latex file, the HTML file for a single-file output, or the
output sub-directory for multi-file HTML output.

If @racket[refer-to-existing-files] is true, then when a document
refers to external files, such as an image or a style file, then the
file is referenced from its source location instead of copied to the
document destination.

If @racket[root-path] is not @racket[#f], it is normally the same as
@racket[dest-dir] or a parent of @racket[dest-dir]. It causes
cross-reference information to record destination files relative to
@racket[root-path]; when cross-reference information is serialized, it
can be deserialized via @method[render% deserialize-info] with a
different root path (indicating that the destination files have
moved).

The @racket[prefix-file], @racket[style-file], and
@racket[style-extra-files] arguments set files that control output
styles in a formal-specific way; see @secref["config-style"] for more
information.

The @racket[extra-files] argument names files to be copied to the
output location, such as image files or extra configuration files.}


@defmethod[(traverse [srcs (listof part?)]
                     [dests (listof path-string?)])
           (and/c hash? immutable?)]{

Performs the @techlink{traverse pass}, producing a hash table that
contains the replacements for and @racket[traverse-block]s and
@racket[traverse-elements]s. See @method[render% render] for
information on the @racket[dests] argument.}

@defmethod[(collect [srcs (listof part?)]
                    [dests (listof path-string?)]
                    [fp (and/c hash? immutable?)])
           collect-info?]{

Performs the @techlink{collect pass}. See @method[render% render] for
information on the @racket[dests] argument. The @racket[fp] argument
is a result from the @method[render% traverse] method.}

@defmethod[(resolve [srcs (listof part?)]
                    [dests (listof path-string?)]
                    [ci collect-info?])
           resolve-info?]{

Performs the @techlink{resolve pass}. See @method[render% render] for
information on the @racket[dests] argument.  The @racket[ci] argument
is a result from the @method[render% collect] method.}

@defmethod[(render [srcs (listof part?)]
                   [dests (listof path-string?)]
                   [ri resolve-info?])
           void?]{

Produces the final output.  The @racket[ri] argument is a result from
the @method[render% render] method.

The @racket[dests] provide names of files for Latex or single-file
HTML output, or names of sub-directories for multi-file HTML output.
If the @racket[dests] are relative, they're relative to the current
directory; normally, they should indicates a path within the
@racket[_dest-dir] supplied on initialization of the @racket[render%]
object.}

@defmethod[(serialize-info [ri resolve-info?])
           any/c]{

Serializes the collected info in @racket[ri].}


@defmethod[(deserialize-info [v any/c]
                             [ci collect-info?]
                             [#:root root-path (or/c path-string? false/c) #f])
           void?]{

Adds the deserialized form of @racket[v] to @racket[ci].

If @racket[root-path] is not @racket[#f], then file paths that are
recorded in @racket[ci] as relative to an instantiation-supplied
@racket[root-path] are deserialized as relative instead to the given
@racket[root-path].}


@defmethod[(get-defined [ci collect-info?]) (listof tag?)]{

Returns a list of tags that were defined within the documents
represented by @racket[ci].}


@defmethod[(get-external [ri resolve-info?]) (listof tag?)]{

Returns a list of tags that were referenced but not defined within the
documents represented by @racket[ri] (though possibly found in
cross-reference information transferred to @racket[ri] via
@racket[xref-transfer-info]).}


@defmethod[(get-undefined [ri resolve-info?]) (listof tag?)]{

Returns a list of tags that were referenced by the resolved documents
with no target found either in the resolved documents represented by
@racket[ri] or cross-reference information transferred to @racket[ri]
via @racket[xref-transfer-info].

If multiple tags were referenced via @racket[resolve-search] and a
target was found for any of the tags using the same dependency key,
then no tag in the set is included in the list of undefined tags.}

}

@; ----------------------------------------

@section{Text Renderer}

@defmodule/local[scribble/text-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @racket[render%] class for generating plain text.}}

@; ----------------------------------------

@section{HTML Renderer}

@defmodule/local[scribble/html-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @racket[render%] class for generating HTML output.

@defmethod[(set-external-tag-path [url string?]) void?]{

Configures the renderer to redirect links to external via
@racket[url], adding a @racket[tag] query element to the end of the
URL that contains the Base64-encoded, @racket[print]ed, serialized
original tag (in the sense of @racket[link-element]) for the link.}

@defmethod[(set-external-root-url [url string?]) void?]{

Configures the renderer to redirect links to documents installed in
the distribution's documentation directory to the given URL, using the
URL as a replacement to the path of the distribution's document
directory.}

}

@defmixin[render-multi-mixin (render%) ()]{

Further specializes a rendering class produced by
@racket[render-mixin] for generating multiple HTML
files.}

}

@; ----------------------------------------

@section{Latex Renderer}

@defmodule/local[scribble/latex-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @racket[render%] class for generating Latex input.}}

@; ----------------------------------------

@section{PDF Renderer}

@defmodule/local[scribble/pdf-render]{

@defmixin[render-mixin (render%) ()]{

Specializes a @racket[render%] class for generating PDF output via
Latex, building on @|latex:render-mixin| from @racketmodname[scribble/latex-render].}}
