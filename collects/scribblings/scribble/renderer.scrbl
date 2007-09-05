#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]
@require-for-label[(lib "class.ss")]

@title[#:tag "renderer"]{Renderer}

A renderer is an object that provides two main methods:
@scheme[collect] and @scheme[render]. The first method is called to
collect global information about the document, including information
that spans multiple documents rendered together; the collection pass
tends to be format-independent, and it usually implemented completely
by the base renderer. The latter method generates the actual output,
which is naturally specific to a particular format.

The @file{base-render.ss} module provides @scheme[render%], which
implements the core of a renderer. The @file{html-renderer.ss},
@file{latex-renderer.ss}, and @file{text-renderer.ss} modules each
provide @scheme[renderer-mixin] to extend the base. The
@file{html-renderer.ss} module also provides
@scheme[multi-renderer-mixin] to produce multi-file HTML instead
instead of single-file HTML.

The mixin structure is meant to support document-specific extensions
to the renderers. For example, the @exec{scribble} command-line tool
might, in the future, extract rendering mixins from a document module
(in addition to the document proper).

See @file{base-render.ss} for more information about the methods of
the renderer. Documents built with higher layers, such as
@file{manual.ss}, generally do not call the render object's methods
directly.

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
