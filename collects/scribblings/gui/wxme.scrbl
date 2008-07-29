#lang scribble/doc
@(require "common.ss"
          (for-label wxme
                     wxme/editor
                     wxme/image
                     (except-in wxme/comment reader)
                     (except-in wxme/xml reader)
                     (except-in wxme/scheme reader)
                     (except-in wxme/text reader)
                     (except-in wxme/test-case reader)
                     (except-in wxme/cache-image reader)))

@(define-syntax-rule (in mod . content)
  (begin
   (define-syntax-rule (intro)
     (begin (require (for-label mod))
            . content))
   (intro)))

@title{WXME Decoding}

@defmodule[wxme]{The @schememodname[wxme] library provides tools for
reading @tech{WXME} @scheme[editor<%>]-format files (see
@secref["editorfileformat"]) without the @scheme[scheme/gui] library
(i.e., using @exec{mzscheme} instead of @exec{mred}).}


@defproc[(is-wxme-stream? [in input-port?]) boolean?]{

Peeks from @scheme[in] and returns @scheme[#t] if it starts with the
magic bytes indicating a @tech{WXME}-format stream (see
@secref["editorfileformat"]), @scheme[#f] otherwise.}


@defproc[(wxme-port->text-port [in input-port?] [close? any/c #t]) 
         input-port?]{

Takes an input port whose stream starts with @tech{WXME}-format data
and returns an input port that produces a text form of the WXME
content, like the result of opening a WXME file in DrScheme and saving
it as text.

If @scheme[close?] is true, then closing the result port close the
original port.

See @secref["snipclassmapping"] for information about the kinds of
non-text content that can be read.}


@defproc[(wxme-port->port [in input-port?] 
                          [close? any/c #t]
                          [snip-filter (any/c . -> . any/c) (lambda (_x) _x)])
         input-port?]{
                          
Takes an input port whose stream starts with @tech{WXME}-format data
and returns an input port that produces text content converted to
bytes, and non-text content as ``special'' values (see
@scheme[read-char-or-special]).

These special values produced by the new input port are different than
the ones produced by reading a file into an @scheme[editor<%>]
object. Instead of instances of the @scheme[snip%], the special values
are typically simple extensions of @scheme[object%].  See
@secref["snipclassmapping"] for information about the kinds of
non-text content that can be read.

If @scheme[close?] is true, then closing the result port close the
original port.

The @scheme[snip-filter] procedure is applied to any special value
generated for the stream, and its result is used as an alternate
special value.

If a special value (possibly produced by the filter procedure) is an
object implementing the @scheme[readable<%>] interface, then the
object's @method[readable<%> read-special] method is called to produce
the special value.}


@defproc[(extract-used-classes [in input-port?])
         (values (listof string?)
                 (listof string?))]{

Returns two values: a list of snip-class names used by the given
stream, and a list of data-class names used by the stream. If the
stream is not a @tech{WXME} stream, the result is two empty lists. The
given stream is not closed, and only data for a @tech{WXME} stream (if
any) is consumed.}


@defproc[(register-lib-mapping! [str string?] 
                                [mod-path (cons/c (one-of/c 'lib) (listof string?))])
         void?]{

Maps a snip-class name to a quoted module path that provides a
@scheme[reader%] implementation. The module path must have the form
@scheme['(lib #,(scheme _string ...))], where each @scheme[_string]
contains only alpha-numeric ASCII characters, @litchar{.},
@litchar{_}, @litchar{-}, and spaces.}


@defproc[(string->lib-path [str string?] [gui? any/c])
         (cons/c (one-of/c 'lib) (listof string?))]{

Returns a quoted module path for @scheme[str] for either
@scheme[editor<%>] mode when @scheme[gui?] is true, or
@schememodname[wxme] mode when @scheme[gui?] is @scheme[#f]. For the
latter, built-in mappings and mapping registered via
@scheme[register-lib-mapping!] are used. If @scheme[str] cannot be
parsed as a library path, and if no mapping is available (either
because the class is built-in or not known), the result is
@scheme[#f].}


@defboolparam[unknown-extensions-skip-enabled skip?]{

A parameter. When set to #f (the default), an exception is raised when
an unrecognized snip class is encountered in a @tech{WXME}
stream. When set to a true value, instances of unrecognized snip
classes are simply omitted from the transformed stream.}


@defboolparam[broken-wxme-big-endian? big?]{

A parameter. Some old and short-lived @tech{WXME} formats depended on
the endian order of the machine where the file was saved. Set this
parameter to pick the endian order to use when reading the file; the
default is the current platform's endian order.}


@defproc[(wxme-read [in input-port?]) any/c]{

Like @scheme[read], but for a stream that starts with
@tech{WXME}-format data. If multiple S-expressions are in the
@tech{WXME} data, they are all read and combined with
@scheme['begin].

If @scheme[scheme/gui/base] is available (as determined by
@scheme[gui-available?]), then @scheme[open-input-text-editor] is
used. Otherwise, @scheme[wxme-port->port] is used.}


@defproc[(wxme-read-syntax [source-v any/c] [in input-port?])
         (or/c syntax? eof-object?)]{

Like @scheme[read-syntax], but for a @tech{WXME}-format input stream.
If multiple S-expressions are in the @tech{WXME} data, they are all
read and combined with @scheme['begin].

If @scheme[scheme/gui/base] is available (as determined by
@scheme[gui-available?]), then @scheme[open-input-text-editor] is
used. Otherwise, @scheme[wxme-port->port] is used.}


@definterface[snip-reader<%> ()]{

An interface to be implemented by a reader for a specific kind of data
in a @tech{WXME} stream. The interface has two methods:
@method[snip-reader<%> read-header] and @method[snip-reader<%> read-snip].

@defmethod[(read-header [version exact-nonnegative-integer?]
                        [stream (is-a?/c stream<%>)])
           any]{

Called at most once per @tech{WXME} stream to initialize the data
type's stream-specific information. This method usually does nothing.}

@defmethod[(read-snip [text-only? Boolean?]
                      [version exact-nonnegative-integer?]
                      [stream (is-a?/c stream<%>)])
           any/c]{

Called when an instance of the data type is encountered in the
stream. This method reads the data and returns either bytes to be
returned as part of the decoded stream or any other kind of value to
be returned as a ``special'' value from the decoded stream. The result
value can optionally be an object that implements
@scheme[readable<%>].}

}

@definterface[readable<%> ()]{

An interface to be implemented by values returned from a snip reader.
The only method is @method[readable<%> read-special].

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? false/c)]
                         [column (or/c exact-nonnegative-integer? false/c)]
                         [position (or/c exact-nonnegative-integer? false/c)])
           any/c]{

Like @method[readable-snip<%> read-special], but for non-graphical
mode. When a value implements this interface, its @method[readable<%>
read-special] method is called with source-location information to
obtain the ``special'' result from the @tech{WXME}-decoding port.}

}

@definterface[stream<%> ()]{

Represents a @tech{WXME} input stream for use by
@scheme[snip-reader<%>] instances.

@defmethod[(read-integer [what any/c]) exact-integer?]{

Reads an exact integer, analogous to @method[editor-stream-in%
get-exact].

The @scheme[what] field describes what is being read, for
error-message purposes, in case the stream does not continue with an
integer.}

@defmethod[(read-fixed-integer [what any/c]) exact-integer?]{

Reads an exact integer that has a fixed size in the stream, analogous
to @method[editor-stream-in% get-fixed].

The @scheme[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-inexact [what any/c]) (and/c real? inexact?)]{

Reads an inexact real number, analogous to @method[editor-stream-in%
get-inexact].

The @scheme[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-raw-bytes [what any/c]) bytes?]{

Reads raw bytes, analogous to @method[editor-stream-in%
get-unterminated-bytes].

The @scheme[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-bytes [what any/c]) bytes?]{

Reads raw bytes, analogous to @method[editor-stream-in% get-bytes].

The @scheme[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-editor [what any/c]) input-port?]{

Reads a nested editor, producing a new input port to extract the
editor's content.

The @scheme[what] argument is as for @method[stream<%> read-integer].}
}

@; ----------------------------------------------------------------------

@section[#:tag "snipclassmapping"]{Snip Class Mapping}

When graphical data is marshaled to the WXME format, it is associated
with a snip-class name to be matched with an implementation at load
time. See also @secref["editorsnipclasses"].

Ideally, the snip-class name is generated as

@schemeblock[
(format "~s" (list '(lib #,(scheme _string ...))
                   '(lib #,(scheme _string ...))))
]

where each element of the @scheme[format]ed list is a quoted module
path (see @scheme[module-path?]). The @scheme[_string]s must contain only
alpha-numeric ASCII characters, plus @litchar{.}, @litchar{_},
@litchar{-}, and spaces, and they must not be @scheme["."] or
@scheme[".."].

In that case, the first quoted module path is used for loading
@tech{WXME} files in graphical mode; the corresponding module must
provide @schemeidfont{snip-class} object that implements the
@scheme[snip-class%] class. The second quoted module path is used by
the @schememodname[wxme] library for converting @tech{WXME} streams
without graphical support; the corresponding module must provide a
@schemeidfont{reader} object that implements the @scheme[reader<%>]
interface. Naturally, the @scheme[snip-class%] instance and
@scheme[reader<%>] instance are expected to parse the same format, but
generate different results suitable for the different contexts (i.e.,
graphical or not).

If a snip-class name is generated as

@schemeblock[
(format "~s" '(lib #,(scheme _string ...)))
]

then graphical mode uses the sole module path, and
@schememodname[wxme] needs a compatibility mapping. Install one with
@scheme[register-lib-mapping!].

If a snip-class name has neither of the above formats, then graphical
mode can use the data only if a snip class is registered for the name,
or if it the name of one of the built-in classes: @scheme["wxtext"],
@scheme["wxtab"], @scheme["wximage"], or @scheme["wxmedia"] (for
nested editors). The @schememodname[wxme] library needs a
compatibility mapping installed with @scheme[register-lib-mapping!]
if it is not one of the built-in classes.

Several compatibility mappings are installed automatically for the
@schememodname[wxme] library. They correspond to popular graphical
elements supported by various versions of DrScheme, including comment
boxes, fractions, XML boxes, Scheme boxes, text boxes, and images
generated by the ``world'' and ``image'' teachpacks (or, more
generally, from @schememodname[mrlib/cache-image-snip]), and test-case
boxes.

For a port created by @scheme[wxme-port->port], nested editors are
represented by instances of the @scheme[editor%] class provided by the
@schememodname[wxme/editor] library. This class provides a single
method, @method[editor% get-content-port], which returns a port for
the editor's content. Images are represented as instances of the
@scheme[image%] class provided by the @schememodname[wxme/image]
library.

Comment boxes are represented as instances of a class that extends
@scheme[editor%] to implement @scheme[readable<%>]; see
@schememodname[wxme/comment]. The read form produces a special comment
(created by @scheme[make-special-comment]), so that the comment box
disappears when @scheme[read] is used to read the stream; the
special-comment content is the readable instance. XML, Scheme, and
text boxes similarly produce instances of @scheme[editor%] and
@scheme[readable<%>] that expand in the usual way; see
@schememodname[wxme/xml], @schememodname[wxme/scheme], and
@scheme[wxme/text]. Images from the ``world'' and ``image'' teachpacks
are packaged as instances of @scheme[cache-image%] from the
@schememodname[wxme/cache-image] library. Test-case boxes are packaged
as instances of @scheme[test-case%] from the
@schememodname[wxme/test-case] library.

@; ----------------------------------------

@subsection{Nested Editors}

@defmodule[wxme/editor]

@defclass[editor% object% ()]{

Instantiated for plain nested editors in a @tech{WXME} stream in text
mode.

@defmethod[(get-content-port) input-port?]{

Returns a port (like the one from @scheme[wxme-port->port]) for the
editor's content.}

}

@; ----------------------------------------

@subsection{Images}

@defmodule[wxme/image]

@defclass[image% object% ()]{

Instantiated for images in a @tech{WXME} stream in text mode.

@defmethod[(get-filename) (or/c bytes? false/c)]{

Returns a filename as bytes, or @scheme[#f] if data is available
instead.}

@defmethod[(get-data) (or/c bytes? false/c)]{

Returns bytes for a PNG, XBM,or XPM file for the image.}

@defmethod[(get-w) (or/c exact-nonnegative-integer? (one-of/c -1))]{

Returns the display width of the image, which may differ from the
width of the actual image specified as data or by a filename; -1 means
that the image data's width should be used.}

@defmethod[(get-h) (or/c exact-nonnegative-integer? (one-of/c -1))]{

Returns the display height of the image, which may differ from the
height of the actual image specified as data or by a filename; -1
means that the image data's height should be used.}

@defmethod[(get-dx) exact-integer?]{

Returns an offset into the actual image to be used
as the left of the display image.}

@defmethod[(get-dy) exact-integer?]{

Returns an offset into the actual image to be used as the top of the
display image.}

}

@; ----------------------------------------

@section{DrScheme Comment Boxes}

@defmodule[wxme/comment]

@in[wxme/comment
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for comment boxes.}]


@defclass[comment-editor% editor% (readable<%>)]{

Instantiated for DrScheme comment boxes in a @tech{WXME} stream for
text mode.

@defmethod[(get-data) false/c]{

No data is available.

}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? false/c)]
                         [column (or/c exact-nonnegative-integer? false/c)]
                         [position (or/c exact-nonnegative-integer? false/c)])
           any/c]{

Generates a special comment using @scheme[make-special-comment]. The
special comment contains the comment text.}

}

@; ----------------------------------------

@section{DrScheme XML Boxes}

@defmodule[wxme/xml]

@in[wxme/xml
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for XML boxes.}]


@defclass[xml-editor% editor% (readable<%>)]{

Instantiated for DrScheme XML boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) any/c]{

Returns @scheme[#t] if whitespace is elimited from the contained XML
literal, @scheme[#f] otherwise.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? false/c)]
                         [column (or/c exact-nonnegative-integer? false/c)]
                         [position (or/c exact-nonnegative-integer? false/c)])
           any/c]{

Generates a @scheme[quasiquote] S-expression that enclosed the XML,
with @scheme[unquote] and @scheme[unquote-splicing] escapes for nested
Scheme boxes.}

}

@; ----------------------------------------

@section{DrScheme Scheme Boxes}

@defmodule[wxme/scheme]

@in[wxme/scheme
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for Scheme boxes.}]


@defclass[scheme-editor% editor% (readable<%>)]{

Instantiated for DrScheme Scheme boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) any/c]{

Returns @scheme[#t] if the box corresponds to a splicing unquote,
@scheme[#f] for a non-splicing unquote.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? false/c)]
                         [column (or/c exact-nonnegative-integer? false/c)]
                         [position (or/c exact-nonnegative-integer? false/c)])
           any/c]{

Generates an S-expression for the code in the box.}

}

@; ----------------------------------------

@section{DrScheme Text Boxes}

@defmodule[wxme/text]

@in[wxme/text
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for text boxes.}]


@defclass[text-editor% editor% (readable<%>)]{

Instantiated for DrScheme text boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) false/c]{

No data is available.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? false/c)]
                         [column (or/c exact-nonnegative-integer? false/c)]
                         [position (or/c exact-nonnegative-integer? false/c)])
           any/c]{

Generates a string containing the text.}

}

@; ----------------------------------------

@section{DrScheme Fractions}

@defmodule[wxme/number]

@in[wxme/number
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for DrScheme fractions that generates exact,
rational numbers.}]

@; ----------------------------------------

@section{DrScheme Teachpack Images}

@defmodule[wxme/cache-image]

@in[wxme/cache-image
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for images in a WXME stream generated by the
``image'' and ``world'' teachpacks---or, more generally, by
@schememodname[mrlib/cache-image-snip].}]


@defclass[cache-image% object% ()]{

Instantiated for DrScheme teachpack boxes in a @tech{WXME} stream for
text mode.

@defmethod[(get-argb) (vectorof byte?)]{

Returns a vector of bytes representing the content of the image.}

@defmethod[(get-width) exact-nonnegative-integer?]{

Returns the width of the image.}

@defmethod[(get-height) exact-nonnegative-integer?]{

Returns the height of the image.}

@defmethod[(get-pin-x) exact-integer?]{

Returns an offset across into the image for the pinhole.}

@defmethod[(get-pin-y) exact-integer?]{

Returns an offset down into the image for the pinhole.}

}

@section{DrScheme Test-Case Boxes}

@defmodule[wxme/test-case]

@in[wxme/test-case
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for DrScheme test-case boxes in a WXME stream. It
generates instances of @scheme[test-case%].}]

@defclass[test-case% object% ()]{

Instantiated for old-style DrScheme test-case boxes in a @tech{WXME}
stream for text mode.

@defmethod[(get-comment) (or/c false/c input-port?)]{

Returns a port for the comment field, if any.}

@defmethod[(get-test) input-port?]{

Returns a port for the ``test'' field.}

@defmethod[(get-expected) input-port?]{

Returns a port for the ``expected'' field.}

@defmethod[(get-should-raise) (or/c false/c input-port?)]{

Returns a port for the ``should raise'' field, if any.}

@defmethod[(get-error-message) (or/c false/c input-port?)]{

Returns a port for the ``error msg'' field, if any.}

@defmethod[(get-enabled?) boolean?]{

Returns @scheme[#t] if the test is enabled.}

@defmethod[(get-collapsed?) boolean?]{

Returns @scheme[#t] if the test is collapsed.}

@defmethod[(get-error-box?) boolean?]{

Returns @scheme[#t] if the test is for an exception.}

}
