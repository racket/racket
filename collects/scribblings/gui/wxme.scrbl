#lang scribble/doc
@(require "common.rkt"
          (for-label wxme wxme/editor wxme/image racket/snip
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

@defmodule[wxme]{The @racketmodname[wxme] library provides tools for
reading @tech{WXME} @racket[editor<%>]-format files (see
@secref["editorfileformat"]) without the @racket[racket/gui] library.}


@defproc[(is-wxme-stream? [in input-port?]) boolean?]{

Peeks from @racket[in] and returns @racket[#t] if it starts with the
magic bytes indicating a @tech{WXME}-format stream (see
@secref["editorfileformat"]), @racket[#f] otherwise.}


@defproc[(wxme-port->text-port [in input-port?] [close? any/c #t])
         input-port?]{

Takes an input port whose stream starts with @tech{WXME}-format data
and returns an input port that produces a text form of the WXME
content, like the result of opening a WXME file in DrRacket and saving
it as text. 

Unlike @racket[wxme-port->port], this function may take liberties
with the snips in a way that would render a valid program invalid.
For example, if the wxme stream @racket[in] contains
a bitmap image, then there may not be a reasonable text-only version
of it and thus @racket[wxme-port->port] might turn what would have been
a valid Racket program into text that is a syntax error,
Nevertheless, the result may still be useful for human readers or 
approximate program-processing tools that run only in a GUI-less context.

If @racket[close?] is true, then closing the result port closes the
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
@racket[read-char-or-special]).

These special values produced by the new input port are different than
the ones produced by reading a file into an @racket[editor<%>]
object. Instead of instances of the @racket[snip%], the special values
are typically simple extensions of @racket[object%].  See
@secref["snipclassmapping"] for information about the kinds of
non-text content that can be read.

If @racket[close?] is true, then closing the result port close the
original port.

The @racket[snip-filter] procedure is applied to any special value
generated for the stream, and its result is used as an alternate
special value.

If a special value (possibly produced by the filter procedure) is an
object implementing the @racket[readable<%>] interface, then the
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
                                [mod-path (cons/c 'lib (listof string?))])
         void?]{

Maps a snip-class name to a quoted module path that provides a
@racket[reader%] implementation. The module path must have the form
@racket['(lib #,(racket _string ...))], where each @racket[_string]
contains only alpha-numeric ASCII characters, @litchar{.},
@litchar{_}, @litchar{-}, and spaces.}


@defproc[(string->lib-path [str string?] [gui? any/c])
         (cons/c 'lib (listof string?))]{

Returns a quoted module path for @racket[str] for either
@racket[editor<%>] mode when @racket[gui?] is true, or
@racketmodname[wxme] mode when @racket[gui?] is @racket[#f]. For the
latter, built-in mappings and mapping registered via
@racket[register-lib-mapping!] are used. If @racket[str] cannot be
parsed as a library path, and if no mapping is available (either
because the class is built-in or not known), the result is
@racket[#f].}


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

Like @racket[read], but for a stream that starts with
@tech{WXME}-format data. If multiple S-expressions are in the
@tech{WXME} data, they are all read and combined with
@racket['begin].

If @racket[racket/gui/base] is available (as determined by
@racket[gui-available?]), then @racket[open-input-text-editor] is
used. Otherwise, @racket[wxme-port->port] is used.}


@defproc[(wxme-read-syntax [source-v any/c] [in input-port?])
         (or/c syntax? eof-object?)]{

Like @racket[read-syntax], but for a @tech{WXME}-format input stream.
If multiple S-expressions are in the @tech{WXME} data, they are all
read and combined with @racket['begin].

If @racket[racket/gui/base] is available (as determined by
@racket[gui-available?]), then @racket[open-input-text-editor] is
used. Otherwise, @racket[wxme-port->port] is used.}


@definterface[snip-reader<%> ()]{

An interface to be implemented by a reader for a specific kind of data
in a @tech{WXME} stream. The interface has two methods:
@method[snip-reader<%> read-header] and @method[snip-reader<%> read-snip].

@defmethod[(read-header [version exact-nonnegative-integer?]
                        [stream (is-a?/c stream<%>)])
           any]{

Called at most once per @tech{WXME} stream to initialize the data
type's stream-specific information. This method usually does nothing.}

@defmethod[(read-snip [text-only? boolean?]
                      [version exact-nonnegative-integer?]
                      [stream (is-a?/c stream<%>)])
           (if text-only?
               bytes?
               any/c)]{

Called when an instance of the data type is encountered in the
stream. This method reads the data and returns either bytes to be
returned as part of the decoded stream or any other kind of value to
be returned as a ``special'' value from the decoded stream. The result
value can optionally be an object that implements
@racket[readable<%>].

The @racket[text-only?] argument is @racket[#f] when 
@racket[wxme-port->text-port] was called and @racket[#t]
when @racket[wxme-port->port] was called.
}

}

@definterface[readable<%> ()]{

An interface to be implemented by values returned from a snip reader.
The only method is @method[readable<%> read-special].

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? #f)]
                         [column (or/c exact-nonnegative-integer? #f)]
                         [position (or/c exact-nonnegative-integer? #f)])
           any/c]{

Like @method[readable-snip<%> read-special], but for non-graphical
mode. When a value implements this interface, its @method[readable<%>
read-special] method is called with source-location information to
obtain the ``special'' result from the @tech{WXME}-decoding port.}

}

@definterface[stream<%> ()]{

Represents a @tech{WXME} input stream for use by
@racket[snip-reader<%>] instances.

@defmethod[(read-integer [what any/c]) exact-integer?]{

Reads an exact integer, analogous to @method[editor-stream-in%
get-exact].

The @racket[what] field describes what is being read, for
error-message purposes, in case the stream does not continue with an
integer.}

@defmethod[(read-fixed-integer [what any/c]) exact-integer?]{

Reads an exact integer that has a fixed size in the stream, analogous
to @method[editor-stream-in% get-fixed].

The @racket[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-inexact [what any/c]) (and/c real? inexact?)]{

Reads an inexact real number, analogous to @method[editor-stream-in%
get-inexact].

The @racket[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-raw-bytes [what any/c]) bytes?]{

Reads raw bytes, analogous to @method[editor-stream-in%
get-unterminated-bytes].

The @racket[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-bytes [what any/c]) bytes?]{

Reads raw bytes, analogous to @method[editor-stream-in% get-bytes].

The @racket[what] argument is as for @method[stream<%> read-integer].}

@defmethod[(read-editor [what any/c]) input-port?]{

Reads a nested editor, producing a new input port to extract the
editor's content.

The @racket[what] argument is as for @method[stream<%> read-integer].}
}

@defproc[(read-snip-from-port [name string?]
                              [who any/c]
                              [stream (is-a?/c stream<%>)])
         bytes?]{
  Given @racket[name], which is expected to be the name of a snipclass,
  uses that snipclass to read from the given stream at the current point
  in that stream. Returns the processed bytes, much like the
  @method[snip-reader<%> read-snip] method.
}

@; ----------------------------------------------------------------------

@section[#:tag "snipclassmapping"]{Snip Class Mapping}

When graphical data is marshaled to the WXME format, it is associated
with a snip-class name to be matched with an implementation at load
time. See also @secref["editorsnipclasses"].

Ideally, the snip-class name is generated as

@racketblock[
(format "~s" (list '(lib #,(racket _string ...))
                   '(lib #,(racket _string ...))))
]

where each element of the @racket[format]ed list is a quoted module
path (see @racket[module-path?]). The @racket[_string]s must contain only
alpha-numeric ASCII characters, plus @litchar{.}, @litchar{_},
@litchar{-}, and spaces, and they must not be @racket["."] or
@racket[".."].

In that case, the first quoted module path is used for loading
@tech{WXME} files in graphical mode; the corresponding module must
provide @racketidfont{snip-class} object that implements the
@racket[snip-class%] class. The second quoted module path is used by
the @racketmodname[wxme] library for converting @tech{WXME} streams
without graphical support; the corresponding module must provide a
@racketidfont{reader} object that implements the @racket[snip-reader<%>]
interface. Naturally, the @racket[snip-class%] instance and
@racket[snip-reader<%>] instance are expected to parse the same format, but
generate different results suitable for the different contexts (i.e.,
graphical or not).

If a snip-class name is generated as

@racketblock[
(format "~s" '(lib #,(racket _string ...)))
]

then graphical mode uses the sole module path, and
@racketmodname[wxme] needs a compatibility mapping. Install one with
@racket[register-lib-mapping!].

If a snip-class name has neither of the above formats, then graphical
mode can use the data only if a snip class is registered for the name,
or if it the name of one of the built-in classes: @racket["wxtext"],
@racket["wxtab"], @racket["wximage"], or @racket["wxmedia"] (for
nested editors). The @racketmodname[wxme] library needs a
compatibility mapping installed with @racket[register-lib-mapping!]
if it is not one of the built-in classes.

Several compatibility mappings are installed automatically for the
@racketmodname[wxme] library. They correspond to popular graphical
elements supported by various versions of DrRacket, including comment
boxes, fractions, XML boxes, Racket boxes, text boxes, and images
generated by the @racketmodname[htdp/image] teachpack (or, more
generally, from @racketmodname[mrlib/cache-image-snip]), and test-case
boxes.

For a port created by @racket[wxme-port->port], nested editors are
represented by instances of the @racket[editor%] class provided by the
@racketmodname[wxme/editor] library. This class provides a single
method, @method[editor% get-content-port], which returns a port for
the editor's content. Images are represented as instances of the
@racket[image%] class provided by the @racketmodname[wxme/image]
library.

Comment boxes are represented as instances of a class that extends
@racket[editor%] to implement @racket[readable<%>]; see
@racketmodname[wxme/comment]. The read form produces a special comment
(created by @racket[make-special-comment]), so that the comment box
disappears when @racket[read] is used to read the stream; the
special-comment content is the readable instance. XML, Racket, and
text boxes similarly produce instances of @racket[editor%] and
@racket[readable<%>] that expand in the usual way; see
@racketmodname[wxme/xml], @racketmodname[wxme/scheme], and
@racket[wxme/text]. Images from the 
@racketmodname[htdp/image] teachpack
are packaged as instances of @racket[cache-image%] from the
@racketmodname[wxme/cache-image] library. Test-case boxes are packaged
as instances of @racket[test-case%] from the
@racketmodname[wxme/test-case] library.

@; ----------------------------------------

@subsection{Nested Editors}

@defmodule[wxme/editor]

@defclass[editor% object% ()]{

Instantiated for plain nested editors in a @tech{WXME} stream in text
mode.

@defmethod[(get-content-port) input-port?]{

Returns a port (like the one from @racket[wxme-port->port]) for the
editor's content.}

}

@; ----------------------------------------

@subsection{Images}

@defmodule[wxme/image]

@defclass[image% image-snip% ()]{

Instantiated for images in a @tech{WXME} stream in text mode.
This class can just be treated like @racket[image-snip%] and should
behave just like it, except it has the methods below in addition
in case old code still needs them. In other words, the methods
below are provided for backwards compatibility with earlier 
verisons of Racket.

@defmethod[(get-data) (or/c bytes? #f)]{

Returns bytes for a PNG, XBM,or XPM file for the image.}

@defmethod[(get-w) (or/c exact-nonnegative-integer? -1)]{

Returns the display width of the image, which may differ from the
width of the actual image specified as data or by a filename; -1 means
that the image data's width should be used.}

@defmethod[(get-h) (or/c exact-nonnegative-integer? -1)]{

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

@section{DrRacket Comment Boxes}

@defmodule[wxme/comment]

@in[wxme/comment
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for comment boxes.}]


@defclass[comment-editor% editor% (readable<%>)]{

Instantiated for DrRacket comment boxes in a @tech{WXME} stream for
text mode.

@defmethod[(get-data) #f]{

No data is available.

}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? #f)]
                         [column (or/c exact-nonnegative-integer? #f)]
                         [position (or/c exact-nonnegative-integer? #f)])
           any/c]{

Generates a special comment using @racket[make-special-comment]. The
special comment contains the comment text.}

}

@; ----------------------------------------

@section{DrRacket XML Boxes}

@defmodule[wxme/xml]

@in[wxme/xml
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for XML boxes.}]


@defclass[xml-editor% editor% (readable<%>)]{

Instantiated for DrRacket XML boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) any/c]{

Returns @racket[#t] if whitespace is elimited from the contained XML
literal, @racket[#f] otherwise.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? #f)]
                         [column (or/c exact-nonnegative-integer? #f)]
                         [position (or/c exact-nonnegative-integer? #f)])
           any/c]{

Generates a @racket[quasiquote] S-expression that enclosed the XML,
with @racket[unquote] and @racket[unquote-splicing] escapes for nested
Racket boxes.}

}

@; ----------------------------------------

@section{DrRacket Racket Boxes}

@defmodule[wxme/scheme]

@in[wxme/scheme
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for Racket boxes.}]


@defclass[racket-editor% editor% (readable<%>)]{

Instantiated for DrRacket Racket boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) any/c]{

Returns @racket[#t] if the box corresponds to a splicing unquote,
@racket[#f] for a non-splicing unquote.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? #f)]
                         [column (or/c exact-nonnegative-integer? #f)]
                         [position (or/c exact-nonnegative-integer? #f)])
           any/c]{

Generates an S-expression for the code in the box.}

}

@; ----------------------------------------

@section{DrRacket Text Boxes}

@defmodule[wxme/text]

@in[wxme/text
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for text boxes.}]


@defclass[text-editor% editor% (readable<%>)]{

Instantiated for DrRacket text boxes in a @tech{WXME} stream for text
mode.

@defmethod[(get-data) #f]{

No data is available.}

@defmethod[(read-special [source any/c]
                         [line (or/c exact-nonnegative-integer? #f)]
                         [column (or/c exact-nonnegative-integer? #f)]
                         [position (or/c exact-nonnegative-integer? #f)])
           any/c]{

Generates a string containing the text.}

}

@; ----------------------------------------

@section{DrRacket Fractions}

@defmodule[wxme/number]

@in[wxme/number
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for DrRacket fractions that generates exact,
rational numbers.}]

@; ----------------------------------------

@section{DrRacket Teachpack Images}

@defmodule[wxme/cache-image]

@in[wxme/cache-image
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for images in a WXME stream generated by the
@racketmodname[htdp/image] teachpack---or, more generally, by
@racketmodname[mrlib/cache-image-snip].}]


@defclass[cache-image% object% ()]{

Instantiated for DrRacket teachpack boxes in a @tech{WXME} stream for
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

@section{DrRacket Test-Case Boxes}

@defmodule[wxme/test-case]

@in[wxme/test-case
@defthing[reader (is-a?/c snip-reader<%>)]{

A text-mode reader for DrRacket test-case boxes in a WXME stream. It
generates instances of @racket[test-case%].}]

@defclass[test-case% object% ()]{

Instantiated for old-style DrRacket test-case boxes in a @tech{WXME}
stream for text mode.

@defmethod[(get-comment) (or/c #f input-port?)]{

Returns a port for the comment field, if any.}

@defmethod[(get-test) input-port?]{

Returns a port for the ``test'' field.}

@defmethod[(get-expected) input-port?]{

Returns a port for the ``expected'' field.}

@defmethod[(get-should-raise) (or/c #f input-port?)]{

Returns a port for the ``should raise'' field, if any.}

@defmethod[(get-error-message) (or/c #f input-port?)]{

Returns a port for the ``error msg'' field, if any.}

@defmethod[(get-enabled?) boolean?]{

Returns @racket[#t] if the test is enabled.}

@defmethod[(get-collapsed?) boolean?]{

Returns @racket[#t] if the test is collapsed.}

@defmethod[(get-error-box?) boolean?]{

Returns @racket[#t] if the test is for an exception.}

}
