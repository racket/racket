#lang scribble/doc
@(require "common.ss"
          scribble/bnf
          (for-label net/url
                     net/url-unit
                     net/url-sig
                     net/head
                     net/uri-codec))

@title[#:tag "url"]{URLs and HTTP}

@defmodule[net/url]{The @schememodname[net/url] library provides
utilities to parse and manipulate URIs, as specified in RFC 2396
@cite["RFC2396"], and to use the HTTP protocol.}

To access the text of a document from the web, first obtain its URL as
a string. Convert the address into a @scheme[url] structure using
@scheme[string->url]. Then, open the document using
@scheme[get-pure-port] or @scheme[get-impure-port], depending on
whether or not you wish to examine its MIME headers.  At this point,
you have a regular input port with which to process the document, as with
any other file. 

Currently the only supported protocols are @scheme["http"] and
sometimes @scheme["file"].

@section{URL Structure}

@declare-exporting[net/url-structs net/url]

@defmodule*/no-declare[(net/url-structs)]{The URL structure types are
provided by the @schememodname[net/url-structs] library, and
re-exported by @schememodname[net/url].}

@; ----------------------------------------


@defstruct[url ([scheme (or/c false/c string?)]
                [user (or/c false/c string?)]
                [host (or/c false/c string?)]
                [port (or/c false/c exact-nonnegative-integer?)]
                [path-absolute? boolean?]
                [path (listof path/param?)]
                [query (listof (cons/c symbol? (or/c false/c string?)))]
                [fragment (or/c false/c string?)])]{

The basic structure for all URLs, which is explained in RFC 3986
@cite["RFC3986"]. The following diagram illustrates the parts:

@verbatim[#:indent 2]|{
  http://sky@www:801/cgi-bin/finger;xyz?name=shriram;host=nw#top
  {-1}   {2} {3} {4}{---5---------} {6} {----7-------------} {8}

  1 = scheme, 2 = user, 3 = host, 4 = port,
  5 = path (two elements),  6 = param (of second path element),
  7 = query, 8 = fragment
}|

The strings inside the @scheme[user], @scheme[path], @scheme[query],
and @scheme[fragment] fields are represented directly as Scheme
strings, without URL-syntax-specific quoting. The procedures
@scheme[string->url] and @scheme[url->string] translate encodings such
as @litchar{%20} into spaces and back again.

By default, query associations are parsed with either @litchar{;} or
@litchar{&} as a separator, and they are generated with @litchar{&} as
a separator. The @scheme[current-alist-separator-mode] parameter
adjusts the behavior.

An empty string at the end of the @scheme[path] list corresponds to a
URL that ends in a slash. For example, the result of
@scheme[(string->url "http://racket-lang.org/a/")] has a
@scheme[path] field with strings @scheme["a"] and @scheme[""], while
the result of @scheme[(string->url "http://racket-lang.org/a")] has a
@scheme[path] field with only the string @scheme["a"].

When a @scheme["file"] URL is represented by a @scheme[url] structure,
the @scheme[path] field is mostly a list of path elements. For Unix
paths, the root directory is not included in @scheme[path]; its
presence or absence is implicit in the @scheme[path-absolute?] flag.
For Windows paths, the first element typically represents a drive, but
a UNC path is represented by a first element that is @scheme[""] and
then successive elements complete the drive components that are
separated by @litchar{/} or @litchar{\}.}

@defstruct[path/param ([path (or/c string? (one-of/c 'up 'same))]
                       [param (listof string?)])]{

A pair that joins a path segment with its params in a URL.}

@; ----------------------------------------

@section{URL Functions}

An HTTP connection is created as a @deftech{pure port} or a
@deftech{impure port}.  A pure port is one from which the MIME headers
have been removed, so that what remains is purely the first content
fragment. An impure port is one that still has its MIME headers.

@defproc[(string->url [str string?]) url?]{

Parses the URL specified by @scheme[str] into a @scheme[url]
struct. The @scheme[string->url] procedure uses
@scheme[form-urlencoded->alist] when parsing the query, so it is
sensitive to the @scheme[current-alist-separator-mode] parameter for
determining the association separator.

If @scheme[str] starts with @scheme["file:"], then the path is always
parsed as an absolute path, and the parsing details depend on
@scheme[file-url-path-convention-type]:

@itemize[

 @item{@scheme['unix] : If @scheme["file:"] is followed by
       @litchar{//} and a non-@litchar{/}, then the first element
       after the @litchar{//} is parsed as a host (and maybe port);
       otherwise, the first element starts the path, and the host is
       @scheme[""].}

 @item{@scheme['windows] : If @scheme["file:"] is followed by
       @litchar{//}, then the @litchar{//} is stripped; the remainder
       parsed as a Windows path. The host is always @scheme[""] and
       the port is always @scheme[#f].}

]}


@defproc[(combine-url/relative [base url?] [relative string?]) url?]{

Given a base URL and a relative path, combines the two and returns a
new URL as per the URL combination specification.  They are combined
according to the rules in RFC 3986 @cite["RFC3986"].

This function does not raise any exceptions.}


@defproc[(netscape/string->url [str string?]) url?]{

Turns a string into a URL, applying (what appear to be) Netscape's
conventions on automatically specifying the scheme: a string starting
with a slash gets the scheme @scheme["file"], while all others get the
scheme @scheme["http"].}


@defproc[(url->string [URL url?]) string?]{

Generates a string corresponding to the contents of a @scheme[url]
struct.  For a @scheme["file:"] URL, the URL must not be relative, the
result always starts @litchar{file://}, and the interpretation of the
path depends on the value of @scheme[file-url-path-convention-type]:

@itemize[

 @item{@scheme['unix] : Elements in @scheme[URL] are treated as path
       elements. Empty strings in the path list are treated like
       @scheme['same].}

 @item{@scheme['windows] : If the first element is @scheme[""] then
       the next two elements define the UNC root, and the rest of the
       elements are treated as path elements. Empty strings in the
       path list are treated like @scheme['same].}

]

The @scheme[url->string] procedure uses
@scheme[alist->form-urlencoded] when formatting the query, so it is
sensitive to the @scheme[current-alist-separator-mode] parameter for
determining the association separator. The default is to separate
associations with a @litchar{&}.}


@defproc[(path->url [path (or/c path-string? path-for-some-system?)])
         url?]{

Converts a path to a @scheme[url].}


@defproc[(url->path [URL url?]
                    [kind (one-of/c 'unix 'windows) (system-path-convention-type)])
         path-for-some-system?]{

Converts @scheme[URL], which is assumed to be a @scheme["file"] URL,
to a path.}


@defparam[file-url-path-convention-type kind (one-of/c 'unix 'windows)]{

Determines the default conversion to and from strings for
@scheme["file"] URLs. See @scheme[string->url] and @scheme[url->string].}


@deftogether[(
@defproc[(get-pure-port [URL url?]
                        [header (listof string?) null])
         input-port?]
@defproc[(head-pure-port [URL url?]
                         [header (listof string?) null])
         input-port?]
@defproc[(delete-pure-port [URL url?]
                           [header (listof string?) null])
         input-port?]
)]{

Initiates a GET/HEAD/DELETE request for @scheme[URL] and returns a
@tech{pure port} corresponding to the body of the response. The
optional list of strings can be used to send header lines to the
server.

The GET method is used to retrieve whatever information is identified
by @scheme[URL].

The HEAD method is identical to GET, except the server must not return
a message body. The meta-information returned in a response to a HEAD
request should be identical to the information in a response to a GET
request.

The DELETE method is used to delete the entity identified by
@scheme[URL].

The @scheme["file"] scheme for URLs is handled only by
@scheme[get-pure-port], which uses @scheme[open-input-file], does not
handle exceptions, and ignores the optional strings.}

@deftogether[(
@defproc[(get-impure-port [URL url?]
                          [header (listof string?) null])
         input-port?]
@defproc[(head-impure-port [URL url?]
                           [header (listof string?) null])
         input-port?]
@defproc[(delete-impure-port [URL url?]
                             [header (listof string?) null])
         input-port?]
)]{

Like @scheme[get-pure-port], etc., but the resulting @tech{impure
port} contains both the returned headers and the body. The
@scheme["file"] URL scheme is not handled by these functions.}

@deftogether[(
@defproc[(post-pure-port [URL url?]
                         [post bytes?]
                         [header (listof string?) null])
         input-port?]
@defproc[(put-pure-port [URL url?]
                        [post bytes?]
                        [header (listof string?) null])
         input-port?]
)]{

Initiates a POST/PUT request for @scheme[URL] and sends the
@scheme[post] byte string.  The result is a @tech{pure port}, which
contains the body of the response is returned.  The optional list of
strings can be used to send header lines to the server.}

@deftogether[(
@defproc[(post-impure-port [URL url?]
                           [post bytes?]
                           [header (listof string?) null])
         input-port?]
@defproc[(put-impure-port [URL url?]
                          [post bytes?]
                          [header (listof string?) null])
         input-port?]
)]{

Like @scheme[post-pure-port] and @scheme[put-pure-port], but the
resulting @tech{impure port} contains both the returned headers and
body.}


@defproc[(display-pure-port [in input-port?]) void?]{

Writes the output of a pure port, which is useful for debugging purposes.}


@defproc[(purify-port [in input-port?]) string?]{

Purifies a port, returning the MIME headers, plus a leading line for
the form @litchar{HTTP/}@nonterm{vers}@litchar{
}@nonterm{code}@litchar{ }@nonterm{message}, where @nonterm{vers} is
something like @litchar{1.0} or @litchar{1.1}, @nonterm{code} is an
exact integer for the response code, and @nonterm{message} is
arbitrary text without a return or newline.

The @schememodname[net/head] library provides procedures, such as
@scheme[extract-field] for manipulating the header.

Since web servers sometimes return mis-formatted replies,
@scheme[purify-port] is liberal in what it accepts as a header. as a
result, the result string may be ill formed, but it will either be the
empty string, or it will be a string matching the following regexp:

@schemeblock[
#rx"^HTTP/.*?(\r\n\r\n|\n\n|\r\r)"
]}


@defproc*[([(call/input-url [URL url?]
                            [connect (url? . -> . input-port?)]
                            [handle (input-port? . -> . any)])
            any]
           [(call/input-url [URL url?]
                            [connect (url? (listof string?) . -> . input-port?)]
                            [handle (input-port? . -> . any)]
                            [header (listof string?)])
            any])]{

Given a URL and a @scheme[connect] procedure like
@scheme[get-pure-port] to convert the URL to an input port (either a
@tech{pure port} or @tech{impure port}), calls the @scheme[handle]
procedure on the port and closes the port on return. The result of the
@scheme[handle] procedure is the result of @scheme[call/input-url].

When a @scheme[header] argument is supplied, it is passed along to the
@scheme[connect] procedure.

The connection is made in such a way that the port is closed before
@scheme[call/input-url] returns, no matter how it returns. In
particular, it is closed if @scheme[handle] raises an exception, or if
the connection process is interruped by an asynchronous break
exception.}


@defparam[current-proxy-servers mapping (listof (list/c string? string? (integer-in 0 65535)))]{

A parameter that determines a mapping of proxy servers used for
connections. Each mapping is a list of three elements:

@itemize[

 @item{the URL scheme, such as @scheme["http"];}

 @item{the proxy server address; and}

 @item{the proxy server port number.}

]

Currently, the only proxiable scheme is @scheme["http"]. The default
mapping is the empty list (i.e., no proxies).}

@; ----------------------------------------

@section{URL Unit}

@defmodule[net/url-unit]

@defthing[url@ unit?]{

Imports @scheme[tcp^], exports @scheme[url^].}

@; ----------------------------------------

@section{URL Signature}

@defmodule[net/url-sig]

@defsignature[url^ ()]{

Includes everything exported by the @schememodname[net/url] module.}

