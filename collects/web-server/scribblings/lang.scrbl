#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "lang"
       #:style 'toc]{Web Language Servlets}

The @web-server allows servlets to be written in a special Web
language that is nearly identical to Scheme. Herein we discuss how it
is different and what API is provided.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "lang-servlets"]{Definition}
@(require (for-label "dummy-language-servlet.ss")) @; to give a binding context

@defmodule*/no-declare[(web-server/lang)]

@declare-exporting[#:use-sources (web-server/scribblings/dummy-language-servlet)]

A @defterm{Web language servlet} is a module written in the
@schememodname[web-server/lang] language. The servlet module should
provide the following function:

@defproc[(start [initial-request request?])
         response?]{
 Called when this servlet is invoked.
 The argument is the HTTP request that initiated the servlet.
}
                   
The only way to run Web language servlets currently is to use the
functional interface to starting the server and create a dispatcher
that includes a @scheme[make-lang-dispatcher] dispatcher.

@; ------------------------------------------------------------
@section[#:tag "considerations"]{Usage Considerations}

A servlet has the following process performed on it automatically:
@itemize[
 @item{All uses of @scheme[letrec] are removed and replaced with equivalent uses of
       @scheme[let] and imperative features. (@filepath{lang/elim-letrec.ss})}
 @item{The program is converted into ANF (Administrative Normal Form),
       making all continuations explicit. (@filepath{lang/anormal.ss})}
 @item{All continuations (and other continuations marks) are recorded in the
       continuation marks of the expression
       they are the continuation of. (@filepath{lang/elim-callcc.ss})}
 @item{All calls to external modules are identified and marked.
       (@filepath{lang/elim-callcc.ss})}
 @item{All uses of @scheme[call/cc] are removed and replaced with
       equivalent gathering of the continuations through the continuation-marks.
       (@filepath{lang/elim-callcc.ss})}
 @item{The program is defunctionalized with a serializable data-structure for each
       anonymous lambda. (@filepath{lang/defun.ss})}
]

This process allows the continuations captured by your servlet to be serialized.
This means they may be stored on the client's browser or the server's disk.
Thus, your servlet has no cost to the server other than execution. This is
very attractive if you've used Scheme servlets and had memory problems.

This process IS defined on all of PLT Scheme and occurs AFTER macro-expansion,
so you are free to use all interesting features of PLT Scheme. However, there
are some considerations you must make.

First, this process drastically changes the structure of your program. It
will create an immense number of lambdas and structures your program
did not normally contain. The performance implication of this has not been
studied with PLT Scheme. However, it is theoretically a benefit. The main
implications would be due to optimizations MzScheme attempts to perform
that will no longer apply. Ideally, your program should be optimized first.

Second, the defunctionalization process is sensitive to the syntactic structure
of your program. Therefore, if you change your program in a trivial way, for example,
changing a constant, then all serialized continuations will be obsolete and will
error when deserialization is attempted. This is a feature, not a bug!

Third, the values in the lexical scope of your continuations must be serializable
for the continuations itself to be serializable. This means that you must use
@scheme[define-serializable-struct] rather than @scheme[define-struct], and take
care to use modules that do the same. Similarly, you may not use @scheme[parameterize],
because parameterizations are not serializable.

Fourth, and related, this process only runs on your code, not on the code you
@scheme[require]. Thus, your continuations---to be capturable---must not
be in the context of another module. For example, the following will not work:
@schemeblock[
 (define requests
   (map (lambda (rg) (send/suspend/url rg))
        response-generators))
]
because @scheme[map] is not transformed by the process. However, if you defined
your own @scheme[map] function, there would be no problem.

Fifth, the store is NOT serialized. If you rely on the store you will
be taking huge risks. You will be assuming that the serialized continuation
is invoked before the server is restarted or the memory is garbage collected.

This process is derived from the paper
@href-link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/" "\"Continuations from Generalized Stack Inspection\""].
We thank Greg Pettyjohn for his initial implementation of this algorithm.

@; ------------------------------------------------------------
@section[#:tag "reprovided"]{Reprovided API}

The APIs from @scheme[net/url], @secref["request-structs.ss"],
@secref["response-structs.ss"], and @secref["helpers.ss"] are reprovided
by the Web language API.

@; ------------------------------------------------------------
@section[#:tag "lang/web.ss"]{Web}
@(require (for-label web-server/lang/web))

@defmodule[web-server/lang/web]

@filepath{lang/web.ss} provides the most basic Web functionality.

@defproc[(send/suspend/url [response-generator (url? . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into
 a URL. Calls @scheme[response-generator] with this URL and delivers
 the response to the client. If the URL is invoked
 the request is returned to this continuation.
}

@defproc[(send/suspend/hidden [response-generator (url? xexpr? . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and generates an INPUT
 form that includes the serialization as a hidden form.
 Calls @scheme[response-generator] with this URL and form field and delivers
 the response to the client. If the URL is invoked with form data containing
 the hidden form,
 the request is returned to this continuation.

 Note: The continuation is NOT stuffed.
}

@defproc[(send/suspend/dispatch [make-response (embed/url/c . -> . response?)])
         any/c]{
 Calls @scheme[make-response] with a function that, when called with a procedure from
 @scheme[request?] to @scheme[any/c] will generate a URL, that when invoked will call
 the function with the @scheme[request?] object and return the result to the caller of
 @scheme[send/suspend/dispatch].
}

@; ------------------------------------------------------------
@section[#:tag "lang/stuff-url.ss"]{Stuff URL}
@(require (for-label web-server/lang/stuff-url))

@defmodule[web-server/lang/stuff-url]

@filepath{lang/stuff-url.ss} provides an interface for "stuffing"
serializable values into URLs. Currently there is a particular
hard-coded behavior, but we hope to make it more flexible in
the future.

@defproc[(stuff-url [v serializable?]
                    [u url?])
         url?]{
 Serializes @scheme[v] and computes the MD5 of the serialized
 representation. The serialization of @scheme[v] is written to
 @filepath{$HOME/.urls/M} where `M' is the MD5. `M' is then
 placed in @scheme[u] as a URL param.
}

@defproc[(stuffed-url? [u url?])
         boolean?]{
 Checks if @scheme[u] appears to be produced by @scheme[stuff-url].
}

@defproc[(unstuff-url [u url?])
         serializable?]{
 Extracts the value previously serialized into @scheme[u] by @scheme[stuff-url].
}

In the future, we will offer the facilities to:
@itemize[
 @item{Optionally use the content-addressed storage.}
 @item{Use different hashing algorithms for the CAS.}
 @item{Encrypt the serialized value.}
 @item{Only use the CAS if the URL would be too long. (URLs may only be 1024 characters.)}
]

@; ------------------------------------------------------------
@section[#:tag "lang/web-extras.ss"]{Web Extras}
@(require (for-label web-server/lang/web-extras))

@defmodule[web-server/lang/web-extras]{The
@schememodname[web-server/lang/web-extras] library provides
@scheme[redirect/get] as
@schememodname[web-server/servlet/web] except it uses
@scheme[send/suspend/url].}

@deftogether[(
@defproc[(redirect/get) request?]
)]{

See @schememodname[web-server/servlet/web].}

@; ------------------------------------------------------------
@section[#:tag "lang/file-box.ss"]{File Boxes}
@(require (for-label web-server/lang/file-box))

@defmodule[web-server/lang/file-box]

As mentioned earlier, it is dangerous to rely on the store in
Web Language servlets, due to the deployment scenarios available
to them. @filepath{lang/file-box.ss} provides a simple API to replace
boxes in a safe way.

@defproc[(file-box? [v any/c])
         boolean?]{Checks if @scheme[v] is a file-box.}

@defproc[(file-box [p path?]
                   [v serializable?])
         file-box?]{
 Creates a file-box that is stored at @scheme[p], with the default
 contents of @scheme[v].
}

@defproc[(file-unbox [fb file-box?])
         serializable?]{
 Returns the value inside @scheme[fb]
}

@defproc[(file-box-set? [fb file-box?])
         boolean?]{
 Returns @scheme[#t] if @scheme[fb] contains a value.
}

@defproc[(file-box-set! [fb file-box?]
                        [v serializable?])
         void]{
 Saves @scheme[v] in the file represented by @scheme[fb].
}

@warning{If you plan on using a load-balancer, make sure your file-boxes
are on a shared medium.}

@; ------------------------------------------------------------
@section[#:tag "lang/web-param.ss"]{Web Parameters}
@(require (for-label web-server/lang/web-param))

@defmodule[web-server/lang/web-param]

As mentioned earlier, it is not easy to use @scheme[parameterize] in the
Web Language. @filepath{lang/web-param.ss} provides (roughly) the same
functionality in a way that is serializable. Like other serializable
things in the Web Language, they are sensitive to source code modification.

@defform[(make-web-parameter default)]{
 Expands to the definition of a web-parameter with
 @scheme[default] as the default value. A web-parameter is
 a procedure that, when called with zero arguments, returns @scheme[default]
 or the last value @scheme[web-parameterize]d in the dynamic context
 of the call.
}

@defproc[(web-parameter? [v any/c])
         boolean?]{
 Checks if @scheme[v] appears to be a web-parameter.
}

@defform[(web-parameterize ([web-parameter-expr value-expr] ...) expr ...)]{
 Runs @scheme[(begin expr ...)] such that the web-parameters that
 the @scheme[web-parameter-expr]s evaluate to are bound to the @scheme[value-expr]s.
 From the perspective of the @scheme[value-expr]s, this is like @scheme[let].
}

@; ------------------------------------------------------------
@section[#:tag "lang/web-cells.ss"]{Web Cells}
@(require (for-label web-server/lang/web-cells))

@defmodule[web-server/lang/web-cells]{The
@schememodname[web-server/lang/web-cells] library provides the same
API as @schememodname[web-server/servlet/web-cells], but in a way
compatible with the Web Language. The one difference is that
@scheme[make-web-cell] is syntax, rather than a function.}

@deftogether[(
@defproc[(web-cell? [v any/c])
         boolean?]
@defform[(make-web-cell default-expr)]
@defproc[(web-cell-ref [wc web-cell?])
         any/c]
@defproc[(web-cell-shadow [wc web-cell?]
                          [v any/c])
         void]
)]{

See @schememodname[web-server/servlet/web-cells].}
