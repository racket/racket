#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "lang"
       #:style 'toc]{Web Language Servlets}

The @web-server allows servlets to be written in a special Web
language that is nearly identical to Scheme. Herein we discuss how it
is different and what API is provided.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "lang-servlets"]{Definition}

A @defterm{Web language servlet} is a module written in the
@scheme[(lib "lang.ss" "web-server")] module language. It should provide
the following identifier:

@defproc[(start [initial-request request?])
         response?]{
 This function is called when this servlet is invoked.
 The argument is the HTTP request that initiated the servlet.
}

@; ------------------------------------------------------------
@section[#:tag "considerations"]{Usage Considerations}


A servlet has the following process performed on it automatically:
@itemize[
 @item{All uses of @scheme[letrec] are removed and replaced with equivalent uses of
       @scheme[let] and imperative features. (@file{lang/elim-letrec.ss})}
 @item{The program is converted into ANF (Administrative Normal Form),
       making all continuations explicit. (@file{lang/anormal.ss})}
 @item{All continuations (and other continuations marks) are recorded in the
       continuation marks of the expression
       they are the continuation of. (@file{lang/elim-callcc.ss})}
 @item{All calls to external modules are identified and marked.
       (@file{lang/elim-callcc.ss})}
 @item{All uses of @scheme[call/cc] are removed and replaced with
       equivalent gathering of the continuations through the continuation-marks.
       (@file{lang/elim-callcc.ss})}
 @item{The program is defunctionalized with a serializable data-structure for each
       anonymous lambda. (@file{lang/defun.ss})}
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

The APIs from @scheme[(lib "url.ss" "net")], @secref["request-structs.ss"],
@secref["response-structs.ss"], and @secref["helpers.ss"] are reprovided
by the Web language API.

@; ------------------------------------------------------------
@section[#:tag "lang/web.ss"]{Web}

@file{lang/web.ss} provides the most basic Web functionality.

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

@defproc[(embed-proc/url [k-url url?]
                         [proc (request? . -> . any/c)])
         url?]{
 Serializes and stuffs @scheme[proc] into @scheme[k-url]. For use with
 @scheme[extract-proc/url].
}

@defproc[(extract-proc/url [req request?])
         any/c]{
 Inspects the URL of @scheme[req] and attempts to extract the procedure
 embedded with @scheme[embed-proc/url]. If successful, it is invoked with
 @scheme[req] as an argument.
}

@; ------------------------------------------------------------
@section[#:tag "lang/stuff-url.ss"]{Stuff URL}

@file{lang/stuff-url.ss} provides an interface for "stuffing"
serializable values into URLs. Currently there is a particular
hard-coded behavior, but we hope to make it more flexible in
the future.

@defproc[(stuff-url [v serializable?]
                    [u url?])
         url?]{
 Serializes @scheme[v] and computes the MD5 of the serialized
 representation. The serialization of @scheme[v] is written to
 @file{$HOME/.urls/M} where `M' is the MD5. `M' is then
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

@file{lang/web-extras.ss} provides @scheme[send/suspend/dispatch] and
@scheme[redirect/get] as @secref["web.ss"], except they use
@scheme[embed-proc/url] + @scheme[extract-proc/url] and
@scheme[send/suspend/url] respectively.

@; ------------------------------------------------------------
@section[#:tag "lang/file-box.ss"]{File Boxes}

As mentioned earlier, it is dangerous to rely on the store in
Web Language servlets, due to the deployment scenarios available
to them. @file{lang/file-box.ss} provides a simple API to replace
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

As mentioned earlier, it is not easy to use @scheme[parameterize] in the
Web Language. @file{lang/web-param.ss} provides (roughly) the same
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

@file{lang/web-cells.ss} provides the same API as @secref["web-cells.ss"],
but in a way compatible with the Web Language. The one difference is that
@scheme[make-web-cell] is syntax, rather than a function.
