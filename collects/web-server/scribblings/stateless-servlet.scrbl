#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "stateless-servlets"]{Stateless Servlets}

@(require (for-label web-server/http
                     scheme/serialize
                     web-server/stuffers
                     (except-in "dummy-stateless-servlet.ss" stuffer))) @; to give a binding context
@declare-exporting[#:use-sources (web-server/scribblings/dummy-stateless-servlet)]

@defthing[interface-version (one-of/c 'stateless)]{
 This indicates that the servlet is a stateless servlet.
}

@defthing[stuffer (stuffer/c serializable? bytes?)]{
 This is the @scheme[stuffer] that will be used for the servlet.
      
 If it is not provided, it defaults to @scheme[default-stuffer].
}

@defproc[(start [initial-request request?])
         response/c]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

An example @scheme['stateless] servlet module:
@schememod[
 web-server
 (define interface-version 'stateless)
 (define stuffer
  (stuffer-chain
   serialize-stuffer
   (md5-stuffer (build-path (find-system-path 'home-dir) ".urls"))))
 (define (start req)
   `(html (body (h2 "Look ma, no state!"))))
]

The @schememodname[web-server] language automatically provides the @schememodname[web-server/lang/lang-api] API.

@; ------------------------------------------------------------
@section[#:tag "considerations"]{Usage Considerations}

@(require (for-label web-server/lang/web))
                     
@defmodulelang[web-server]

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
@href-link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/" "Continuations from Generalized Stack Inspection"].
We thank Greg Pettyjohn for his initial implementation of this algorithm.
