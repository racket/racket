#lang scribble/doc
@(require "web-server.ss"
          (for-label scheme/serialize
                     web-server/lang/abort-resume
                     web-server/lang/web))

@title[#:tag "considerations"]{Usage Considerations}
                     
A stateless servlet has the following process performed on it automatically:
@itemize[
 @item{All uses of @scheme[letrec] are removed and replaced with equivalent uses of
       @scheme[let] and imperative features.}
 @item{The program is converted into @link["http://en.wikipedia.org/wiki/Administrative_normal_form"]{ANF} (Administrative Normal Form),
       making all continuations explicit.}
 @item{All continuations and continuations marks are recorded in the
       continuation marks of the expression
       they are the continuation of.}
 @item{All calls to external modules are identified and marked.}
 @item{All uses of @scheme[call/cc] are removed and replaced with
       equivalent gathering of the continuations through the continuation marks installed earlier.}
 @item{The program is defunctionalized with a serializable data-structure for each
       @scheme[lambda].}
]

This process allows the continuations captured by your servlet to be serialized.
This means they may be stored on the client's browser or the server's disk.
Thus, your servlet has no cost to the server other than execution. This is
very attractive if you've used Scheme servlets and had memory problems.

This process is defined on all of PLT Scheme and occurs after macro-expansion,
so you are free to use all interesting features of PLT Scheme. However, there
are some considerations you must make.

First, this process drastically changes the structure of your program. It
will create an immense number of lambdas and structures your program
did not normally contain. The performance implication of this has not been
studied with PLT Scheme.

Second, the defunctionalization process is sensitive to the syntactic structure
of your program. Therefore, if you change your program in a trivial way, for example,
changing a constant, then all serialized continuations will be obsolete and will
error when deserialization is attempted. This is a feature, not a bug! It is a small
price to pay for protection from the sorts of errors that would occur if your program
were changed in a meaningful way.

Third, the values in the lexical scope of your continuations must be serializable
for the continuations itself to be serializable. This means that you must use
@scheme[define-serializable-struct] rather than @scheme[define-struct], and take
care to use modules that do the same. Similarly, you may not use @scheme[parameterize],
because parameterizations are not serializable.

Fourth, and related, this process only runs on your code, not on the code you
@scheme[require]. Thus, your continuations---to be serializable---must not
be in the context of another module. For example, the following will fail with an @as-index{"unsafe context"}
exception:

@schemeblock[
 (define requests
   (map (lambda (rg) (send/suspend/url rg))
        response-generators))
]
because @scheme[map] is not transformed by the process. However, if you defined
your own @scheme[map] function, there would be no problem. Another solution is to
store the @scheme[map] part of the continuation on the server with @scheme[serial->native]
and @scheme[native->serial]:
@schemeblock[
 (define requests
   (serial->native
    (map (lambda (rg) (native->serial (send/suspend/url rg)))
        response-generators)))
]

Fifth, the store is @bold{not} serialized. If you rely on the store you will
be taking huge risks. You will be assuming that the serialized continuation
is invoked on the same server before the server is restarted or
the memory is garbage collected.

This process is derived from the ICFP papers
@emph{@link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/"]{Continuations from Generalized Stack Inspection}} by Pettyjohn et al. in 2005 and 
@emph{Automatically RESTful Web Applications, Or Marking Modular Serializable Continuations} by Jay McCarthy in 2009.
We thank Greg Pettyjohn for his initial implementation of this algorithm.
