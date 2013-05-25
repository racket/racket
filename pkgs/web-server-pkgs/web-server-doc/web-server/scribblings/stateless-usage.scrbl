#lang scribble/doc
@(require "web-server.rkt"
          (for-label racket/serialize
                     web-server/lang/stuff-url
                     web-server/lang/abort-resume
                     web-server/lang/web))

@title[#:tag "considerations"]{Usage Considerations}

A stateless servlet has the following process performed on it
automatically:
@itemize[
 @item{All uses of @racket[letrec] are removed and replaced with
       equivalent uses of @racket[let] and imperative features.}
 @item{The program is converted into @link["http://en.wikipedia.org/wiki/Administrative_normal_form"]{ANF} (Administrative Normal Form),
       making all continuations explicit.}
 @item{All continuations and continuations marks are recorded in the
       continuation marks of the expression
       they are the continuation of.}
 @item{All calls to external modules are identified and marked.}
 @item{All uses of @racket[call/cc] are removed and replaced with
       equivalent gathering of the continuations through the continuation marks installed earlier.}
 @item{The program is defunctionalized with a serializable data-structure for each
       @racket[lambda].}
]

This process allows the continuations captured by your servlet to be
serialized.  This means they may be stored on the client's browser or
the server's disk.

This means your servlet has no cost to the server other than
execution. This is very attractive if you've used Racket servlets and
had memory problems.

This means your server can restart in the middle of a long running Web
interaction without the URLs that have been shared with the client
expiring.  This is very attractive if you've used Racket servlets and
had session timeout problems.

This process is defined on all of Racket and occurs after
macro-expansion, so you are free to use all interesting features of
Racket.  However, there are some considerations you must make.

First, this process drastically changes the structure of your program.
It will create an immense number of lambdas and structures your program
did not normally contain. The performance implication of this has not
been studied with Racket.

Second, the defunctionalization process is sensitive to the syntactic
structure of your program. Therefore, if you change your program in a
trivial way, for example, changing a constant, then all serialized
continuations will be obsolete and will error when deserialization is
attempted. This is a feature, not an error! It is a small price to pay
for protection from the sorts of errors that would occur if your
program were changed in a meaningful way. If you use the
@racket[default-stuffer] or @racketmodname[web-server/stuffers/hash],
then whenever you change your servlet's code, you can safely delete
all saved continuations, because they won't be used any longer.

Third, the values in the lexical scope of your continuations must be
serializable for the continuations itself to be serializable. This means
that you must use @racket[define-serializable-struct] rather than
@racket[define-struct], and take care to use modules that do the same.
Similarly, you may not use @racket[parameterize], because
parameterizations are not serializable.

Fourth, and related, this process only runs on your code, not on the
code you @racket[require]. Thus, your continuations---to be
serializable---must not be in the context of another module. For
example, the following will fail with an @as-index{"unsafe context"}
exception:

@racketblock[
 (define requests
   (map (lambda (rg) (send/suspend/url rg))
        response-generators))
]
because @racket[map] is not transformed by the process. However, if you defined
your own @racket[map] function, there would be no problem. Another solution is to
store the @racket[map] part of the continuation on the server with @racket[serial->native]
and @racket[native->serial]:
@racketblock[
 (define requests
   (serial->native
    (map (lambda (rg) (native->serial (send/suspend/url rg)))
        response-generators)))
]

Fifth, the store is @bold{not} serialized. If you rely on the store you will
be taking huge risks. You will be assuming that the serialized continuation
is invoked on the same server before the server is restarted or
the memory is garbage collected.

This process is derived from the papers
@emph{@link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/"]{Continuations from Generalized Stack Inspection}} by Pettyjohn et al. in 2005, 
@emph{@link["http://faculty.cs.byu.edu/~jay/static/icfp065-mccarthy.pdf"]{Automatically RESTful Web Applications, Or Marking Modular Serializable Continuations}} by Jay McCarthy in 2009,
and
@emph{@link["http://faculty.cs.byu.edu/~jay/static/oopsla026-mccarthy.pdf"]{The Two-State Solution: Native and Serializable Continuations Accord}} by Jay McCarthy in 2010,
We thank Greg Pettyjohn for his initial implementation of this algorithm.
