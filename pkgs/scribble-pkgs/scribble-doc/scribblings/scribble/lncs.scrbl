#lang scribble/manual
@(require (except-in "utils.rkt" author)
          (except-in (for-label scribble/lncs/lang) #%module-begin))

@(define-syntax-rule (def base-author)
   (begin
     (require (for-label scribble/base))
     (define base-author @racket[author])))
@(def base-author)

@title{LNCS Paper Format}

@defmodulelang[scribble/lncs #:use-sources (scribble/lncs/lang)]{
  The @racketmodname[scribble/lncs]
language is like @racketmodname[scribble/base], but configured with
Latex style defaults to use the @filepath{llncs.cls} class
file. The class file is not included with Scribble due to license issues,
but if the file is not manually installed into the
@racket[scribble/lncs] collection, then it is downloaded on demand to
@racket[(find-system-path 'addon-dir)].}

@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @racket[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defform/subs[#:literals (author)
              (authors auth ...)
              ([auth (author pre-content-expr ...)
                     (author #:inst str-expr pre-content-expr ...)])
              #:contracts ([pre-content-expr pre-content?]
                           [str-expr string?])]{

A replacement for @base-author from @racketmodname[scribble/base].

The @racket[#:inst] should be a number that matches up to one of the
arguments to @racket[institutes].
}


@defidform[author]{For use only in @racket[authors].}


@defform[#:literals (institute)
         (institutes (institute pre-content-expr ...) ...)
         #:contracts ([pre-content-expr pre-content?])]{

The @racket[pre-content-expr]s are used as the institutions of the authors.

}

@defidform[institute]{For use only in @racket[institutes].}

@defform[(email pre-content-expr ...)]{
  Specifies an email address; must be used inside @racket[institute].
}

@section{Example}

Here is an example of a paper written in the LNCS format:

@margin-note{For more randomly generated papers, see SCIgen:
             @url["http://pdos.csail.mit.edu/scigen"]}

@codeblock[#:keep-lang-line? #t]|{
#lang scribble/lncs

@authors[@author[#:inst "1"]{Lauritz Darragh}
         @author[#:inst "2"]{Nikolaj Kyran}
         @author[#:inst "2"]{Kirsten Gormlaith}
         @author[#:inst "2"]{Tamaz Adrian}]

@institutes[
  @institute["University of Southeast Boston"
             @linebreak[]
             @email|{darragh@cs.seboston.edu}|]
  @institute["University of Albion"
             @linebreak[]
             @email|{{nkyran,gorm,tamaz}@cs.albion.ac.uk}|]]

@title{Arak: Low-Energy, Interposable Theory}

@abstract{The implications of client-server symmetries have been
far-reaching and pervasive. Given the current status of
constant-time theory, mathematicians daringly desire the synthesis
of rasterization, which embodies the essential principles of
algorithms. In this work, we describe a client-server tool for
investigating flip-flop gates (Arak), verifying that the
producer-consumer problem can be made homogeneous, secure, and
wireless.}
}|

