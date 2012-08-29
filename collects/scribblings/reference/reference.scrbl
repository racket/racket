#lang scribble/manual
@(require "mz.rkt"
          scribble/core scribble/html-properties scribble/latex-properties
          racket/list)

@(define (racket-extra-libs)
   (make-delayed-element
    (lambda (renderer p ri)
      (let ([mods (append-map
                   (lambda (k) (list ", " (resolve-get p ri k)))
                   (sort (resolve-get-keys
                          p ri (lambda (v) (eq? (car v) 'racket-extra-lib)))
                         string<?
                         #:key (lambda (k) (symbol->string (cadr k)))
                         #:cache-keys? #t))])
        `(,@(drop-right mods 2) ", and " ,(last mods))))
    (lambda () "...")
    (lambda () "...")))

@(define (extras)
   (make-style #f (list
                   (make-css-addition "extras.css")
                   (make-tex-addition "extras.tex"))))

@title[#:style (extras)]{@bold{The Racket Reference}}

@author["Matthew Flatt" "PLT"]

This manual defines the core Racket language and describes its
most prominent libraries. The companion manual @|Guide| provides a
friendlier (though less precise and less complete) overview of the
language.

@defmodulelang*[(racket/base racket)
                ;; Use sources for overlap with `scheme' and `mzscheme':
                #:use-sources ('#%kernel
                               racket/private/more-scheme
                               racket/private/misc
                               racket/private/qqstx
                               racket/private/stxcase-scheme
                               racket/private/letstx-scheme
                               racket/private/define
                               racket/private/stx
                               racket/private/map
                               racket/private/list
                               racket/private/base)]{

Unless otherwise noted, the bindings defined in this manual are
exported by the @racketmodname[racket/base] and @racketmodname[racket]
languages.}

@margin-note{The @racketmodname[racket/base] library is much smaller than
the @racketmodname[racket] library and will typically load faster.

The @racketmodname[racket] library combines
@racketmodname[racket/base]@racket-extra-libs[].}

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["syntax.scrbl"]
@include-section["data.scrbl"]
@include-section["struct.scrbl"]
@include-section["class.scrbl"]
@include-section["units.scrbl"]
@include-section["contracts.scrbl"]
@include-section["match.scrbl"]
@include-section["control.scrbl"]
@include-section["concurrency.scrbl"]
@include-section["macros.scrbl"]
@include-section["io.scrbl"]
@include-section["security.scrbl"]
@include-section["os.scrbl"]
@include-section["memory.scrbl"]
@include-section["unsafe.scrbl"]
@include-section["running.scrbl"]

@;------------------------------------------------------------------------

@(bibliography

  (bib-entry #:key "C99"
             #:author "ISO/IEC"
             #:title "ISO/IEC 9899:1999 Cor. 3:2007(E))"
             #:date "2007")

  (bib-entry #:key "Danvy90"
             #:author "Olivier Danvy and Andre Filinski"
             #:title "Abstracting Control"
             #:location "LISP and Functional Programming"
             #:date "1990")

  (bib-entry #:key "Felleisen88a"
             #:author "Matthias Felleisen"
             #:title "The theory and practice of first-class prompts"
             #:location "Principles of Programming Languages"
             #:date "1988")

  (bib-entry #:key "Felleisen88"
             #:author "Matthias Felleisen, Mitch Wand, Dan Friedman, and Bruce Duba"
             #:title "Abstract Continuations: A Mathematical Semantics for Handling Full Functional Jumps"
             #:location "LISP and Functional Programming"
             #:date "1988")

  (bib-entry #:key "Friedman95" 
             #:title "Exception system proposal" 
             #:author "Daniel P. Friedman, C. T. Haynes, and R. Kent Dybvig" 
             #:location "web page"
             #:url "http://www.cs.indiana.edu/scheme-repository/doc.proposals.exceptions.html"
             #:date "1995")

  (bib-entry #:key "Gasbichler02" 
             #:title "Processes vs. User-Level Threads in Scsh" 
             #:author "Martin Gasbichler and Michael Sperber" 
             #:date "2002"
             #:location "Workshop on Scheme and Functional Programming")

 (bib-entry #:key "Gunter95"
            #:author "Carl Gunter, Didier Remy, and Jon Rieke"
            #:title "A Generalization of Exceptions and Control in ML-like Languages"
            #:location "Functional Programming Languages and Computer Architecture"
            #:date "1995")

 (bib-entry #:key "Haynes84"
            #:author "Christopher T. Haynes and Daniel P. Friedman"
            #:title "Engines Build Process Abstractions"
            #:location "Symposium on LISP and Functional Programming"
            #:date "1984")
 
 (bib-entry #:key "Hayes97"
            #:author "Barry Hayes"
            #:title "Ephemerons: a New Finalization Mechanism"
            #:location "Object-Oriented Languages, Programming, Systems, and Applications"
            #:date "1997")
 
 (bib-entry #:key "Hieb90"
            #:author "Robert Hieb and R. Kent Dybvig"
            #:title "Continuations and Concurrency"
            #:location "Principles and Practice of Parallel Programming"
            #:date "1990")

 (bib-entry #:key "L'Ecuyer02"
            #:author "Pierre L'Ecuyer, Richard Simard, E. Jack Chen, and W. David Kelton"
            #:title "An Object-Oriented Random-Number Package With Many Long Streams and Substreams"
            #:location "Operations Research, 50(6)"
            #:date "2002")

  (bib-entry #:key "Queinnec91"
             #:author "Queinnec and Serpette"
             #:title "A Dynamic Extent Control Operator for Partial Continuations"
             #:location "Principles of Programming Languages"
             #:date "1991")

  (bib-entry #:key "Shan04"
             #:author "Ken Shan"
             #:title "Shift to Control"
             #:location "Workshop on Scheme and Functional Programming"
             #:date "2004")

 (bib-entry #:key "Sperber07"
            #:author "Michael Sperber, R. Kent Dybvig, Matthew Flatt, and Anton van Straaten (editors)"
            #:title @elem{The Revised@superscript{6} Report on the Algorithmic Language Scheme}
            #:date "2007"
            #:url "http://www.r6rs.org/")

  (bib-entry #:key "Sitaram90"
             #:author "Dorai Sitaram and Matthias Felleisen"
             #:title "Control Delimiters and Their Hierarchies"
             #:location @italic{Lisp and Symbolic Computation}
             #:date "1990")

  (bib-entry #:key "Sitaram93" 
             #:title "Handling Control" 
             #:author "Dorai Sitaram"
             #:location "Programming Language Design and Implementation" 
             #:date "1993")

  (bib-entry #:key "SRFI-42"
             #:title "SRFI-42: Eager Comprehensions"
             #:author "Sebastian Egner"
             #:location "SRFI"
             #:url "http://srfi.schemers.org/srfi-42/"
             #:date "2003")

  (bib-entry #:key "Strickland12"
             #:title "Chaperones and Impersonators: Run-time Support for Reasonable Interposition"
             #:author "T. Stephen Strickland, Sam Tobin-Hochstadt, Matthew Flatt, and Robert Bruce Findler"
             #:location "Object-Oriented Programming, Systems, and Languages (OOPSLA"
             #:url "http://www.eecs.northwestern.edu/~robby/pubs/papers/oopsla2012-stff.pdf"
             #:date "2012")
  )

@;------------------------------------------------------------------------

@index-section[]
