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

@title[#:style (extras)]{The Racket Reference}

@author["Matthew Flatt" "PLT"]

This manual defines the core Racket language and describes its
most prominent libraries. The companion manual @|Guide| provides a
friendlier (though less precise and less complete) overview of the
language.

@margin-note{The source of this manual is available on
@hyperlink["https://github.com/racket/racket/tree/master/pkgs/racket-doc/scribblings/reference"]{GitHub}.}

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
@racketmodname[racket/base]@racket-extra-libs[].
In addition, it re-exports @racket[for-syntax] everything from
@racketmodname[racket/base].}

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["notation.scrbl"]
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

  (bib-entry #:key "Baker93"
             #:author "Henry G. Baker"
             #:title "Equal Rights for Functional Objects or, the More Things Change, the More They are the Same"
             #:date "1993"
             #:location "SIGPLAN OOPS Messenger"
             #:url "https://doi.org/10.1145/165593.165596")

  (bib-entry #:key "C99"
             #:author "ISO/IEC"
             #:title "ISO/IEC 9899:1999 Cor. 3:2007(E)"
             #:date "2007")

  (bib-entry #:key "Culpepper07"
             #:author "Ryan Culpepper, Sam Tobin-Hochstadt, and Matthew Flatt"
             #:title "Advanced Macrology and the Implementation of Typed Scheme"
             #:location "Workshop on Scheme and Functional Programming"
             #:url "https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf"
             #:date "2007")

  (bib-entry #:key "Danvy90"
             #:author "Olivier Danvy and Andre Filinski"
             #:title "Abstracting Control"
             #:location "LISP and Functional Programming"
             #:url "https://doi.org/10.1145/91556.91622"
             #:date "1990")

  (bib-entry #:key "Felleisen88a"
             #:author "Matthias Felleisen"
             #:title "The theory and practice of first-class prompts"
             #:location "Principles of Programming Languages"
             #:url "https://www.cs.tufts.edu/~nr/cs257/archive/matthias-felleisen/prompts.pdf"
             #:date "1988")

  (bib-entry #:key "Felleisen88"
             #:author "Matthias Felleisen, Mitch Wand, Dan Friedman, and Bruce Duba"
             #:title "Abstract Continuations: A Mathematical Semantics for Handling Full Functional Jumps"
             #:location "LISP and Functional Programming"
             #:url "https://help.luddy.indiana.edu/techreports/TRNNN.cgi?trnum=TR248"
             #:date "1988")

  (bib-entry #:key "Feltey18"
             #:author "Daniel Feltey, Ben Greenman, Christophe Scholliers, Robert Bruce Findler, and Vincent St-Amour"
             #:title "Collapsible Contracts: Fixing a Pathology of Gradual Typing"
             #:location "Object-Oriented Programming, Systems, and Languages (OOPSLA)"
             #:url "https://www.ccis.northeastern.edu/~types/publications/collapsible/fgsfs-oopsla-2018.pdf"
             #:date "2018")

  (bib-entry #:key "Flatt02"
             #:author "Matthew Flatt"
             #:title "Composable and Compilable Macros: You Want it When?"
             #:location "International Conference on Functional Programming (ICFP)"
             #:url "https://www.cs.utah.edu/plt/publications/macromod.pdf"
             #:date "2002")

  (bib-entry #:key "Flatt07"
             #:author "Matthew Flatt, Gang Yu, Robert Bruce Findler, and Matthias Felleisen"
             #:title "Adding Delimited and Composable Control to a Production Programming Environment"
             #:location "International Conference on Functional Programming (ICFP)"
             #:url "http://www.cs.utah.edu/plt/publications/icfp07-fyff.pdf"
             #:date "2007")

  (bib-entry #:key "Flatt13"
             #:author "Matthew Flatt"
             #:title "Submodules in Racket: You Want It When, Again?"
             #:location "International Conference on Generative Programming: Concepts & Experiences (GPCE'13)"
             #:url "https://www.cs.utah.edu/plt/publications/gpce13-f-color.pdf"
             #:date "2013")

  (bib-entry #:key "Friedman95"
             #:title "Exception system proposal"
             #:author "Daniel P. Friedman, C. T. Haynes, and R. Kent Dybvig"
             #:location "web page"
             #:url "https://web.archive.org/web/20161012054505/http://www.cs.indiana.edu/scheme-repository/doc.proposals.exceptions.html"
             #:date "1995")

  (bib-entry #:key "Gasbichler02"
             #:title "Processes vs. User-Level Threads in Scsh"
             #:author "Martin Gasbichler and Michael Sperber"
             #:date "2002"
             #:url "http://www.ccs.neu.edu/home/shivers/papers/scheme02/article/threads.pdf"
             #:location "Workshop on Scheme and Functional Programming")

  (bib-entry #:key "Greenberg15"
             #:author "Michael Greenberg"
             #:title "Space-Efficient Manifest Contracts"
             #:location "Principles of Programming Languages (POPL)"
             #:url "https://cs.pomona.edu/~michael/papers/popl2015_space.pdf"
             #:date "2015")

 (bib-entry #:key "Gunter95"
            #:author "Carl Gunter, Didier Remy, and Jon Rieke"
            #:title "A Generalization of Exceptions and Control in ML-like Languages"
            #:location "Functional Programming Languages and Computer Architecture"
            #:url "http://gallium.inria.fr/~remy/ftp/prompt.pdf"
            #:date "1995")

 (bib-entry #:key "Haynes84"
            #:author "Christopher T. Haynes and Daniel P. Friedman"
            #:title "Engines Build Process Abstractions"
            #:location "Symposium on LISP and Functional Programming"
            #:url "https://legacy.cs.indiana.edu/ftp/techreports/TR159.pdf"
            #:date "1984")

 (bib-entry #:key "Hayes97"
            #:author "Barry Hayes"
            #:title "Ephemerons: a New Finalization Mechanism"
            #:location "Object-Oriented Languages, Programming, Systems, and Applications"
            #:url "https://static.aminer.org/pdf/PDF/000/522/273/ephemerons_a_new_finalization_mechanism.pdf"
            #:date "1997")

 (bib-entry #:key "Hieb90"
            #:author "Robert Hieb and R. Kent Dybvig"
            #:title "Continuations and Concurrency"
            #:location "Principles and Practice of Parallel Programming"
            #:url "https://legacy.cs.indiana.edu/ftp/techreports/TR256.pdf"
            #:date "1990")

  (bib-entry #:key "Lamport79"
             #:title "How to Make a Multiprocessor Computer That Correctly Executes Multiprocess Programs"
             #:author "Leslie Lamport"
             #:location "IEEE Transactions on Computers"
             #:url "https://www.microsoft.com/en-us/research/uploads/prod/2016/12/How-to-Make-a-Multiprocessor-Computer-That-Correctly-Executes-Multiprocess-Programs.pdf"
             #:date "179")

  (bib-entry #:key "L'Ecuyer02"
            #:author "Pierre L'Ecuyer, Richard Simard, E. Jack Chen, and W. David Kelton"
            #:title "An Object-Oriented Random-Number Package With Many Long Streams and Substreams"
            #:location "Operations Research, 50(6)"
            #:url "https://www.iro.umontreal.ca/~lecuyer/myftp/papers/streams00.pdf"
            #:date "2002")

  (bib-entry #:key "Queinnec91"
             #:author "Queinnec and Serpette"
             #:title "A Dynamic Extent Control Operator for Partial Continuations"
             #:location "Principles of Programming Languages"
             #:url "https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.40.9946&rep=rep1&type=pdf"
             #:date "1991")

  (bib-entry #:key "Reppy99"
             #:author "John H. Reppy"
             #:title "Concurrent Programming in ML"
             #:is-book? #t
             #:location "Cambridge University Press"
             #:url "https://doi.org/10.1017/CBO9780511574962"
             #:date "1999")

  (bib-entry #:key "Roux14"
             #:author "Pierre Roux"
             #:title "Innocuous Double Rounding of Basic Arithmetic Operations"
             #:location @elem{@italic{Journal of Formalized Reasoning}, 7(1)}
             #:date "2014")

  (bib-entry #:key "Sapin18"
             #:author "Simon Sapin"
             #:title "The WTF-8 Encoding"
             #:url "http://simonsapin.github.io/wtf-8/"
             #:date "2018")

  (bib-entry #:key "Shan04"
             #:author "Ken Shan"
             #:title "Shift to Control"
             #:location "Workshop on Scheme and Functional Programming"
             #:url "http://homes.sice.indiana.edu/ccshan/recur/recur.pdf"
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
             #:url "https://www2.ccs.neu.edu/racket/pubs/lasc1990-sf.pdf"
             #:date "1990")

  (bib-entry #:key "Sitaram93"
             #:title "Handling Control"
             #:author "Dorai Sitaram"
             #:location "Programming Language Design and Implementation"
             #:url "http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.7256"
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
             #:location "Object-Oriented Programming, Systems, and Languages (OOPSLA)"
             #:url "http://www.eecs.northwestern.edu/~robby/pubs/papers/oopsla2012-stff.pdf"
             #:date "2012")

  (bib-entry #:key "Stucki15"
             #:title "RRB Vector: A Practical General Purpose Immutable Sequence"
             #:author "Nicolas Stucki, Tiark Rompf, Vlad Ureche, and Phil Bagwell"
             #:location "International Conference on Functional Programming"
             #:url "https://dl.acm.org/doi/abs/10.1145/2784731.2784739"
             #:date "2015")

  (bib-entry #:key "Torosyan21"
             #:title "Runtime and Compiler Support for HAMTs"
             #:author "Son Torosyan, Jon Zeppieri, and Matthew Flatt"
             #:location "Dynamic Languages Symposium (DLS)"
             #:url "https://www.cs.utah.edu/plt/publications/dls21-tzf.pdf"
             #:date "2021")
  )

@;------------------------------------------------------------------------

@index-section[]
