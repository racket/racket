#lang at-exp racket/base
(require racket/format
         net/url
         (only-in "config.rkt" current-stamp))

(provide make-readme
         make-source-notes
         make-macosx-notes)

(define (maybe-stamp config)
  (if (hash-ref config '#:release? #f)
      ""
      @~a{ (@(current-stamp))}))

(define (make-readme config)
  @~a{
      The Racket Programming Language
      ===============================

      This is the
        @|(hash-ref config '#:name "Racket")|
      distribution for version @(version)@(maybe-stamp config).@;

      @(if (hash-ref config '#:source? #f)
           (string-append "\n" (make-source-notes config) "\n")
           "")@;
      @(if (and (not (hash-ref config '#:source? #f))
                (eq? (hash-ref config '#:platform (system-type)) 'macosx))
           (string-append "\n" (make-macosx-notes config) "\n")
           "")@;
      @(let* ([catalogs (filter
                         (lambda (s) (not (equal? s "")))
                         (or (hash-ref config '#:dist-catalogs #f)
                             (let ([v (hash-ref config '#:dist-base-url #f)])
                               (and v
                                    (list (url->string
                                           (combine-url/relative (string->url v) "catalog/")))))
                             null))]
              [s (if (= 1 (length catalogs)) "" "s")]
              [is (if (= 1 (length catalogs)) "is" "are")])
         (if (null? catalogs)
             ""
             @~a{@"\n"The distribution has been configured so that when you install or
                 update packages, the package catalog@|s| at@;
                 @(apply ~a (for/list ([catalog (in-list catalogs)])
                              @~a{@"\n"  @|catalog|}))
                 @|is| consulted, first.@"\n"}))@;
     
      Visit
         http://racket-lang.org/ 
      for more Racket resources.
     
     
      License
      -------
     
      Racket
      Copyright (c) 2010-2013 PLT Design Inc.
     
      Racket is distributed under the GNU Lesser General Public License
      (LGPL).  This means that you can link Racket into proprietary
      applications, provided you follow the rules stated in the LGPL.  You can
      also modify Racket; if you distribute a modified version, you must
      distribute it under the terms of the LGPL, which in particular means
      that you must release the source code for the modified software.  See
      share/COPYING_LESSER.txt for more information.})

(define (make-source-notes config)

  @~a{This distribution provides source for the Racket run-time system;
      for build and installation instructions, see "racket/src/README".
      Besides the run-time system's source, the distribution provides
      pre-built versions of the core Racket bytecode, as well as pre-built
      versions of included packages and documentation --- which makes it
      suitable for quick installation on a Unix platform for which
      executable binaries are not already provided.})

(define (make-macosx-notes config)
  @~a{Install by dragging the enclosing
        @|(hash-ref config '#:dist-name "Racket")| v@(version)
      folder to your Applications folder --- or wherever you like. You can
      move the folder at any time, but do not move applications or other
      files within the folder. If you want to use the Racket command-line
      programs, then (optionally) add the path of the "bin" subdirectory to
      your PATH environment variable.})
