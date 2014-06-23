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

      @(if (let ([src? (hash-ref config '#:source? #f)])
             (or (hash-ref config '#:source-runtime? src?)
                 (hash-ref config '#:source-pkgs? src?)))
           (string-append "\n" (make-source-notes config) "\n")
           "")@;
      @(if (and (not (hash-ref config '#:source-runtime? 
                               (hash-ref config '#:source? #f)))
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
      @(let* ([name (hash-ref config '#:install-name "")])
         (if (or (equal? name "")
                 (equal? name (version)))
             ""
             @~a{@"\n"The distribution has been configured so that the installation
                 name is
                   @name
                 Multiple installations with this name share `user'-scoped packages,
                 which makes it easier to upgrade from such an installation to this one.
                 To avoid sharing (which is better for keeping multiple installations
                 active) use `raco pkg config -i --set name ...' to choose a different
                 name for this installation.@"\n"}))@;
     
      Visit
         http://racket-lang.org/ 
      for more Racket resources.
     
     
      License
      -------
     
      Racket
      Copyright (c) 2010-2014 PLT Design Inc.
     
      Racket is distributed under the GNU Lesser General Public License
      (LGPL).  This means that you can link Racket into proprietary
      applications, provided you follow the rules stated in the LGPL.  You can
      also modify Racket; if you distribute a modified version, you must
      distribute it under the terms of the LGPL, which in particular means
      that you must release the source code for the modified software.  See
      share/COPYING_LESSER.txt for more information.})

(define (make-source-notes config)
  (define src? (hash-ref config '#:source? #f))
  (define rt-src
    @~a{This distribution provides source for the Racket run-time system;
        for build and installation instructions, see "src/README".})
  (define pkg-src
    @~a{(The distribution also includes the core Racket collections and any
        installed packages in source form.)})
  (define pkg-built
    @~a{Besides the run-time system's source, the distribution provides
        pre-built versions of the core Racket bytecode, as well as pre-built
        versions of included packages and documentation --- which makes it
        suitable for quick installation on a Unix platform for which
        executable binaries are not already provided.})
  (cond
   [(and (hash-ref config '#:source-runtime? src?)
         (not (hash-ref config '#:source-pkgs? src?)))
    (~a rt-src "\n" pkg-built)]
   [(and (hash-ref config '#:source-runtime? src?)
         (hash-ref config '#:source-pkgs? src?))
    (~a rt-src "\n" pkg-src)]
   [else
    @~a{The distribution includes any pre-installed packages in source form.}]))

(define (make-macosx-notes config)
  (define vers-suffix
    (if (hash-ref config '#:versionless? #f)
        ""
        @~a{ v@(version)}))
  (if (hash-ref config '#:mac-pkg? #f)
      @~a{The installation directory is
            /Applications/@(string-append
                            (hash-ref config '#:dist-name "Racket")
                            (if (hash-ref config '#:release? #f)
                                ""
                                vers-suffix))
          The installer also adjusts "/etc/paths.d/racket" to point to that
          directory's "bin" directory, which adjusts the default PATH
          environment variable for all users.}
      @~a{Install by dragging the enclosing
            @|(hash-ref config '#:dist-name "Racket")|@|vers-suffix|
          folder to your Applications folder --- or wherever you like. You can
          move the folder at any time, but do not move applications or other
          files within the folder. If you want to use the Racket command-line
          programs, then (optionally) add the path of the "bin" subdirectory to
          your PATH environment variable.}))
