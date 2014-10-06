#lang racket/base
(require racket/path
         racket/system
         racket/format
         racket/file
         racket/pretty
         "cmdline.rkt")

(define libs
  '("libffi.6"
    "libgio-2.0.0"
    "libgmodule-2.0.0"
    "libgthread-2.0.0"
    "libglib-2.0.0"
    "libgobject-2.0.0"
    "libintl.8"
    "libharfbuzz.0"
    "libpango-1.0.0"
    "libpangocairo-1.0.0"
    "libpangoft2-1.0.0"
    "libexpat.1"
    "libfontconfig.1"
    "libfreetype.6"
    "libcairo.2"
    "libpixman-1.0"
    "libpng16.16"
    "libgmp.10"
    "libmpfr.4"
    "libjpeg.9"
    "libpoppler.44"
    "libpoppler-glib.8"))

(define win-libs
  '("libiconv-2"
    "libeay32"
    "ssleay32"
    "sqlite3"
    "longdouble"
    "zlib1"
    "libpangowin32-1.0.0"))

(define linux-libs
  (append
   '("libXau.6"
     "libxcb-shm.0"
     "libxcb-render.0"
     "libxcb.1"
     "libX11.6"
     "libXext.6"
     "libXrender.1"
     "fonts")
   '("libcrypto.1.0.0"
     "libssl.1.0.0"
     "libz.1"
     "libsqlite3.0")
   '("libgtk-x11-2.0.0"
     "libgdk-x11-2.0.0"
     "libatk-1.0.0"
     "libgdk_pixbuf-2.0.0")))
(define linux-remove-libs
  '("libintl.8"))

(define package-mapping
  `(["draw"        ; pkg name
     "-2"          ; pkg suffix (increment after "-" when library versions change)
     "racket/draw" ; subdir
     "" ; extra for "LICENSE.txt"
     #t ; dynamic libraries (as opposed to shared files)
     #f ; for-pkg name (e.g., "base"), of #f if the same as the pkg name
     (["libffi" "libffi - Copyright (c) 1996-2014  Anthony Green, Red Hat, Inc and others."]
      ["libglib" "GLib is released under the GNU Library General Public License (GNU LGPL)."]
      "libgio"
      "libgmodule"
      "libgobject"
      "libgthread"
      ["libintl" "libintl is released under the GNU Library General Public License (GNU LGPL)."]
      ["libharfbuzz" "HarfBuzz is relased under a MIT license."]
      ["libpango" "Pango is released under the GNU Library General Public License (GNU LGPL)."]
      "libpangocairo"
      "libpangoft2"
      "libpangowin32"
      "libexpat"
      ["libfontconfig" ,(~a "FontConfig:\n"
                            " Copyright © 2000,2001,2002,2003,2004,2006,2007 Keith Packard\n"
                            " Copyright © 2005 Patrick Lam\n"
                            " Copyright © 2009 Roozbeh Pournader\n"
                            " Copyright © 2008,2009 Red Hat, Inc.\n"
                            " Copyright © 2008 Danilo Šegan\n"
                            " Copyright © 2012 Google, Inc.")]
      ["libfreetype" "Pixman is relased under the FreeType project license."]
      ["libcairo" "Cairo is released under the GNU Library General Public License (GNU LGPL)."]
      ["libpixman" "Pixman is relased under a MIT license."]
      ["libpng" "Libpng is released under the libpng license."]
      ["libjpeg" "This software is based in part on the work of the Independent JPEG Group."]
      ["zlib1" "zlib is by Jean-loup Gailly and Mark Adler."]
      ["libz" "zlib is by Jean-loup Gailly and Mark Adler."])]
    ["racket"
     "-2"
     "racket"
     ""
     #t
     #f
     (["libeay32" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (http://www.openssl.org/).\n"
                       "\n"
                       "Eric Young is the author of libeay and ssleay.")]
      "ssleay32"
      ["libssl" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (http://www.openssl.org/).\n")]
      "libcrypto"
      ["libiconv-2" "libiconv is released under the GNU Lesser General Public License (GNU LGPL)."]
      ["longdouble" ,(~a "The source to longdouble is included with the Racket source code,\n"
                         "which is available from\n"
                         "  http://www.racket-lang.org/")])]
    ["math"
     ""
     "math"
     ""
     #t
     #f
     (["libgmp" "GNU MP is released under the GNU Lesser General Public License (GNU LGPL)."]
      ["libmpfr" "MPFR is released under the GNU Lesser General Public License (GNU LGPL)."])]

    ["draw-x11"
     ""
     "racket/draw/x11"
     ""
     #t
     "draw"
     (["libX11.6" "libX11 is released under the X.Org Foundation license."]
      ["libXau.6" "libXau - Copyright 1988, 1993, 1994, 1998  The Open Group"]
      ["libxcb-shm.0" "libxcb - Copyright (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett."]
      "libxcb-render.0"
      "libxcb.1"
      ["libXext.6" "libXext - Copyright 1986, 1987, 1988, 1989, 1994, 1998  The Open Group"]
      ["libXrender.1" "libXrender - Copyright © 2001,2003 Keith Packard"])]
    ["draw-ttf"
     ""
     "racket/draw/ttf"
     ""
     #f
     "draw"
     (["fonts" ,(~a "Fonts:\n"
                    " Copyright © 2000,2001,2002,2003,2004,2006,2007 Keith Packard\n"
                    " Copyright © 2005 Patrick Lam\n"
                    " Copyright © 2009 Roozbeh Pournader\n"
                    " Copyright © 2008,2009 Red Hat, Inc.\n"
                    " Copyright © 2008 Danilo Šegan\n"
                    " Copyright © 2012 Google, Inc.")])]

    ["gui"
     ""
     "racket/gui"
     ""
     #t
     #f
     (["libgtk-x11-2.0.0" "GTK+ is released under the GNU Library General Public License (GNU LGPL)."]
      ["libatk-1.0.0" "ATK is released under the GNU Library General Public License (GNU LGPL)."]
      "libgdk-x11-2.0.0"
      "libgdk_pixbuf-2.0.0")]

    ["db"
     ""
     "db"
     ""
     #t
     "base"
     (["libsqlite3.0" "SQLite3 is in the public domain."]
      ["sqlite3" "SQLite3 is in the public domain."])]
    
    ["poppler"
     ""
     "racket-poppler"
     ""
     #t
     "racket-poppler"
     (["libpoppler"
       ;; Note: Poppler is GPL and *not* in the main Racket distribution (which is LGPL)
       "Poppler is released under the GNU General Public License (GNU GPL)."])]))

(define (libs-of-pkg p) (list-ref p 6))

(define (plain-path? p)
  (equal? p "fonts"))

(define dest-dir
  (build-command-line
   #:args (dest-dir)
   dest-dir))

(define from (build-path (current-directory) "dest" (if win? "bin" "lib")))

(define (find-pkg lib)
  (define pkg (for/or ([p (in-list package-mapping)])
                (define nl
                  (for/or ([nl (in-list (libs-of-pkg p))])
                    (define n (if (pair? nl) (car nl) nl))
                    (cond
                     [(equal? n lib) nl]
                     [else
                      (define len (string-length n))
                      (and ((string-length lib) . > . (add1 len))
                           (string=? n (substring lib 0 len))
                           (regexp-match? #rx"[-.0-9]" (string (string-ref lib len)))
                           nl)])))
                (and nl
                     (list (car p) (cadr p) (caddr p)
                           (and (pair? nl) (cadr nl))))))
  (unless pkg
    (error 'install "cannot find package for library: ~e" lib))
  (apply values pkg))

(define (gen-info platform i-platform for-pkg pkg-name subdir libs lics lic-end lib?)
  (define dest (build-path dest-dir pkg-name))
  (define lib-path (build-path dest subdir "info.rkt"))
  (define top-path (build-path dest "info.rkt"))
  (define same? (equal? lib-path top-path))
  (define (write-libs o)
    (newline o)
    (pretty-write `(define install-platform ,i-platform) o)
    (newline o)
    (pretty-write `(define ,(if lib?
                                'copy-foreign-libs
                                'copy-shared-files)
                    (quote ,libs))
                  o)
    (define dirs (filter (lambda (lib)
                           (directory-exists? (build-path dest subdir lib)))
                         libs))
    (unless (null? dirs)
      (newline o)
      (pretty-write `(define compile-omit-paths (quote ,dirs)) o)))
  (define (write-pkg o)
    (newline o)
    (pretty-write `(define collection 'multi) o)
    (pretty-write `(define deps '("base")) o)
    (newline o)
    (pretty-write `(define pkg-desc ,(format "native libraries for \"~a\" package" for-pkg)) o)
    (newline o)
    (pretty-write `(define pkg-authors '(mflatt)) o))
  (unless same?
    (printf "Write ~a\n" lib-path)
    (call-with-output-file*
     lib-path
     #:exists 'truncate
     (lambda (o)
       (displayln "#lang setup/infotab" o)
       (write-libs o))))
  (printf "Write ~a\n" top-path)
  (call-with-output-file*
   top-path
   #:exists 'truncate
   (lambda (o)
     (displayln "#lang setup/infotab" o)
     (write-pkg o)
     (when same?
       (write-libs o))))
  (define lic-path (build-path dest "LICENSE.txt"))
  (printf "Write ~a\n" lic-path)
  (call-with-output-file*
   lic-path
   #:exists 'truncate
   (lambda (o)
     (displayln pkg-name o)
     (displayln "Copyright (c) 2010-2014 PLT Design Inc." o)
     (newline o)
     (displayln "This package is distributed under the GNU Lesser General Public" o)
     (displayln "License (LGPL).  This means that you can link this package into" o)
     (displayln "proprietary applications, provided you follow the rules stated in the" o)
     (displayln "LGPL.  You can also modify this package; if you distribute a modified" o)
     (displayln "version, you must distribute it under the terms of the LGPL, which in" o)
     (displayln "particular means that you must release the source code for the" o)
     (displayln "modified software.  See http://www.gnu.org/copyleft/lesser.html" o)
     (displayln "for more information." o)
     (for ([l (in-list lics)])
       (newline o)
       (displayln l o))
     (display lic-end o))))

(define (install platform i-platform so fixup libs)
  (define pkgs (make-hash))
  (define pkgs-lic (make-hash))

  (define (install lib)
    (define p (cond
               [(plain-path? lib) lib]
               [(procedure? so) (so lib)]
               [else (format "~a.~a" lib so)]))
    (define-values (pkg suffix subdir lic) (find-pkg lib))
    (define dir (build-path dest-dir
                            (~a pkg "-" platform suffix)
                            subdir))
    (define dest (build-path dir p))
    (make-directory* dir)
    (cond
     [(file-exists? dest) (delete-file dest)]
     [(directory-exists? dest) (delete-directory/files dest)])
    (define src (build-path from p))
    (if (directory-exists? src)
        (copy-directory/files src dest)
        (copy-file src dest))
    (unless (plain-path? p)
      (fixup p dest))

    (hash-update! pkgs pkg (lambda (l) (cons p l)) '())
    (when lic
      (hash-update! pkgs-lic pkg (lambda (l) (cons lic l)) '())))
  
  (for-each install libs)

  (for ([(pkg libs) (in-hash pkgs)])
    (define a (assoc pkg package-mapping))
    (gen-info platform 
              i-platform
              (or (list-ref a 5) pkg)
              (~a pkg "-" platform (list-ref a 1))
              (list-ref a 2)
              libs
              (reverse (hash-ref pkgs-lic pkg null))
              (list-ref a 3)
              (list-ref a 4))))

(define (install-mac)
  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (system (format "install_name_tool -id ~a ~a" (file-name-from-path p-new) p-new))
    (for-each (lambda (s)
                (system (format "install_name_tool -change ~a @loader_path/~a ~a" 
                                (format "~a/~a.dylib" from s)
                                (format "~a.dylib" s)
                                p-new)))
              libs)
    (system (format "strip -S ~a" p-new)))

  (define platform (~a (if m32? 
                           (if ppc? "ppc" "i386")
                           "x86_64")
                       "-macosx"))
  
  (install platform platform "dylib" fixup libs))

(define (install-win)
  (define exe-prefix (if m32?
                         "i686-w64-mingw32"
                         "x86_64-w64-mingw32"))

  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (system (~a exe-prefix "-strip -S " p-new)))

  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PATH" (~a (if m32?
                           "/usr/mw32/bin:"
                           "/usr/mw64/bin:")
                       (getenv "PATH")))

    (install (~a "win32-" (if m32? "i386" "x86_64"))
             (~a "win32\\" (if m32? "i386" "x86_64"))
             "dll"
             fixup
             (for/list ([s (in-list (append libs
                                            win-libs))])
               (regexp-replace #rx"!"
                               (regexp-replace* #rx"[.]"
                                                (regexp-replace #rx"[.](?=.*[.])" s "!")
                                                "-")
                               ".")))))

(define (install-linux)
  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (file-or-directory-permissions p-new #o755)
    (unless (system (format "strip -S ~a" p-new))
      (error "strip failed"))
    ;; Might fail if there are no external references:
    (system (format "chrpath -r '$ORIGIN' ~a" p-new)))

  (define platform (~a (if m32?
                           "i386"
                           "x86_64")
                       "-linux-natipkg"))

  (define (add-so orig-p)
    (let loop ([p orig-p] [suffix ""])
      (define p-so (string-append p ".so" suffix))
      (cond
       [(file-exists? (build-path from p-so))
        p-so]
       [else
        (define m (regexp-match #rx"^(.*)[.](.*)$" p))
        (cond
         [m
          (loop (cadr m) (string-append "." (caddr m) suffix))]
         [else
          (error 'add-so "not found: ~s" orig-p)])])))

  (install platform platform add-so fixup (append (remove* linux-remove-libs
							   libs)
						  linux-libs)))

(cond
 [win? (install-win)]
 [linux? (install-linux)]
 [else (install-mac)])
