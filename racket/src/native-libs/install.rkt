#lang racket/base
(require racket/path
         racket/system
         racket/format
         racket/file
         racket/pretty
         racket/list
         "cmdline.rkt")

(define sign-as #f)
(define only-meta? #f)

(define dest-dir
  (build-command-line
   #:once-each
   [("--sign-as") id "Sign Mac OS X libraries"
    (set! sign-as id)]
   [("--only-meta") "only generate \"info.rkt\" and \"LICENSE.txt\" files"
    (set! only-meta? #t)]
   #:args (dest-dir)
   dest-dir))

(when (and mac? aarch64? (not sign-as))
  (error "supply `--sign-as` for AArch64 Mac OS"))

;; Hack to make AArch64 Mac OS and Windows libraries look like other Macs:
(define aarch64-renames
  `(("libffi.7" "libffi.6")))

(define libs
  `("libffi.6"
    "libgio-2.0.0"
    "libgmodule-2.0.0"
    "libgthread-2.0.0"
    "libglib-2.0.0"
    "libgobject-2.0.0"
    "libintl.9"
    "libharfbuzz.0"
    "libfribidi.0"
    "libpango-1.0.0"
    "libpangocairo-1.0.0"
    "libpangoft2-1.0.0"
    "libatk-1.0.0"
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
  (append
   '("libiconv-2"
     "libeay32"
     "ssleay32"
     "sqlite3"
     "zlib1"
     "libpangowin32-1.0.0")
   (if aarch64?
       null
       '("longdouble"))))

(define mac-libs
  '("libedit.0"))

(define mac64-libs
  '("MMTabBarView.framework"))

(define macx86-libs
  '("PSMTabBarControl.framework"))

(define nonwin-libs
  '("libcrypto.1.1"
    "libssl.1.1"
    "libuuid.1"))

(define no-copy-libs
  '("PSMTabBarControl.framework"
    "MMTabBarView.framework"))

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
   '("libz.1"
     "libsqlite3.0")
   '("libgtk-x11-2.0.0"
     "libgdk-x11-2.0.0"
     "libgdk_pixbuf-2.0.0")))
(define linux-remove-libs
  '("libintl.9"))

(struct pkg-spec (name
                  suffix ; (increment after "-" when library versions change)
                  subdir
                  license-txt-extra ; string to use in "LICENSE.txt"
                  info-rkt-comments ; list of strings for comments in the package "info.rkt" file
                  lib? ; dynamic libraries (as opposed to shared files)
                  for-pkg ; name (e.g., "base"), of #f if the same as the pkg name
                  version ; string or #f
                  libs) ; libs is a:
  ;; (listof (cons/c base-name (cons/c (or/c #f license-desc) (non-empty-listof license-sexp))))
  #:transparent)

(define-syntax-rule (package-mapping-qq ([x ...] ...))
  ;; extra parentheses to avoid reindenting everything
  (list (pkg-spec `x ...) ...))

(define package-mapping
  (package-mapping-qq
   (["draw"        ; pkg name
     "-3"          ; pkg suffix (increment after "-" when library versions change)
     "racket/draw" ; subdir
     "" ; extra for "LICENSE.txt"
     ("additionally, fontconfig/src/fcmd5.h and"
      "fontconfig/src/ftglue.{c,h} are placed in the public domain"
      "using non-standardized language.")
     #t ; dynamic libraries (as opposed to shared files)
     #f ; for-pkg name (e.g., "base"), of #f if the same as the pkg name
     #f ; version
     (["libffi" ,(~a "libffi - Copyright (c) 1996-2014  Anthony Green, Red Hat, Inc and others.\n"
                     "libffi is released under the MIT license.")
                MIT]
      ["libglib" "GLib is released under the GNU Library General Public License (GNU LGPL)."
                 LGPL-2.1-or-later]
      ["libgio" #f LGPL-2.1-or-later]
      ["libgmodule" #f LGPL-2.1-or-later]
      ["libgobject" #f LGPL-2.1-or-later]
      ["libgthread" #f LGPL-2.1-or-later]
      ["libintl" "libintl is released under the GNU Library General Public License (GNU LGPL)."
                 LGPL-2.0-or-later ; some files
                 LGPL-2.1-or-later]
      ["libharfbuzz" "HarfBuzz is released under a MIT license."
                     MIT-Modern-Variant]
      ["libfribidi" "FriBidi is released under the GNU Library General Public License (GNU LGPL)."
                    LGPL-2.1-or-later]
      ["libpango" "Pango is released under the GNU Library General Public License (GNU LGPL)."
                  LGPL-2.1-or-later]
      ["libpangocairo" #f LGPL-2.1-or-later]
      ["libpangoft2" #f LGPL-2.1-or-later]
      ["libpangowin32" #f LGPL-2.1-or-later]
      ["libexpat" #f MIT]
      ["libuuid" "libuuid is released under a Modified BSD license."
                 BSD-3-clause]
      ["libfontconfig" ,(~a "FontConfig:\n"
                            " Copyright © 2000,2001,2002,2003,2004,2006,2007 Keith Packard\n"
                            " Copyright © 2005 Patrick Lam\n"
                            " Copyright © 2009 Roozbeh Pournader\n"
                            " Copyright © 2008,2009 Red Hat, Inc.\n"
                            " Copyright © 2008 Danilo Šegan\n"
                            " Copyright © 2012 Google, Inc.")
                       MIT
                       MIT-Modern-Variant
                       HPND-sell-variant
                       Unicode-DFS-2016]
      ["libfreetype" "FreeType is released under the FreeType project license."
                     FTL]
      ["libcairo" "Cairo is released under the GNU Library General Public License (GNU LGPL)."
                  (LGPL-2.1-only OR MPL-1.1)]
      ["libpixman" "Pixman is released under a MIT license."
                   MIT
                   FTL ; has this part been replaced?
                   HPND-sell-variant]
      ["libpng" "Libpng is released under the libpng license."
                Libpng]
      ["libjpeg" "This software is based in part on the work of the Independent JPEG Group."
                 IJG]
      ["zlib1" "zlib is by Jean-loup Gailly and Mark Adler."
               Zlib]
      ["libz" "zlib is by Jean-loup Gailly and Mark Adler."
              Zlib])]
    ["racket"
     "-3"
     "racket"
     ""
     ()
     #t
     "base"
     "1.2"
     (["libeay32" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (https://www.openssl.org/).\n"
                       "\n"
                       "Eric Young is the author of libeay and ssleay.")
                  OpenSSL]
      ["ssleay32" #f OpenSSL]
      ["libssl" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (https://www.openssl.org/).\n")
                OpenSSL]
      ["libcrypto" #f OpenSSL]
      ["libiconv-2" "libiconv is released under the GNU Lesser General Public License (GNU LGPL)."
                    LGPL-3.0-or-later]
      ["longdouble" ,(~a "The source to longdouble is included with the Racket source code,\n"
                         "which is available from\n"
                         "  https://www.racket-lang.org/")
                    (Apache-2.0 OR MIT)]
      ["libedit" ,(~a "This package includes libedit software developed for NetBSD under the\n"
                      "NetBSD license.")
                 BSD-3-clause])]

    ["math"
     ""
     "math"
     ""
     ()
     #t
     #f
     #f
     (["libgmp" ,(~a "GNU MP is dual licensed under under the conditions of the GNU Lesser\n"
                     "General Public License (LGPL), version 3, or the GNU General Public\n"
                     "License (GPL), version 2. This is the recipient’s choice, and the\n"
                     "recipient also has the additional option of applying later versions of\n"
                     "these licenses.")
                (LGPL-3.0-or-later OR GPL-2.0-or-later)]
      ["libmpfr" ,(~a "MPFR is released under the GNU Lesser General Public License (GNU LGPL),\n"
                      "either version 3 of the license, or (at your option) any later version.")
                 LGPL-3.0-or-later])]

    ["draw-x11"
     ""
     "racket/draw/x11"
     ""
     ()
     #t
     "draw"
     #f
     (["libX11.6" "libX11 is released under the X.Org Foundation license."
                  X11]
      ["libXau.6" "libXau - Copyright 1988, 1993, 1994, 1998  The Open Group"
                  X11]
      ["libxcb-shm.0" "libxcb - Copyright (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett."
                      X11]
      ["libxcb-render.0" #f X11]
      ["libxcb.1" #f X11]
      ["libXext.6" "libXext - Copyright 1986, 1987, 1988, 1989, 1994, 1998  The Open Group"
                   X11]
      ["libXrender.1" "libXrender - Copyright © 2001,2003 Keith Packard"
                      X11])]
    ["draw-ttf"
     ""
     "racket/draw/ttf"
     ""
     ()
     #f
     "draw"
     #f
     (["fonts" ,(~a "GNU FreeFont\n"
                    "Copyleft 2002, 2003, 2005, 2008, 2009, 2010 Free Software Foundation.\n"
                    "\n"
                    "Free UCS scalable fonts is free software; you can redistribute it and/or\n"
                    "modify it under the terms of the GNU General Public License as published\n"
                    "by the Free Software Foundation; either version 3 of the License, or\n"
                    "(at your option) any later version.\n"
                    "\n"
                    "The fonts are distributed in the hope that they will be useful, but\n"
                    "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n"
                    "or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n"
                    "for more details.\n"
                    "\n"
                    "You should have received a copy of the GNU General Public License along\n"
                    "with this program; if not, write to the Free Software Foundation, Inc.,\n"
                    "51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.\n"
                    "\n"
                    "As a special exception, if you create a document which uses this font, and\n"
                    "embed this font or unaltered portions of this font into the document, this\n"
                    "font does not by itself cause the resulting document to be covered by the\n"
                    "GNU General Public License. This exception does not however invalidate any\n"
                    "other reasons why the document might be covered by the GNU General Public\n"
                    "License. If you modify this font, you may extend this exception to your\n"
                    "version of the font, but you are not obligated to do so.  If you do not\n"
                    "wish to do so, delete this exception statement from your version.")
               (GPL-3.0-or-later WITH Font-exception-2.0)])]

    ["gui"
     ""
     "racket/gui"
     ""
     ()
     #t
     #f
     "1.3" ; version
     (["libgtk-x11-2.0.0" "GTK+ is released under the GNU Library General Public License (GNU LGPL)."
                          LGPL-2.1-or-later]
      ["libatk" "ATK is released under the GNU Library General Public License (GNU LGPL)."
                LGPL-2.1-or-later]
      ["libgdk-x11-2.0.0" #f LGPL-2.1-or-later]
      ["libgdk_pixbuf-2.0.0" #f LGPL-2.1-or-later]
      ["PSMTabBarControl.framework" ,(~a "PSMTabBarControl is BSD licensed.\n"
                                         ;; no https
                                         "See: http://www.positivespinmedia.com/dev/PSMTabBarControl.html")
                                    BSD-3-clause]
      ["MMTabBarView.framework" ,(~a "MMTabBarView is BSD licensed.\n"
                                     "See: https://mimo42.github.io/MMTabBarView/")
                                BSD-3-clause])]

    ["db"
     ""
     "db"
     ""
     ()
     #t
     "base"
     #f
     (["libsqlite3.0" "SQLite3 is in the public domain."
                      blessing]
      ["sqlite3" "SQLite3 is in the public domain."
                 blessing])]
    
    ["poppler"
     ""
     "racket-poppler"
     ""
     ()
     #t
     "racket-poppler"
     #f
     (["libpoppler"
       ;; Note: Poppler is GPL and *not* in the main Racket distribution (which is LGPL)
       "Poppler is released under the GNU General Public License (GNU GPL)."
       GPL-2.0-or-later])])))

(define (framework? p)
  (regexp-match? #rx"[.]framework" p))

(define (plain-path? p)
  (or (equal? p "fonts")
      (framework? p)))

(define (revert-name p renames)
  (or (for/or ([pr (in-list renames)])
	(and (equal? (cadr pr) p)
	     (car pr)))
      p))

(define from (build-path (current-directory) "dest" (if win? "bin" "lib")))

(define (find-pkg lib)
  (define pkg (for/or ([p (in-list package-mapping)])
                (define nl
                  (for/or ([nl (in-list (pkg-spec-libs p))])
                    (define n (car nl))
                    (cond
                     [(equal? n lib) nl]
                     [else
                      (define len (string-length n))
                      (and ((string-length lib) . > . (add1 len))
                           (string=? n (substring lib 0 len))
                           (regexp-match? #rx"[-.0-9]" (string (string-ref lib len)))
                           nl)])))
                (and nl
                     (list (pkg-spec-name p)
                           (pkg-spec-suffix p)
                           (pkg-spec-subdir p)
                           (cadr nl)
                           (cddr nl)))))
  (unless pkg
    (error 'install "cannot find package for library: ~e" lib))
  (apply values pkg))

(define (write-setup-infotab o)
  (for-each (λ (s) (displayln s o))
            `("#lang setup/infotab"
              ";; SPDX-License-Identifier: (Apache-2.0 OR MIT)"
              ";; THIS FILE IS AUTO-GENERATED FROM racket/src/native-libs/install.rkt")))

(define (gen-info platform i-platform for-pkg pkg-name subdir libs lic-sexps lics lic-end comments lib? vers)
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
                           (or (framework? lib)
                               (directory-exists? (build-path dest subdir lib))))
                         libs))
    (unless (null? dirs)
      (newline o)
      (pretty-write `(define compile-omit-paths (quote ,dirs)) o)))
  (define (write-pkg o)
    (newline o)
    (pretty-write `(define collection 'multi) o)
    (pretty-write `(define deps '("base")) o)
    (newline o)
    (pretty-write `(define pkg-desc ,(format "~a for \"~a\" package"
                                             (if lib?
                                                 "native libraries"
                                                 "shared files")
                                             for-pkg))
                  o)
    (newline o)
    (pretty-write `(define pkg-authors '(mflatt)) o)
    (when vers
      (newline o)
      (pretty-write `(define version ,vers) o))
    (let* ([lic-sexps (remove-duplicates (cons '(Apache-2.0 OR MIT) lic-sexps))]
           [license
            (let loop ([this (car lic-sexps)]
                       [more (cdr lic-sexps)])
              (if (pair? more)
                  `(,this AND ,(loop (car more) (cdr more)))
                  this))])
      (newline o)
      (pretty-write `(define license ',license) o))
    (unless (null? comments)
      (newline o)
      (for ([comment (in-list comments)])
        (fprintf o ";; ~a\n" comment))))
  (unless same?
    (printf "Write ~a\n" lib-path)
    (call-with-output-file*
     lib-path
     #:exists 'truncate
     (lambda (o)
       (write-setup-infotab o)
       (write-libs o))))
  (printf "Write ~a\n" top-path)
  (call-with-output-file*
   top-path
   #:exists 'truncate
   (lambda (o)
     (write-setup-infotab o)
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
     (newline o)
     (displayln "The Racket code in this package is distributed under the Apache 2.0" o)
     (displayln "and MIT licenses. The user can choose the license under which they" o)
     (displayln "will be using the software. There may be other licenses within the" o)
     (displayln "distribution with which the user must also comply." o)
     (for ([l (in-list lics)])
       (newline o)
       (displayln l o))
     (display lic-end o))))

(define (install platform i-platform so fixup libs renames)
  (define pkgs (make-hash))
  (define pkgs-lic (make-hash))
  (define pkgs-lic-sexps (make-hash))

  (define (install lib)
    (define-values (p orig-p)
      (let ()
	(define (both v) (values v v))
	(cond
	 [(plain-path? lib) (both lib)]
	 [(procedure? so) (both (so lib))]
	 [else
	  (define (make lib) (format "~a.~a" lib so))
	  (values (make lib) (make (revert-name lib renames)))])))
    (define-values (pkg suffix subdir lic lic-sexps) (find-pkg lib))
    (when (null? lic-sexps)
      (error 'install "missing license for library: ~e\n  package: ~e" lib pkg))
    (define dir (build-path dest-dir
                            (~a pkg "-" platform suffix)
                            subdir))
    (define dest (build-path dir p))
    (let-values ([(base name dir?) (split-path dest)])
      (make-directory* base))
    (unless only-meta?
      (unless (member p no-copy-libs)
        (cond
          [(file-exists? dest) (delete-file dest)]
          [(directory-exists? dest) (delete-directory/files dest)])
        (define src (build-path from orig-p))
        (if (directory-exists? src)
            (copy-directory/files src dest)
            (copy-file src dest)))
      (unless (plain-path? p)
        (fixup p dest)))

    (hash-update! pkgs pkg (lambda (l) (cons p l)) '())
    (hash-update! pkgs-lic-sexps pkg (λ (l) (append lic-sexps l)) '())
    (when lic
      (hash-update! pkgs-lic pkg (lambda (l) (cons lic l)) '())))
  
  (for-each install libs)

  (for ([(pkg libs) (in-hash pkgs)])
    (define a
      (findf (λ (a)
               (equal? pkg (pkg-spec-name a)))
             package-mapping))
    (gen-info platform 
              i-platform
              (or (pkg-spec-for-pkg a) pkg)
              (~a pkg "-" platform (pkg-spec-suffix a))
              (pkg-spec-subdir a)
              libs
              (reverse (hash-ref pkgs-lic-sexps pkg))
              (reverse (hash-ref pkgs-lic pkg null))
              (pkg-spec-license-txt-extra a)
              (pkg-spec-info-rkt-comments a)
              (pkg-spec-lib? a)
              (pkg-spec-version a))))

(define (install-mac)
  (define (fixup p p-new)
    (unless (framework? p)
      (printf "Fixing ~s\n" p-new)
      (when aarch64?
	(system (format "codesign --remove-signature ~a" p-new)))
      (unless (memq 'write (file-or-directory-permissions p-new))
        (file-or-directory-permissions p-new #o744))
      (system (format "install_name_tool -id ~a ~a" (file-name-from-path p-new) p-new))
      (for-each (lambda (s)
                  (system (format "install_name_tool -change ~a @loader_path/~a ~a"
                                  (format "~a/~a.dylib" from (revert-name s renames))
                                  (format "~a.dylib" s)
                                  p-new)))
                (append libs nonwin-libs))
      (system (format "strip -S ~a" p-new))
      (when sign-as
	(system (format "codesign -s ~s --timestamp ~a" sign-as p-new)))))

  (define platform (~a (if m32? 
                           (if ppc? "ppc" "i386")
			   (if aarch64? "aarch64" "x86_64"))
                       "-macosx"))

  (define renames (if aarch64?
                      aarch64-renames
                      null))

  (install platform platform "dylib" fixup
           (append libs
                   (cond
                     [ppc? '()]
                     [else mac-libs])
                   (cond
                     [m32? '()]
                     [else mac64-libs])
                   (cond
                     [aarch64? '()]
                     [else macx86-libs])
                   nonwin-libs)
           renames))

(define (install-win)
  (define exe-prefix (cond
                       [m32? "i686-w64-mingw32"]
                       [aarch64? "aarch64-w64-mingw32"]
                       [else "x86_64-w64-mingw32"]))

  (define renames (if aarch64?
                      aarch64-renames
                      null))
  
  (define (rename-one s)
    (regexp-replace #rx"!"
                    (regexp-replace* #rx"[.]"
                                     (regexp-replace #rx"[.](?=.*[.])" s "!")
                                     "-")
                    "."))

  (define win-renames
    (map (lambda (p) (list (rename-one (car p)) (rename-one (cadr p))))
         renames))

  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (system (~a exe-prefix "-strip -S " p-new))
    (define-values (i o) (open-input-output-file p-new #:exists 'update))
    (for-each (lambda (p)
                (let loop ()
                  (file-position i 0)
                  (define m (regexp-match-positions (regexp-quote (car p)) i))
                  (when m
                    (file-position o (caar m))
                    (display (cadr p) o)
                    (flush-output o)
                    (loop))))
              win-renames)
    (close-input-port i)
    (close-output-port o))

  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PATH" (~a (if m32?
                           "/usr/local/mw32/bin:/usr/mw32/bin:"
                           "/usr/local/mw64/bin:/usr/mw64/bin:")
                       (getenv "PATH")))

    (install (~a "win32-" (if m32? "i386" (if aarch64? "arm64" "x86_64")))
             (~a "win32\\" (if m32? "i386" (if aarch64? "arm64" "x86_64")))
             "dll"
             fixup
             (for/list ([s (in-list (append libs
                                            win-libs))])
               (rename-one s))
             win-renames)))

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
    (define special-cases
      '("libpangoft2-1.0"
        "libpangocairo-1.0"
        "libpango-1.0"
        "libgobject-2.0"
        "libglib-2.0"
        "libgthread-2.0"
        "libgmodule-2.0"
        "libgio-2.0"
        "libgdk_pixbuf-2.0"
        "libatk-1.0"
        "libgdk-x11-2.0"
        "libgtk-x11-2.0"))
    (let loop ([p orig-p] [suffix ""])
      (define p-so (string-append p ".so" suffix))
      (cond
       [(or (file-exists? (build-path from p-so))
            (and only-meta? (member p special-cases)))
        p-so]
       [else
        (define m (regexp-match #rx"^(.*)[.](.*)$" p))
        (cond
         [m
          (loop (cadr m) (string-append "." (caddr m) suffix))]
         [only-meta?
          p-so]
         [else
          (error 'add-so "not found: ~s" orig-p)])])))

  (install platform platform add-so fixup
           (append (remove* linux-remove-libs
                            libs)
                   nonwin-libs
                   linux-libs)
           null))

(cond
 [win? (install-win)]
 [linux? (install-linux)]
 [else (install-mac)])
