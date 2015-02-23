#lang racket/base
(require racket/system
         racket/format
         racket/string
         racket/file
         racket/runtime-path
         "cmdline.rkt")

(define skip-unpack? #f)
(define skip-config? #f)

(define package-name
  (build-command-line
   #:once-each
   [("--skip-unpack") "Skip `unpack` step"
    (set! skip-unpack? #t)]
   [("--skip-config") "Skip `configure` step"
    (set! skip-config? #t)]
   #:args (package-name)
   package-name))

;; --------------------------------------------------
;; Shell commands:

(define (system/show s)
  (displayln s)
  (unless (system s)
    (exit 1)))

(define (system*/show . s)
  (displayln (string-join (map (lambda (v) (if (path? v) (path->string v) v))
                               s)
                          " "))
  (unless (apply system* s)
    (exit 1)))

;; --------------------------------------------------
;; Unpack package and find package's source directory in the current
;; directory:

(define (find-package package-name dir? [fail-ok? #f])
  (define candidates
    (for/list ([f (in-list (directory-list))]
               #:when (if dir?
                          (directory-exists? f)
                          (file-exists? f))
               #:when (let ([s (path->string f)])
                        (and ((string-length s) . > . (string-length package-name))
                             (string=? (substring s 0 (string-length package-name))
                                       package-name))))
    f))

  (cond
   [(and fail-ok? (null? candidates))
    #f]
   [else
    (when (null? candidates)
      (error 'build "could not find ~a for package: ~a" 
             (if dir? "directory" "archive")
             package-name))
    (unless (null? (cdr candidates))
      (error 'build "found multiple ~a matches for package: ~s" 
             (if dir? "directory" "archive")
             candidates))
    (car candidates)]))

(define-runtime-path longdouble-c "../racket/src/longdouble/longdouble.c")
(define-runtime-path longdouble-h "../racket/src/longdouble/longdouble.h")

(unless skip-unpack?
  (case package-name
    [("longdouble")
     (make-directory* "longdouble-1")
     (copy-file longdouble-c "longdouble-1/longdouble.c" #t)
     (copy-file longdouble-h "longdouble-1/longdouble.h" #t)
     (when (file-exists? "longdouble-1/longdouble.dll")
       (delete-file "longdouble-1/longdouble.dll"))
     (with-output-to-file "longdouble-1/configure"
       #:exists 'truncate
       (lambda ()
         (printf "#!~a\n" (find-executable-path "sh"))
         (printf "echo 'longdouble.dll:' > Makefile\n")
         (printf "echo \"\t${CC} -shared -o longdouble.dll -DIMPLEMENTING_MSC_LONGDOUBLE=1 longdouble.c\" >> Makefile\n")
         (printf "echo '' >> Makefile\n")
         (printf "echo 'install:' >> Makefile\n")
         (printf "echo '\tcp longdouble.dll ../dest/bin' >> Makefile\n")))
     (file-or-directory-permissions "longdouble-1/configure" #o777)]
    [else
     (define archive (or (for/or ([archives-dir (in-list archives-dirs)])
                           (parameterize ([current-directory archives-dir])
                             (define p (find-package package-name #f #t))
                             (and p (build-path archives-dir p))))
                         (find-package package-name #f)))
     (define dir (find-package package-name #t #t))
     (when dir
       (printf "Removing ~a" dir)
       (delete-directory/files dir))
     (system/show (~a "tar zxf " archive))]))

(define package-dir (find-package package-name #t))

;; --------------------------------------------------
;; Create destination:

(unless (directory-exists? "dest")
  (make-directory "dest"))

(define dest (path->complete-path "dest"))

;; --------------------------------------------------
;; Patches:

;; Fix a problem with glyph extents and clipped rendering:
(define-runtime-path cairo-coretext-patch "patches/cairo-coretext.patch")

;; Hack to workaround broken Courier New in Mac OS X 10.{7.8}:
(define-runtime-path courier-new-patch "patches/courier-new.patch")

;; Enable kerning and set DPI to 72:
(define-runtime-path coretext-patch "patches/coretext.patch")

;; Enable "symbol" fonts, and fix off-by-one:
(define-runtime-path win32text-patch "patches/win32text.patch")

;; Fix a problem with a surface connected to a clipped drawing context
(define-runtime-path win32cairofallback-patch "patches/win32cairofallback.patch")

;; Needed when building with old GCC, such as 4.0:
(define-runtime-path gmp-weak-patch "patches/gmp-weak.patch")

;; XP doesn't have rand_s() as used by glib:
(define-runtime-path rand-patch "patches/rand.patch")

;; HarfBuzz makefile seems broken for MinGW as of 0.9.27:
(define-runtime-path fixdef-patch "patches/fixdef.patch")

;; Remove "-fno-check-new", which Clang does not recognize:
(define-runtime-path nonochecknew-patch "patches/nonochecknew.patch")

;; 64-bit MinGW doesn't like this use of `__always_inline__`:
(define-runtime-path noforceinline-patch "patches/noforceinline.patch")

;; Disable libtool's management of standard libs so that
;; MinGW's -static-libstdc++ works:
(define-runtime-path libtool-link-patch "patches/libtool-link.patch")
(define-runtime-path libtool64-link-patch "patches/libtool64-link.patch")

;; Add FcSetFallbackDirs to set fallback directories dynamically:
(define-runtime-path fcdirs-patch "patches/fcdirs.patch")
(define-runtime-path fonts-conf "patches/fonts.conf")

;; --------------------------------------------------
;; General environment and flag configuration:

(define win-prefix (if m32?
                       "i686-w64-mingw32"
                       "x86_64-w64-mingw32"))

;; Build GNU sed to avoid potential BSD sed:
(define need-sed? win?)

(define (sdk n)
  (~a " -isysroot /Developer/SDKs/MacOSX10."n".sdk -mmacosx-version-min=10."n))

(define all-env
  (cond
   [win?
    (case package-name
      [("pkg-config" "sed")
       ;; pkgconfig and sed run on build platform:
       null]
      [("openssl")
       ;; Not libtool, and prefix added automatically
       (list
        (list "CC" "gcc -static-libgcc"))]
      [else
       (list
        ;; We'd prefer to add "-static-libgcc" to CFLAGS, but
        ;; libtool doesn't pass `static-libgcc` through.
        (list "CC" (~a win-prefix "-gcc -static-libgcc")))])]
   [mac?
    (cond
     [m32?
      (define sdk-flags (sdk 5))
      (list
       (list "CPPFLAGS" (~a "-m32" sdk-flags))
       (list "LDFLAGS" (~a "-m32" sdk-flags)))]
     [else
      (define sdk-flags (sdk 6))
      (list
       (list "CPPFLAGS" (~a "-m64" sdk-flags))
       (list "LDFLAGS" (~a "-m64" sdk-flags)))])]
   [else
    (cond
     [m32?
      (list
       (list "CPPFLAGS" "-m32")
       (list "LDFLAGS" "-m32"))]
     [else
      null])]))

(define cxx-env
  (if win?
      (list
       (list "CXX" (~a win-prefix "-g++ -static-libgcc -static-libstdc++")))
      null))

(define all-args
  (append
   (list (~a "--prefix=" dest))
   (cond
    [win?
     (case package-name
       [("pkg-config" "sed")
        ;; pkgconfig and sed run on build platform:
        null]
       [("openssl")
        ;; not the usual "configure"
        null]
       [("zlib")
        ;; zlib doesn't understand --host=
        null]
       [else
        ;; Everything else cross-compiles normally:
        (cond
         [m32?
          (list "--host=i686-w64-mingw32")]
         [else
          (list "--host=x86_64-w64-mingw32")])])]
    [else null])))

(define (merge e1 e2)
  (define ht
    (for/fold ([ht (hash)]) ([e (in-list (append e1 e2))])
      (define v (hash-ref ht (car e) #f))
      (if v
          (hash-set ht (car e) (string-append v " " (cadr e)))
          (hash-set ht (car e) (cadr e)))))
  (for/list ([(k v) (in-hash ht)])
    (list k v)))

(define gcc-4.0?
  (and mac?
       (let ([o (open-output-bytes)])
         (parameterize ([current-output-port o])
           (system "gcc -v"))
         (regexp-match? #rx"gcc version 4[.]0" (get-output-bytes o)))))
(when gcc-4.0? (printf "using gcc 4.0"))

;; --------------------------------------------------
;; Package-specific environment and flag configuration:

(define (config #:depends [deps null]
                #:env [env null]
                #:configure-exe [exe #f]
                #:configure [args null]
                #:make [make "make"]
                #:make-install [make-install (~a make " install")]
                #:setup [setup null]
                #:patches [patches null]
                #:post-patches [post-patches null]
                #:fixup [fixup #f])
  (for ([d (in-list (append (if (or (equal? package-name "pkg-config")
                                    (equal? package-name "sed"))
                                '()
                                (append
                                 '("pkg-config")
                                 (if need-sed? '("sed") '())))
                            deps))])
    (unless (file-exists? (build-path dest "stamps" d))
      (error 'build "prerequisite needed: ~a" d)))
  (values env exe args make make-install setup patches post-patches fixup))

(define path-flags
  (list (list "CPPFLAGS" (~a "-I" dest "/include"))
        (list "LDFLAGS" (~a "-L" dest "/lib"))))

(define ld-library-path-flags
  (list (list "LD_LIBRARY_PATH"
	      (path->string (build-path dest "lib")))))

(define (nonmac-only)
  (unless (or win? linux?)
    (error (format "build ~a only for Windows or Linux" package-name))))

(define (linux-only)
  (unless linux?
    (error (format "build ~a only for Linux" package-name))))
  
(define-values (extra-env configure-exe extra-args make-command make-install-command 
                          setup patches post-patches fixup)
  (case package-name
    [("pkg-config") (config #:configure (list "--with-internal-glib"))]
    [("sed") (config)]
    [("longdouble") (config)]
    [("libiconv")
     (nonmac-only)
     (config)]
    [("sqlite")
     (nonmac-only)
     (config #:fixup (and win?
                          (~a "cd " (build-path dest "bin")
                              " && mv libsqlite3-0.dll sqlite3.dll")))]
    [("openssl")
     (nonmac-only)
     (config #:configure-exe (find-executable-path "sh")
             #:configure (if win?
			     (list "./Configure"
				   (~a "--cross-compile-prefix=" win-prefix "-")
				   #f ; other flags here
				   (~a "mingw" (if m32? "" "64"))
				   "shared")
			     (list "./Configure"
				   #f
				   "shared"
				   "linux-x86_64"))
	     #:make (if linux?
			(~a "make SHARED_LDFLAGS=" "-Wl,-rpath," dest "/lib")
			"make"))]
    [("expat") (config)]
    [("gettext") (config #:depends (if win? '("libiconv") '())
                         #:configure '("--enable-languages=c")
                         #:make (if win?
                                    ;; We only need libintl, and building
                                    ;; only that avoids other problems.
                                    "cd gettext-runtime/intl && make"
                                    "make"))]
    [("inputproto"
      "xproto"
      "xtrans"
      "kbproto"
      "xextproto"
      "renderproto"
      "libpthread-stubs"
      "libXau"
      "xcb-proto"
      "libxcb"
      "libX11"
      "libXext"
      "libXrender")
     (linux-only)
     (config #:env path-flags)]
    [("gdk-pixbuf")
     (linux-only)
     (config #:depends '("libX11")
	     #:configure '("--without-libtiff")
	     #:env (append path-flags
			   ld-library-path-flags))]
    [("atk")
     (linux-only)
     (config #:depends '("libX11")
	     #:env (append path-flags
			   ld-library-path-flags))]
    [("gtk+")
     (linux-only)
     (config #:depends '("gdk-pixbuf" "atk" "libXrender")
	     #:env (append path-flags
			   ld-library-path-flags))]
    [("freefont")
     (config #:configure-exe (find-executable-path "echo")
             #:make (~a "cp " fonts-conf " .")
             #:make-install (~a "rm -rf " dest "/lib/fonts"
                                " && mkdir -p " dest "/lib/fonts"
                                " && cp fonts.conf"
                                " FreeMono.ttf" 
                                " FreeSans.ttf" 
                                " FreeSerif.ttf" 
                                " " dest "/lib/fonts"))]
    [("libffi") (config)]
    [("zlib")
     (nonmac-only)
     (config #:make (if win?
			(~a "make -f win32/Makefile.gcc"
			    " PREFIX=" win-prefix "-"
			    " INCLUDE_PATH=" dest "/include"
			    " LIBRARY_PATH=" dest "/lib"
			    " BINARY_PATH=" dest "/bin"
			    " LDFLAGS=-static-libgcc")
			"make")
             #:fixup (and win?
			  (~a "cp zlib1.dll " dest "/bin && cp libz.dll.a " dest "/lib")))]
    [("glib") (config #:depends (append '("libffi" "gettext")
                                        (if win? '("libiconv") '()))
                      #:env (append path-flags
                                    ;; Disable Valgrind support, which particularly
                                    ;; goes wrong for 64-bit Windows builds.
                                    (list (list "CPPFLAGS" "-DNVALGRIND=1")))
                      #:patches (if (and win? m32?)
                                    (list rand-patch)
                                    null))]
    [("libpng") (config #:depends (if (or win? linux?) '("zlib") '())
                        #:env (if (or linux? win?)
				  (append
				   path-flags
				   (if linux?
				       (list (list "LDFLAGS" (~a "-Wl,-rpath," dest "/lib")))
				       null))
				  null))]
    [("freetype") (config #:depends '("libpng"))]
    [("fontconfig") (config #:depends '("expat" "freetype")
                            #:configure '("--disable-docs")
                            #:patches (list fcdirs-patch))]
    [("pixman") (config #:patches (if (and win? (not m32?))
                                      (list noforceinline-patch)
                                      null))]
    [("cairo") (config #:depends (append '("pixman" "fontconfig" "freetype" "libpng")
					 (if linux?
					     '("libX11" "libXrender")
					     null))
                       #:env path-flags
                       #:configure (if (not linux?)
				       '("--enable-xlib=no")
				       null)
                       #:patches (list cairo-coretext-patch
                                       courier-new-patch
                                       win32cairofallback-patch))]
    [("harfbuzz") (config #:depends '("fontconfig" "freetype" "cairo")
                          #:configure '("--without-icu")
                          #:patches (if win?
                                        (list fixdef-patch)
                                        null)
                          #:env cxx-env)]
    [("pango") (config #:depends '("cairo" "harfbuzz")
                       #:env (if win? path-flags null)
                       #:configure (append
				    (if (not linux?)
					'("--without-x")
					null)
				    '("--with-included-modules=yes"
				      "--with-dynamic-modules=no"))
                       #:patches (list coretext-patch
                                       win32text-patch))]
    [("gmp") (config #:patches (if gcc-4.0? (list gmp-weak-patch) null)
                     #:configure (append
                                  '("--enable-shared" "--disable-static")
                                  (if (and m32? mac?)
                                      (list "ABI=32")
                                      null)))]
    [("mpfr") (config #:configure (append (if win? '("--enable-thread-safe") null)
                                          '("--enable-shared" "--disable-static"))
                      #:depends '("gmp")
                      #:env path-flags)]
    [("jpeg") (config)]
    [("poppler") (config #:env (append path-flags
                                       cxx-env)
                         #:patches (list nonochecknew-patch)
                         #:post-patches (if win?
                                            (list (if m32?
                                                      libtool-link-patch
                                                      libtool64-link-patch))
                                            null)
                         #:configure '("--enable-zlib"
				       "--disable-splash-output"
				       "--disable-poppler-cpp"))]
    [else (error 'build "unrecognized package: ~a" package-name)]))

;; --------------------------------------------------
;; Go!

(define (stamp package-name)
  (make-directory* (build-path dest "stamps"))
  (call-with-output-file*
   (build-path dest "stamps" package-name)
   #:exists 'truncate
   void))

(parameterize ([current-directory package-dir]
               [current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))])
  (putenv "PATH" (~a dest "/bin"
                     ":"
                     (if win?
                         (if m32?
                             "/usr/mw32/bin:"
                             "/usr/mw64/bin:")
                         "")
                     (getenv "PATH")))
  (for ([e (in-list (merge all-env extra-env))])
    (printf "~a=~a\n" (car e) (cadr e))
    (putenv (car e) (cadr e)))
  (unless skip-unpack?
    (for ([s (in-list setup)])
      (system/show s))
    (for ([p (in-list patches)])
      (system/show (~a "patch -p2 < " p))))
  (unless skip-config?
    (apply system*/show
           (or configure-exe "./configure")
           (let loop ([extra-args extra-args])
             (cond
              [(null? extra-args) all-args]
              [(not (car extra-args)) (append all-args (cdr extra-args))]
              [else (cons (car extra-args) (loop (cdr extra-args)))])))
    (for ([p (in-list post-patches)])
      (system/show (~a "patch -p2 < " p))))
  (system/show make-command)
  (system/show make-install-command)
  (when fixup
    (system/show fixup))
  (stamp package-name)
  (displayln "Success!"))
