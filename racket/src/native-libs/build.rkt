#lang at-exp racket/base
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

(define-runtime-path longdouble-c "../bc/src/longdouble/longdouble.c")
(define-runtime-path longdouble-h "../bc/src/longdouble/longdouble.h")

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

;; Fix a problem with blank glyphs triggering Type 3 substitutions:
(define-runtime-path cairo-emptyglyph.patch "patches/cairo-emptyglyph.patch")

;; Hack to workaround broken Courier New in Mac OS 10.{7.8}:
(define-runtime-path courier-new-patch "patches/courier-new.patch")

;; Enable kerning and set DPI to 72:
(define-runtime-path coretext-patch "patches/coretext.patch")

;; Support registration of extra font families:
(define-runtime-path coretext-fontreg-patch "patches/coretext-fontreg.patch")

;; Avoid crash when CTFontCollectionCreateMatchingFontDescriptors fails:
(define-runtime-path coretext-nullarray "patches/coretext-nullarray.patch")

;; MinGW doesn't like `-Wp,-D_FORTIFY_SOURCE=2`, at least not without
;; linking extra libraries:
(define-runtime-path cairo-nofortfy-patch "patches/cairo-nofortify.patch")

;; Adds cairo_quartz_get_cg_context_with_clip, which is based on
;; https://hg.mozilla.org/mozilla-central/file/tip/gfx/cairo/native-clipping.patch
(define-runtime-path cairo-cg-surface-patch "patches/cairo-cg-surface.patch")

;; Define some functions that aren't in Mac OS 10.5 (for the 32-bit build)
(define-runtime-path pango-surrogate-patch "patches/pango-surrogate.patch")

;; Enable "symbol" fonts, and fix off-by-one:
(define-runtime-path win32text-patch "patches/win32text.patch")

;; Disable emoji-specific font, which intereferes with substitutions
;; (i.e., auto-find a suitable font) as implemented by `racket/draw`
(define-runtime-path pango-emoji-patch "patches/pango-emoji.patch")

;; Merge a Pango patch that fixes a decoding problem
(define-runtime-path pango-emojiiter-patch "patches/pango-emojiiter.patch")

;; Allow more flexible font matching
(define-runtime-path pango-match-patch "patches/pango-match.patch")

;; Detect oblique before italic on Mac OS
(define-runtime-path pango-preferoblique-patch "patches/pango-preferoblique.patch")

;; Add `-lusp10` before `-lgdi32` to preserve support for Windows 7
(define-runtime-path pango-usp10-patch "patches/pango-usp10.patch")

;; Needed when building with old GCC, such as 4.0:
(define-runtime-path gmp-weak-patch "patches/gmp-weak.patch")

;; For `getline` on 32-bit Mac OS 10.6:
(define-runtime-path libedit-getline-patch "patches/libedit-getline.patch")

;; strerror_s is not available in XP
(define-runtime-path glib-strerror-patch "patches/glib-strerror.patch")

;; Remove "-fno-check-new", which Clang does not recognize:
(define-runtime-path nonochecknew-patch "patches/nonochecknew.patch")

;; Remove `volatile` declaration
(define-runtime-path poppler-no-volatile-patch "patches/poppler-no-volatile.patch")

;; 64-bit MinGW doesn't like this use of `__always_inline__`:
(define-runtime-path noforceinline-patch "patches/noforceinline.patch")

;; `vector` syntax with old gcc
(define-runtime-path pixman-altivec-patch "patches/pixman-altivec.patch")

;; No need for pixman demos and tests
(define-runtime-path pixman-notest-patch "patches/pixman-notest.patch")

;; Disable pthread use for pixman on Windows
(define-runtime-path pixman-nopthread-patch "patches/pixman-nopthread.patch")

;; Disable libtool's management of standard libs so that
;; MinGW's -static-libstdc++ works:
(define-runtime-path libtool-link-patch "patches/libtool-link.patch")
(define-runtime-path libtoolhb-link-patch "patches/libtoolhb-link.patch")

;; Add FcSetFallbackDirs to set fallback directories dynamically:
(define-runtime-path fcdirs-patch "patches/fcdirs.patch")
(define-runtime-path fonts-conf "patches/fonts.conf")

;; Skip `fc-config` on install:
(define-runtime-path fc-config-patch "patches/fc-config.patch")

;; Avoid problems compiling with an old version of g++
(define-runtime-path harfbuzz-oldcompiler-patch "patches/harfbuzz-oldcompiler.patch")

;; Adapt inline-function handling for an old gcc
(define-runtime-path gmp-inline-patch "patches/gmp-inline.patch")

;; Configure for AArch64
(define-runtime-path openssl-aarch64osx-patch "patches/openssl-aarch64osx.patch")
(define-runtime-path openssl-aarch64nt-patch "patches/openssl3-aarch64nt.patch")

;; libffi via MinGW for AArch64:
(define-runtime-path libffi-arm64nt-patch "patches/libffi-arm64nt.patch")
  
;; --------------------------------------------------

(define (replace-in-file file orig new)
  (define rx (regexp-quote orig))
  (define-values (i o) (open-input-output-file file #:exists 'update))
  (define pos (caar (regexp-match-positions rx i)))
  (file-position o pos)
  (write-bytes new o)
  (close-output-port o)
  (close-input-port i))

;; --------------------------------------------------
;; General environment and flag configuration:

(define win-prefix (cond
                     [m32? "i686-w64-mingw32"]
                     [aarch64? "aarch64-w64-mingw32"]
                     [else "x86_64-w64-mingw32"]))

;; Build GNU sed to avoid potential BSD sed:
(define need-sed? win?)

(define (sdk n)
  (~a " -isysroot /usr/local/Developer/SDKs/MacOSX10."n".sdk -mmacosx-version-min=10."n))
(define mac32-sdk 6)
(define mac64-sdk 9)

(define all-env
  (cond
   [win?
    (case package-name
      [("pkg-config" "sed")
       ;; pkgconfig and sed run on build platform:
       null]
      [("openssl-1" "openssl-3")
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
     [aarch64?
      (define flags "-arch arm64 -mmacosx-version-min=11")
      (list
       (list "CPPFLAGS" (~a flags))
       (list "LDFLAGS" (~a flags)))]
     [m32?
      (define sdk-flags (sdk mac32-sdk))
      (list
       (list "CPPFLAGS" (~a "-m32" sdk-flags))
       (list "LDFLAGS" (~a "-m32" sdk-flags
                           ;; suppress deprecation warning:
                           " -Wl,-w")))]
     [else
      (define sdk-flags (sdk mac64-sdk))
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

(define (make-windows-cross_file.txt cpu)
  (define content
    @~a{[host_machine]
        system = 'windows'
        cpu_family = '@|cpu|'
        cpu = '@|cpu|'
        endian = 'little'
        
        [properties]
        c_args = ['-I@|dest|/include']
        c_link_args = ['-static-libgcc', '-L@|dest|/lib']
        
        [binaries]
        c = '@|cpu|-w64-mingw32-gcc'
        cpp = '@|cpu|-w64-mingw32-g++'
        ar = '@|cpu|-w64-mingw32-ar'
        ld = '@|cpu|-w64-mingw32-ld'
        objcopy = '@|cpu|-w64-mingw32-objcopy'
        strip = '@|cpu|-w64-mingw32-strip'
        pkgconfig = 'pkg-config'
        windres = '@|cpu|-w64-mingw32-windres'})
  (call-with-output-file*
   "cross_file.txt"
   #:exists 'truncate
   (lambda (out)
     (displayln content out))))

(define (make-all-args use-cross-file)
  (append
   (list (~a "--prefix=" dest))
   (cond
    [win?
     (case package-name
       [("pkg-config" "sed")
        ;; pkgconfig and sed run on build platform:
        null]
       [("openssl-1" "openssl-3")
        ;; not the usual "configure"
        null]
       [("zlib")
        ;; zlib doesn't understand --host=
        null]
       [else
        ;; Everything else cross-compiles normally:
        (cond
          [use-cross-file
           (list "--cross-file" "cross_file.txt")]
          [m32?
           (list "--host=i686-w64-mingw32")]
          [aarch64?
           (list "--host=aarch64-w64-mingw32")]
          [else
           (list "--host=x86_64-w64-mingw32")])])]
    [else null])
   (case package-name
     [("openssl-1" "openssl-3")
      ;; Especially for the natipkg build, but it makes sense
      ;; to suppress the path (which records the build location)
      ;; on all platforms:
      (list "--openssldir=/RACKET_USE_ALT_PATH")]
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
                #:install-patches [install-patches null]
                #:fixup [fixup #f]
                #:fixup-proc [fixup-proc #f]
                #:use-cross-file [use-cross-file #f])
  (for ([d (in-list (append (if (or (equal? package-name "pkg-config")
                                    (equal? package-name "sed"))
                                '()
                                (append
                                 '("pkg-config")
                                 (if need-sed? '("sed") '())))
                            deps))])
    (unless (file-exists? (build-path dest "stamps" d))
      (error 'build "prerequisite needed: ~a" d)))
  (values env exe args make make-install setup patches post-patches install-patches fixup fixup-proc
          use-cross-file))

(define path-flags
  (list (list "CPPFLAGS" (~a "-I" dest "/include"))
        (list "LDFLAGS" (~a "-L" dest "/lib"))))

(define ld-library-path-flags
  (list (list "LD_LIBRARY_PATH"
	      (path->string (build-path dest "lib")))))

(define (add-flag env var val)
  (cond
    [(equal? val "") env]
    [else
     (let loop ([env env])
       (cond
         [(null? env) (list (list var val))]
         [(equal? (caar env) var)
          (cons (list var (string-append (cadar env)
                                         " "
                                         val))
                (cdr env))]
         [else (cons (car env) (loop (cdr env)))]))]))

(define (nonmac-only)
  (unless (or win? linux?)
    (error (format "build ~a only for Windows or Linux" package-name))))

(define (linux-only)
  (unless linux?
    (error (format "build ~a only for Linux" package-name))))

(define-values (extra-env configure-exe extra-args make-command make-install-command 
                          setup patches post-patches install-patches fixup fixup-proc
                          use-cross-file)
  (case package-name
    [("pkg-config") (config #:configure (list "--with-internal-glib"))]
    [("sed") (config)]
    [("longdouble") (config)]
    [("libedit") (config
                  #:patches (if (and mac? m32?)
                                (list libedit-getline-patch)
                                null))]
    [("libiconv")
     (nonmac-only)
     (config #:configure '("--enable-extra-encodings"))]
    [("sqlite")
     (nonmac-only)
     (config #:fixup (and win?
                          (~a "cd " (build-path dest "bin")
                              " && mv libsqlite3-0.dll sqlite3.dll")))]
    [("openssl-1" "openssl-3")
     (define make
       (if linux?
           (~a "make SHARED_LDFLAGS=" "-Wl,-rpath," dest "/lib")
           "make"))
     (define vers (if aarch64? #"3" #"1_1"))
     (config #:configure-exe (find-executable-path "perl")
             #:configure (cond
                          [win?
                           (append
                            (list "./Configure"
                                  (~a "--cross-compile-prefix=" win-prefix "-")
                                  #f ; other flags here
                                  (~a "mingw" (if m32? "" (if aarch64? "-arm64" "64")))
                                  "shared")
                            (if aarch64?
				'("no-asm")
				null))]
                          [mac?
			   (append
                            (list "./Configure"
                                  #f
                                  "shared"
                                  (cond
                                   [ppc? "darwin-ppc-cc"]
                                   [m32? "darwin-i386-cc"]
                                   [aarch64? "darwin64-aarch64-cc"]
                                   [else "darwin64-x86_64-cc"])
                                  (car (regexp-match #rx"-mmacosx-version-min=[0-9.]*"
                                                     (cadr (assoc "CPPFLAGS" all-env)))))
			    (if aarch64?
				'("no-asm")
				null))]
                          [else
                           (list "./Configure"
                                 #f
                                 "shared"
                                 "linux-x86_64")])
	     #:make make
             #:make-install (~a make " install_sw")
	     #:patches (if (and win? aarch64?)
                           (list openssl-aarch64nt-patch)
                           (list openssl-aarch64osx-patch))
             #:fixup (and win?
                          (~a "cd " (build-path dest "bin")
                              " && mv libssl-" vers (if (or m32? aarch64?) "" "-x64") ".dll ssleay32.dll"
                              " && mv libcrypto-" vers (if (or m32? aarch64?) "" "-x64") ".dll libeay32.dll"))
             #:fixup-proc (and win?
                               (lambda ()
                                 (replace-in-file (build-path dest "bin" "ssleay32.dll")
                                                  (bytes-append #"libcrypto-" vers (if (or m32? aarch64?) #"" #"-x64") #".dll\0")
                                                  #"libeay32.dll\0"))))]
    [("expat") (config)]
    [("gettext") (config #:depends (if win? '("libiconv") '())
                         #:configure (append
                                      '("--enable-languages=c")
                                      (if win?
                                          '("--enable-threads=windows")
                                          null))
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
     (config #:depends (if linux?
                           '("libX11")
                           '())
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
    [("libffi")
     (cond
       [(and mac? aarch64?)
        (config #:configure '("-host=aarch64-apple-darwin"))]
       [(and win? aarch64?)
        (config #:env (list (list "CPPFLAGS" "-D_M_ARM64"))
                #:patches (list libffi-arm64nt-patch))]
       [else
        (config)])]
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
                      #:configure-exe (find-executable-path "meson")
                      #:use-cross-file (and win? (cond
                                                   [aarch64? "aarch64"]
                                                   [m32? "i686"]
                                                   [else "x86_64"]))
                      #:make "meson compile -C _build"
                      #:make-install "meson install -C _build"
                      #:configure (append '("setup")
                                          ;; '("-Dinternal_pcre=true")
                                          (if linux? '("-Dlibmount=disabled") '())
                                          '(#f "_build"))
                      #:env (add-flag (add-flag path-flags
                                                ;; Disable Valgrind support, which particularly
                                                ;; goes wrong for 64-bit Windows builds.
                                                "CPPFLAGS" (string-append
                                                            "-DNVALGRIND=1"
                                                            (if mac?
                                                                " -include Kernel/uuid/uuid.h"
                                                                "")))
                                      "LDFLAGS" (if (and win? (not aarch64?))
                                                    "-Wl,--allow-multiple-definition"
                                                    ""))
                      #:patches (cond
                                  [win? (list glib-strerror-patch)]
                                  [else null]))]
    [("libpng") (config #:depends (if (or win? linux?) '("zlib") '())
                        #:env (if (or linux? win?)
                                  (append
                                   path-flags
                                   (if linux?
                                       (list (list "LDFLAGS" (~a "-Wl,-rpath," dest "/lib")))
                                       null))
                                  null))]
    [("libuuid") (config)]
    [("freetype") (config #:depends '("libpng"))]
    [("fontconfig") (config #:depends (append '("expat" "freetype")
                                              (if win? '() '("libuuid")))
                            #:configure (append '("--disable-docs")
                                                (if win?
                                                    `("--without-libiconv-prefix"
                                                      "--without-libintl-prefix")
                                                    '()))
                            #:patches (list fcdirs-patch)
			    #:install-patches (cond
					       [(and mac? aarch64?) (list fc-config-patch)]
					       [else null]))]
    [("pixman") (config #:patches (append
                                   (cond
                                     [(and win? (not m32?)) (list noforceinline-patch)]
                                     [ppc? (list pixman-altivec-patch)]
                                     [else null])
                                   (cond
                                     [win? (list pixman-nopthread-patch)]
                                     [else (list)])
                                   (list pixman-notest-patch)))]
    [("cairo")
     (when mac?
       (define zlib.pc (build-path dest "lib" "pkgconfig" "zlib.pc"))
       (unless (file-exists? zlib.pc)
         (call-with-output-file*
          zlib.pc
          (lambda (o) (write-string "Name: zlib\nDescription: zlib\nVersion: 1.0\nLibs: -lz\nLibs.private:\nCflags:\n" o)))))
     (config #:depends (append '("pixman" "fontconfig" "freetype" "libpng")
                               (if linux?
                                   '("libX11" "libXrender")
                                   null))
             #:env path-flags
             #:configure (append
                          (if (not linux?)
                              '("--enable-xlib=no")
                              null)
                          '("png_REQUIRES=libpng16")
                          (if mac?
                              '("CFLAGS=-include Kernel/uuid/uuid.h")
                              '()))
             #:patches (append
                        (list cairo-emptyglyph.patch
                              cairo-coretext-patch
                              courier-new-patch
                              cairo-cg-surface-patch)
                        (if win?
                            (list cairo-nofortfy-patch)
                            null)))]
    [("harfbuzz") (config #:depends '("fontconfig" "freetype" "cairo")
                          #:configure '("--without-icu")
                          #:env cxx-env
                          #:patches (if ppc?
                                        (list harfbuzz-oldcompiler-patch)
                                        null)
                          #:post-patches (if (and win? aarch64?)
                                             (list libtoolhb-link-patch)
                                             null))]
    [("fribidi") (config #:configure '("--disable-docs"))]
    [("pango") (config #:depends '("cairo" "harfbuzz" "fribidi")
                       #:env (if win? path-flags null)
                       #:configure (append
				    (if (not linux?)
					'("--without-x")
					null)
				    '("--with-included-modules=yes"
				      "--with-dynamic-modules=no")
                                    (if mac?
                                        '("CFLAGS=-include Kernel/uuid/uuid.h")
                                        '()))
                       #:patches (append
                                  (list coretext-patch
                                        coretext-fontreg-patch
                                        coretext-nullarray
                                        win32text-patch
                                        pango-emojiiter-patch
                                        pango-match-patch)
                                  (if mac?
                                      (list pango-preferoblique-patch)
                                      null)
                                  (if (and mac? m32? (mac32-sdk . < . 6))
                                      (list pango-surrogate-patch)
                                      null)
                                  (if (or mac? win?)
                                      (list pango-emoji-patch)
                                      null)
                                  (if (and win? (not aarch64?))
                                      (list pango-usp10-patch)
                                      null)))]
    [("gmp") (config #:patches (cond
                                 [gcc-4.0?
                                  (list gmp-weak-patch)]
                                 [else null])
                     #:configure (append
                                  '("--enable-shared" "--disable-static")
                                  (if (and linux? (not (or m32? aarch64?)))
                                      '("--host=core2-linux-gnu") ; core2 for portability
                                      null)
                                  (if (and win? aarch64?)
                                      '("--disable-assembly")
                                      '())
				  (if (and mac? aarch64?)
				      '("-host=aarch64-apple-darwin")
				      null)
                                  (if (and mac? (not ppc?))
                                      '("--build=corei-apple-darwin")
                                      null)
                                  (if (and m32? mac?)
                                      (list "ABI=32")
                                      null))
                     #:post-patches (if (and mac? ppc?)
                                        (list gmp-inline-patch)
                                        null))]
    [("mpfr") (config #:configure (append (if (and #f win?) ; creates dependency on "libwinpthread-1.dll"
                                              '("--enable-thread-safe")
                                              null)
                                          '("--enable-shared" "--disable-static"))
                      #:depends '("gmp")
                      #:env path-flags)]
    [("jpeg") (config)]
    [("poppler") (config #:env (append path-flags
                                       cxx-env)
                         #:patches (list nonochecknew-patch
                                         poppler-no-volatile-patch)
                         #:post-patches (if win?
                                            (list libtool-link-patch)
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

(define (remove-libtool-flat-namespace)
  ;; old versions of libtool fail to detect latest Mac OS and
  ;; add ancient `-flat_namespace` flag
  (when (file-exists? "libtool")
    (define s (file->string "libtool"))
    (define s2 (regexp-replace #rx"\\\\[$]wl-flat_namespace \\\\[$]wl-undefined \\\\[$][{]wl[}]suppress"
                               s
                               "\\\\$wl-undefined \\\\${wl}dynamic_lookup"))
    (unless (equal? s s2)
      (call-with-output-file*
       "libtool"
       #:exists 'truncate
       (lambda (o) (display s2 o))))))

(parameterize ([current-directory package-dir]
               [current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))])
  (putenv "PATH" (~a dest "/bin"
                     ":"
                     (if win?
                         (if m32?
                             "/usr/local/mw32/bin:/usr/mw32/bin:"
                             "/usr/local/mw64/bin:/usr/mw64/bin:")
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
  (when use-cross-file
    (make-windows-cross_file.txt use-cross-file))
  (unless skip-config?
    (apply system*/show
           (or configure-exe "./configure")
           (let loop ([extra-args extra-args])
             (cond
              [(null? extra-args) (make-all-args use-cross-file)]
              [(not (car extra-args)) (append (make-all-args use-cross-file) (cdr extra-args))]
              [else (cons (car extra-args) (loop (cdr extra-args)))])))
    (for ([p (in-list post-patches)])
      (system/show (~a "patch -p2 < " p))))
  (remove-libtool-flat-namespace)
  (system/show make-command)
  (for ([p (in-list install-patches)])
    (system/show (~a "patch -p2 < " p)))
  (system/show make-install-command)
  (when fixup
    (system/show fixup))
  (when fixup-proc
    (fixup-proc))
  (stamp package-name)
  (displayln "Success!"))
