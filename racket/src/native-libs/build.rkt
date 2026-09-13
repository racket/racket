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

(define-syntax-rule (define-patch-path id path)
  (begin
    (define-runtime-path id path)
    (unless (file-exists? id) (error "patch file absent" path))))

;; Fix a problem with blank glyphs triggering Type 3 substitutions:
(define-patch-path cairo-emptyglyph-patch "patches/cairo-emptyglyph.patch")

;; Hack to workaround broken Courier New in Mac OS 10.{7.8}:
(define-patch-path courier-new-patch "patches/courier-new.patch")

;; Support registration of extra font families:
(define-patch-path coretext-fontreg-patch "patches/coretext-fontreg.patch")

;; Adds cairo_quartz_get_cg_context_with_clip, which is based on
;; https://hg.mozilla.org/mozilla-central/file/tip/gfx/cairo/native-clipping.patch
(define-patch-path cairo-cg-surface-patch "patches/cairo-cg-surface.patch")

;; Drop a glyph-advance hack that interferes with italic output to PDF
(define-patch-path cairo-quartz-advance-patch "patches/cairo-quartz-advance.patch")

;; When substitutions are handled by Pango/Cairo and a substition ends up
;; empty, then carry on with PDF writing anyway; that can happen when writing
;; "算法名称" with "Lucida Grande" on macOS Monterey, for example
(define-patch-path cairo-empty-font-subset-patch "patches/cairo-empty-font-subset.patch")

;; Turn off -lthread to avoid "libwinpthread-1.dll"
(define-patch-path cairo-win-pthread-patch "patches/cairo-win-pthread.patch")

;; Needed when building with old GCC, such as 4.0:
(define-patch-path gmp-weak-patch "patches/gmp-weak.patch")

;; For `getline` on 32-bit Mac OS 10.6:
(define-patch-path libedit-getline-patch "patches/libedit-getline.patch")

(define-patch-path glib-disable-gio-test-patch "patches/glib-disable-gio-test.patch")

;; Disable pthread use for pixman on Windows
(define-patch-path pixman-nopthread-patch "patches/pixman-nopthread.patch")

;; Add FcSetFallbackDirs to set fallback directories dynamically:
(define-patch-path fontconfig-dirs-patch "patches/fontconfig-dirs.patch")
(define-patch-path fonts-conf "patches/fonts.conf")

;; Skip `fc-config` on install:
(define-patch-path fc-config-patch "patches/fc-config.patch")

;; Adapt inline-function handling for an old gcc
(define-patch-path gmp-inline-patch "patches/gmp-inline.patch")

;; Configure for AArch64
(define-patch-path openssl-no-rcflags-patch "patches/openssl-no-rcflags.patch")
(define-patch-path openssl-aarch64nt-patch "patches/openssl3-aarch64nt.patch")

;; Avoid shared-mime-info and libxml2 dependency:
(define-patch-path gdk-pixbuf-no-sniff-patch "patches/gdk-pixbuf-no-sniff.patch")

;; Make the Gtk+ build work with a newer GDK that deprecates some bindings
(define-patch-path gtk-with-newer-gdk-patch "patches/gtk-with-newer-gdk.patch")

;; Disable test and demo executables
(define-patch-path gtk-no-demos-patch "patches/gtk-no-demos.patch")

;; Replacement "config.guess" for some old packages to add AArch64
(define-runtime-path config.guess "../lt/config.guess")

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
                     [i386? "i686-w64-mingw32"]
                     [aarch64? "aarch64-w64-mingw32"]
                     [x86_64? "x86_64-w64-mingw32"]
                     [else (error "missing Windows arch")]))

;; Build GNU sed to avoid potential BSD sed:
(define need-sed? win?)

(define (sdk n #:base [base 10])
  (if (or x86_64? aarch64?)
      (~a " -mmacosx-version-min="base"."n)
      (~a " -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX"base"."n".sdk -mmacosx-version-min="base"."n)))
(define mac32-sdk 6)
(define mac64-sdk (if (equal? package-name "poppler") 15 9))
(define macaarch64-sdk (sdk 0 #:base 11))

(define using-clang-mingw? (and win? aarch64?))

(define all-env
  (cond
   [win?
    (case package-name
      [("pkg-config" "sed" "bison")
       ;; runs on build platform:
       null]
      [("openssl-1" "openssl-3")
       ;; Not libtool, and prefix added automatically
       (list
        (list "CC" "gcc -static-libgcc"))]
      [else
       (list
        ;; We'd prefer to add "-static-libgcc" to CFLAGS, but
        ;; libtool doesn't pass `static-libgcc` through.
        (list "CC" (~a win-prefix "-gcc" (if using-clang-mingw?
                                             ""
                                             " -static-libgcc"))))])]
   [mac?
    (case package-name
      [("pkg-config" "sed" "bison")
       ;; runs on build platform:
       null]
      [else
       (cond
         [aarch64?
          (define flags (~a "-arch arm64 " macaarch64-sdk))
          (list
           (list "CPPFLAGS" (~a flags))
           (list "LDFLAGS" (~a flags)))]
         [i386?
          (define sdk-flags (sdk mac32-sdk))
          (list
           (list "CPPFLAGS" (~a "-arch i386" sdk-flags))
           (list "CXXFLAGS" (~a "-arch i386" sdk-flags))
           (list "LDFLAGS" (~a "-arch i386" sdk-flags
                               ;; suppress deprecation warning:
                               " -Wl,-w")))]
         [x86_64?
          (define sdk-flags (sdk mac64-sdk))
          (list
           (list "CPPFLAGS" (~a "-arch x86_64" sdk-flags))
           (list "CXXFLAGS" (~a "-arch x86_64" sdk-flags))
           (list "LDFLAGS" (~a "-arch x86_64" sdk-flags)))]
         [else (error "flags arch")])])]
   [else
    (cond
     [i386?
      (list
       (list "CPPFLAGS" "-m32")
       (list "LDFLAGS" "-m32"))]
     [else
      null])]))

(define cxx-env
   (if win?
       (list
       (list "CXX" (~a win-prefix "-g++"
                       (if using-clang-mingw?
                           ""
                           " -static-libgcc -static-libstdc++"))))
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
        pkg-config = 'pkg-config'
        windres = '@|cpu|-w64-mingw32-windres'})
  (call-with-output-file*
   "cross_file.txt"
   #:exists 'truncate
   (lambda (out)
     (displayln content out))))

(define (make-mac-cross_file.txt cpu)
  (define flags (string-join (string-split
                              (sdk (case cpu
                                     [("i386") mac32-sdk]
                                     [("x86_64") mac64-sdk]
                                     [("arm64") macaarch64-sdk])))
                             "', '"))
  (define content
    @~a{[host_machine]
        system = 'darwin'
        subsystem = 'macos'
        kernel = 'xnu'
        cpu_family = '@(if (equal? cpu "i386") "x86" cpu)'
        cpu = '@|cpu|'
        endian = 'little'

        [built-in options]
        ;; SOL_LOCAL and LOCAL_PEERPID hacks are for glib < macOS 10.13
        c_args = ['-arch', '@|cpu|', '-I@|dest|/include', '@|flags|', '-DSOL_LOCAL=0', '-DLOCAL_PEERPID=2']
        c_link_args = ['-arch', '@|cpu|', '-L@|dest|/lib', '@|flags|']
        objc_args = ['-arch', '@|cpu|', '-I@|dest|/include', '@|flags|', '-DSOL_LOCAL=0', '-DLOCAL_PEERPID=2']
        objc_link_args = ['-arch', '@|cpu|', '-L@|dest|/lib', '@|flags|']
        
        [properties]
        needs_exe_wrapper = false

        [binaries]
        c = 'clang'
        cpp = 'clang++'
        objc = 'clang'
        objcpp = 'clang++'
        ar = 'ar'
        strip = 'strip'
        pkg-config = 'pkg-config'})
  (call-with-output-file*
   "cross_file.txt"
   #:exists 'truncate
   (lambda (out)
     (displayln content out))))

#;
(define (make-mac-cross_file.txt cpu)
  (define content
    @~a{[host_machine]
        system = 'darwin'
        cpu_family = '@|cpu|'
        cpu = '@|cpu|'
        endian = 'little'

        [binaries]
        c = ['gcc', '-arch', '@|cpu|']
        cpp = ['g++', '-arch', '@|cpu|']
        objc = ['gcc', '-arch', '@|cpu|']
        ar = 'ar'
        as = 'fail_now'
        ld = ['ld', '-arch', '@|cpu|']
        objcopy = ['objcopy', '-arch', '@|cpu|']
        strip = ['strip', '-arch', '@|cpu|']
        pkg-config = 'pkg-config'})
  (call-with-output-file*
   "cross_file.txt"
   #:exists 'truncate
   (lambda (out)
     (displayln content out))))

(define (make-mac-toolchain.txt cpu)
  (define content
    @~a{set(CMAKE_SYSTEM_NAME macos)
        set(CMAKE_C_COMPILER gcc -arch @|cpu|)
        set(CMAKE_CXX_COMPILER g++ -arch @|cpu|)})
  (call-with-output-file*
   "toolchain.txt"
   #:exists 'truncate
   (lambda (out)
     (displayln content out))))



(define (make-all-args use-cross-file)
  (append
   (case package-name
     [("poppler")
      ;; cmake
      (list (~a "-DCMAKE_PREFIX_PATH=" dest)
            (~a "-DCMAKE_INSTALL_PREFIX=" dest))]
     [else
      (list (~a "--prefix=" dest)
            ;; override use of system name in lib path:
            (~a "--libdir=" (build-path dest "lib")))])
   (cond
    [win?
     (case package-name
       [("pkg-config" "sed" "bison")
        ;; runs on build platform
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
          [i386?
           (list "--host=i686-w64-mingw32")]
          [aarch64?
           (list "--host=aarch64-w64-mingw32")]
          [x86_64?
           (list "--host=x86_64-w64-mingw32")]
          [else (error "missing host arch")])])]
    [mac?
     (case package-name
       [("pkg-config" "sed" "bison")
        ;; runs on build platform
        null]
       [("openssl-1" "openssl-3")
        ;; not the usual "configure"
        null]
       [("poppler")
        ;; cmake
        (cond
          [x86_64?
           (list "-DCMAKE_OSX_ARCHITECTURES=x86_64"
                 (~a "-DCMAKE_OSX_DEPLOYMENT_TARGET=10." mac64-sdk))]
          [else
           null])]
       [else
        (cond
          [use-cross-file
           (list "--cross-file" "cross_file.txt")]
          [x86_64?
           (list "--host=x86_64-apple-darwin")]
          [i386?
           (list "--host=i386-apple-darwin")]
          [else null])])]
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
                #:use-cross-file [use-cross-file #f]
                #:build-directory [build-dir #f])
  (for ([d (in-list (append (if (or (equal? package-name "pkg-config")
                                    (equal? package-name "sed")
                                    (equal? package-name "bison"))
                                '()
                                (append
                                 '("pkg-config")
                                 (if need-sed? '("sed") '())))
                            deps))])
    (unless (file-exists? (build-path dest "stamps" d))
      (error 'build "prerequisite needed: ~a" d)))
  (values env exe args make make-install setup patches post-patches install-patches fixup fixup-proc
          use-cross-file build-dir))

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

(define (meson-exe)
  (cond
    [linux?
     (list (find-executable-path "python3.14")
           (let ([dir (find-package "meson" #t #t)])
             (unless dir
               (let ([tgz (for/or ([archives-dir (in-list archives-dirs)])
                            (parameterize ([current-directory archives-dir])
                              (define tgz (find-package "meson" #f #t))
                              (and tgz (build-path archives-dir tgz))))])
                 (unless tgz
                   (error "need meson package"))
                 (system/show (~a "tar zxf " tgz))))
             (path->complete-path (build-path (find-package "meson" #t #f) "meson.py"))))]
    [else
     (find-executable-path "meson")]))

(define (meson-make)
  (define exe (meson-exe))
  (append (if (list? exe) exe (list exe))
          (list "compile" "-C" "_build")))

(define (meson-install)
  (define exe (meson-exe))
  (append (if (list? exe) exe (list exe))
          (list "install" "-C" "_build")))

(define (meson-configure . args)
  (append '("setup")
          '("--buildtype" "release")
          (apply append args)
          '(#f "_build")))

(define (meson-cross-file)
  (cond
    [win?
     (cond
       [aarch64? "aarch64"]
       [i386? "i686"]
       [else "x86_64"])]
    [mac?
     (cond
       [aarch64? #f]
       [i386? "i386"]
       [else "x86_64"])]
    [else #f]))

(define-values (extra-env configure-exe extra-args make-command make-install-command 
                          setup patches post-patches install-patches fixup fixup-proc
                          use-cross-file build-dir)
  (case package-name
    [("pkg-config") (config #:configure (if #t
                                            (list) ; using local install, Homebrew, etc.
                                            (list "--with-internal-glib")))]
    [("sed") (config)]
    [("longdouble") (config)]
    [("libedit") (config
                  #:patches (if (and mac? (or i386? ppc?))
                                (list libedit-getline-patch)
                                null))]
    [("libiconv")
     (nonmac-only)
     (config #:configure '("--enable-extra-encodings"))]
    [("sqlite")
     (nonmac-only)
     (config #:fixup (and win?
                          (~a "cd " (build-path dest "bin")
                              " && mv libsqlite3-0.dll sqlite3.dll"))
             #:env (if linux?
                       (list (list "LDFLAGS" (~a "-Wl,-rpath," dest "/lib")))
                       null))]
    [("openssl-1" "openssl-3")
     (define make
       (if linux?
           (~a "make SHARED_LDFLAGS=" "-Wl,-rpath," dest "/lib")
           "make"))
     (define vers (if (equal? package-name "openssl-1") #"1_1" #"3"))
     (config #:configure-exe (find-executable-path "perl")
             #:configure (cond
                          [win?
                           (append
                            (list "./Configure"
                                  (~a "--cross-compile-prefix=" win-prefix "-")
                                  #f ; other flags here
                                  (~a "mingw" (if i386? "" (if aarch64? "-arm64" "64")))
                                  "shared")
                            (if aarch64?
				'("no-asm")
				null))]
                          [mac?
			   (append
                            (list "./Configure"
                                  #f ; other flags here
                                  "shared"
                                  (cond
                                   [ppc? "darwin-ppc-cc"]
                                   [i386? "darwin-i386-cc"]
                                   [aarch64? "darwin64-arm64-cc"]
                                   [else "darwin64-x86_64-cc"])
                                  (car (regexp-match #rx"-mmacosx-version-min=[0-9.]*"
                                                     (cadr (assoc "CPPFLAGS" all-env)))))
                            (if i386?
                                '("-DBROKEN_CLANG_ATOMICS")
                                null)
			    (if aarch64?
				'("no-asm")
				null))]
                          [else
                           (list "./Configure"
                                 #f
                                 "shared"
                                 (if aarch64?
                                     "linux-aarch64"
                                     "linux-x86_64"))])
             #:patches (if (and win? aarch64?)
                           (list openssl-aarch64nt-patch)
                           (list))
             #:post-patches (if (and win? aarch64?)
                                (list openssl-no-rcflags-patch)
                                null)
	     #:make make
             #:make-install (~a make " install_sw")
             #:fixup (and win?
                          (~a "cd " (build-path dest "bin")
                              " && mv libssl-" vers (if (or i386? aarch64?) "" "-x64") ".dll ssleay32.dll"
                              " && mv libcrypto-" vers (if (or i386? aarch64?) "" "-x64") ".dll libeay32.dll"))
             #:fixup-proc (and win?
                               (lambda ()
                                 (replace-in-file (build-path dest "bin" "ssleay32.dll")
                                                  (bytes-append #"libcrypto-" vers (if (or i386? aarch64?) #"" #"-x64") #".dll\0")
                                                  #"libeay32.dll\0"))))]
    [("expat") (config)]
    [("gettext") (config #:depends (if win? '("libiconv") '())
                         #:build-directory "gettext-runtime"
                         #:configure (append
                                      '("--enable-languages=c")
                                      (if win?
                                          '("--enable-threads=windows")
                                          null))
                         ;; avoid installing `msgfmt`, which might be used in cross-build
                         #:fixup (and (not linux?)
                                      (~a "rm -f " (build-path dest "bin/msgfmt"))))]
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
     (config #:env path-flags
             #:setup (if aarch64?
                         (list
                          (~a "cp " config.guess " config.guess"))
                         null))]
    [("gdk-pixbuf")
     (linux-only)
     (config #:depends '("libX11")
	     #:configure '("--without-libtiff")
             #:patches (list gdk-pixbuf-no-sniff-patch)
	     #:env (append path-flags
			   ld-library-path-flags))]
    [("atk")
     (config #:depends (if linux?
                           '("libX11")
                           '())
	     #:env (append path-flags
			   ld-library-path-flags
                           (if linux?
                               (list (list "LDFLAGS" (~a "-Wl,-rpath," dest "/lib")))
                               null)))]
    [("gtk+")
     (linux-only)
     (config #:depends '("gdk-pixbuf" "atk" "libXrender")
             #:patches (list gtk-with-newer-gdk-patch
                             gtk-no-demos-patch)
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
       [(and mac? i386?)
        (config #:configure '("-host=i386-apple-darwin"))]
       [(and win? aarch64?)
        (config #:env (list (list "CPPFLAGS" "-D_M_ARM64"))
                #:configure '("--disable-symvers"))]
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
                      #:configure-exe (meson-exe)
                      #:use-cross-file (meson-cross-file)
                      #:make (meson-make)
                      #:make-install (meson-install)
                      #:configure (meson-configure
                                   ;; '("-Dinternal_pcre=true")
                                   (if linux? '("-Dlibmount=disabled") '()))
                      #:env (add-flag (add-flag path-flags
                                                ;; Disable Valgrind support, which particularly
                                                ;; goes wrong for 64-bit Windows builds.
                                                "CPPFLAGS" (string-append
                                                            "-DNVALGRIND=1"
                                                            (if (and mac? (or i386? ppc?))
                                                                (string-append
                                                                 ;; assertion uses unavailable strnlen
                                                                 " -DG_DISABLE_ASSERT"
                                                                 ;; avoid `SOL_LOCAL`
                                                                 " -DSO_PEERCRED")
                                                                "")))
                                      "LDFLAGS" (cond
                                                  [(and win? (not aarch64?))
                                                   "-Wl,--allow-multiple-definition"]
                                                  [linux?
                                                   (~a "-Wl,-rpath," dest "/lib")]
                                                  [else ""]))
                      #:patches (append
                                 (list glib-disable-gio-test-patch)
                                 (cond
                                   [else null])))]
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
                                              (if (or win? mac?) '() '("libuuid")))
                            #:configure (append '("--disable-docs")
                                                (if win?
                                                    `("--without-libiconv-prefix"
                                                      "--without-libintl-prefix")
                                                    '()))
                            #:patches (list fontconfig-dirs-patch)
			    #:install-patches (cond
					       [(and mac? aarch64?) (list fc-config-patch)]
					       [else null]))]
    [("pixman") (config #:configure-exe (meson-exe)
                        #:use-cross-file (meson-cross-file)
                        #:make (meson-make)
                        #:make-install (meson-install)
                        #:configure (meson-configure
                                     '("-Dtests=disabled"))
                        #:patches (cond
                                    [win? (list pixman-nopthread-patch)]
                                    [else (list)]))]
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
             #:configure-exe (meson-exe)
             #:use-cross-file (meson-cross-file)
             #:make (meson-make)
             #:make-install (meson-install)
             #:configure (meson-configure
                          '("-Dfontconfig=enabled" "-Dfreetype=enabled" "-Dtests=disabled"))
             #:env (if win?
                       (add-flag path-flags
                                 "LDFLAGS"
                                 "-static-libgcc -static-libstdc++ -Wl,-static -Wl,--whole-archive -lwinpthread -Wl,-shared -Wl,--no-whole-archive")
                       (if linux?
                           (add-flag path-flags
                                     "LDFLAGS"
                                     (~a "-Wl,-rpath," dest "/lib"))
                           path-flags))
             #:patches (append
                        (list courier-new-patch
                              cairo-cg-surface-patch
                              cairo-quartz-advance-patch
                              cairo-empty-font-subset-patch
                              cairo-emptyglyph-patch)
                        (if win?
                            (list cairo-win-pthread-patch)
                            null)))]
    [("harfbuzz") (config #:depends '("fontconfig" "freetype" "cairo")
                          #:configure-exe (meson-exe)
                          #:use-cross-file (meson-cross-file)
                          #:configure (meson-configure
                                       '("-Dfreetype=enabled"
                                         "-Dcoretext=enabled"
                                         "-Dtests=disabled"))
                          #:make (meson-make)
                          #:make-install (meson-install)
                          #:env cxx-env)]
    [("fribidi") (config #:configure '("--disable-docs"))]
    [("pango") (config #:depends '("cairo" "harfbuzz" "fribidi")
                       #:env (cond
                               [win? path-flags]
                               [else '()])
                       #:configure-exe (meson-exe)
                       #:use-cross-file (meson-cross-file)
                       #:make (meson-make)
                       #:make-install (meson-install)
                       #:configure (meson-configure
                                    '("-Dfontconfig=enabled"))
                       #:patches (append
                                  (list coretext-fontreg-patch)))]
    [("gmp") (config #:patches (cond
                                 [gcc-4.0?
                                  (list gmp-weak-patch)]
                                 [else null])
                     #:configure (append
                                  '("--enable-shared" "--disable-static")
                                  (if (and linux? (not (or i386? aarch64?)))
                                      '("--host=core2-linux-gnu") ; core2 for portability
                                      null)
                                  (if (and win? aarch64?)
                                      '("--disable-assembly")
                                      '())
                                  (if (and mac? (or i386? x86_64?))
                                      '("--build=corei-apple-darwin")
                                      null)
                                  (if (and i386? mac?)
                                      (list "ABI=32")
                                      null))
                     #:post-patches (if (and mac? ppc?)
                                        (list gmp-inline-patch)
                                        null))]
    [("mpfr" "mpfr-3" "mpfr-4")
     (config #:configure (append (if (and #f win?) ; creates dependency on "libwinpthread-1.dll"
                                     '("--enable-thread-safe")
                                     null)
                                 '("--enable-shared" "--disable-static"))
             #:depends '("gmp")
             #:env path-flags)]
    [("jpeg") (config)]
    [("poppler") (config #:env (append path-flags
                                       cxx-env)
                         #:configure-exe (find-executable-path "cmake")
                         #:configure (list "-S" "." "-B" "build"
                                           "-DENABLE_QT5=OFF" "-DENABLE_QT6=OFF"
                                           "-DENABLE_NSS3=OFF" "-DENABLE_BOOST=OFF"
                                           "-DENABLE_GPGME=OFF" "-DENABLE_LIBTIFF=OFF"
                                           "-DENABLE_LIBOPENJPEG=OFF" "-DENABLE_LCMS=OFF")
                         #:make (list (find-executable-path "cmake") "--build" "build")
                         #:make-install (list (find-executable-path "cmake") "--install" "build"))]
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

(when (and linux? aarch64?)
  (unless (link-exists? (build-path dest "lib" "aarch64-linux-gnu"))
    (make-directory* (build-path dest "lib"))
    (make-file-or-directory-link "." (build-path dest "lib" "aarch64-linux-gnu"))))

(when build-dir
  (make-directory* (build-path package-dir build-dir)))

(parameterize ([current-directory (if build-dir
                                      (build-path package-dir build-dir)
                                      package-dir)]
               [current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))])
  (putenv "PATH" (~a dest "/bin"
                     ":"
                     (if win?
                         (if i386?
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
    (cond
      [win?
       (make-windows-cross_file.txt use-cross-file)]
      [mac?
       (make-mac-cross_file.txt use-cross-file)
       (make-mac-toolchain.txt use-cross-file)]))
  (unless skip-config?
    (apply system*/show
           (if configure-exe
               (if (pair? configure-exe)
                   (car configure-exe)
                   configure-exe)
               "./configure")
           (append
            (if (pair? configure-exe)
                (cdr configure-exe)
                null)
            (let loop ([extra-args extra-args])
              (cond
                [(null? extra-args) (make-all-args use-cross-file)]
                [(not (car extra-args)) (append (make-all-args use-cross-file) (cdr extra-args))]
                [else (cons (car extra-args) (loop (cdr extra-args)))]))))
    (for ([p (in-list post-patches)])
      (system/show (~a "patch -p2 < " p))))
  (remove-libtool-flat-namespace)
  (if (list? make-command)
      (apply system*/show make-command)
      (system/show make-command))
  (for ([p (in-list install-patches)])
    (system/show (~a "patch -p2 < " p)))
  (if (list? make-install-command)
      (apply system*/show make-install-command)
      (system/show make-install-command))
  (when fixup
    (system/show fixup))
  (when fixup-proc
    (fixup-proc))
  (stamp package-name)
  (displayln "Success!"))

