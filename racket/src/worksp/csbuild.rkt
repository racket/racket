#lang racket/base
(require racket/cmdline
	 racket/path
	 racket/file
         racket/runtime-path
	 compiler/find-exe
	 racket/system
	 "cs/prep.rkt"
	 "cs/recompile.rkt")

;; Environment variables that affect the build:
;;  PLT_BOOTFILE_NO_COMPRESS - disables compression of boot files
;;  PLT_CS_MAKE_COMPRESSED_DATA - enables more ".zo" compression
;;  PLT_CS_MAKE_NO_COMPRESSED - disables default ".zo" compression

(define-runtime-path here ".")

(define scheme-dir (build-path 'up "ChezScheme"))
(define pull? #f)
(define static-libs? #t)
(define install-boot? #t) ; currently always enabled
(define machine (if (= 32 (system-type 'word))
		    "ti3nt"
		    "ta6nt"))
(define cs-suffix "")
(define boot-mode "--chain")
(define extra-repos-base #f)

(command-line
 #:once-each
 [("--pull") "Use `git pull` on auto-cloned Chez Scheme repo"
             (set! pull? #t)]
 [("--racketcs-suffix") str "Select the suffix for RacketCS"
                        (set! cs-suffix (string-upcase str))]
 [("--boot-mode") mode "Select the mode for Racket bootstrapping"
                  (set! boot-mode mode)]
 [("--machine") mach "Select the Chez Scheme machine name"
                (set! machine mach)]
 [("--extra-repos-base") url "Clone repos from <url>ChezScheme/.git, etc."
                         (unless (equal? url "")
                           (set! extra-repos-base url))]
 [("--disable-libs") "Disable installaton of static libraries (currently ignored)"
                     (set! static-libs? #f)]
 #:args
 ()
 (void))

(current-directory here)

;; filters #f from arguments
(define (system*! prog . all-args)
  (define args (for/list ([arg (in-list all-args)]
                          #:when arg)
                 arg))
  (printf "{in ~a}\n" (current-directory))
  (printf "~a" prog)
  (for ([arg (in-list args)])
    (printf " [~a]" arg))
  (newline)
  (unless (apply system*
		 (if (string? prog)
		     (find-executable-path (path-add-extension prog #".exe"))
		     prog)
		 args)
    (error 'csbuild "command failed")))

(define (system! cmd)
  (printf "{in ~a}\n" (current-directory))
  (printf "~a\n" cmd)
  (unless (system cmd)
    (error 'csbuild "command failed")))

;; ----------------------------------------

;; Bootstrap Chez Scheme boot files
(let/ec esc
  (parameterize ([current-environment-variables
		  (environment-variables-copy (current-environment-variables))]
		 [exit-handler (let ([orig-exit (exit-handler)])
				 (lambda (v)
				   (if (zero? v)
				       (esc)
				       (orig-exit v))))])
    (putenv "SCHEME_SRC" (path->string scheme-dir))
    (putenv "MACH" machine)
    (dynamic-require (build-path scheme-dir "rktboot" "make-boot.rkt") #f)))

;; Prepare to use Chez Scheme makefile
(prep-chez-scheme scheme-dir machine)

;; Finish building Chez Scheme
(parameterize ([current-directory (build-path scheme-dir machine "c")])
  (system*! "nmake"
	    (format "Makefile.~a" machine)))

;; Replace Chez-on-Racket-built bootfiles with Chez-built bootfiles
(recompile scheme-dir machine #:system* system*!)

;; ----------------------------------------

;; Run Racket in directories that reach here with "../worksp".
;; By using a relative path, we avoid problems with spaces in path names.
(define rel-racket (build-path 'up "worksp" (find-relative-path (current-directory) (find-exe))))

(define chain-racket
  (format "~a -O info@compiler/cm -l- setup ~a ../setup-go.rkt ../build/compiled"
	  rel-racket
          boot-mode))

(define build-dir (path->directory-path (build-path 'up "build")))

;; ----------------------------------------

;; We could regenerate schemified layers in development mode
;; (which on Unix is done by `make` in `racket/src/cs`).

(when #f
  (define (build-layer name
                       #:dir [dir name]
                       #:skip-make? [skip-make? #f])
    (parameterize ([current-directory (build-path 'up dir)])
      (make-directory* (build-path build-dir "compiled"))
      (unless skip-make?
        (system*! "nmake"
                  (format "~a-src-generate" name)
                  (format "BUILDDIR=~a" build-dir)
                  (format "RACKET=~a ~a ~a" chain-racket "ignored" (build-path build-dir "compiled/ignored.d"))))))

  (build-layer "expander")
  (build-layer "thread")
  (build-layer "io")
  (build-layer "regexp")

  (build-layer "schemify")
  (build-layer "known" #:dir "schemify"))

;; ----------------------------------------

(define scheme (build-path scheme-dir machine "bin" machine "scheme.exe"))
(define scheme-boot (build-path scheme-dir machine "boot" machine))
(define (path->relative p)
  (if (relative-path? p)
      p
      (find-relative-path (current-directory) p)))
(define rel-scheme (build-path 'up "worksp" (path->relative scheme)))
(define rel-scheme-boot (build-path 'up "worksp" (path->relative scheme-boot)))

(parameterize ([current-directory (build-path 'up "cs")])
  (define convert.d (build-path build-dir "compiled" "convert.d"))
  (unless (file-exists? convert.d) (call-with-output-file convert.d void))
  (unless (getenv "PLT_CS_MAKE_NO_COMPRESSED")
    (putenv "PLT_CS_MAKE_COMPRESSED" "yes"))
  (system*! "nmake"
	    (build-path "../build/racket.so") ; need forward slashes
	    (format "RACKET=~a" rel-racket)
	    (format "SCHEME=~a" rel-scheme)
	    (format "BUILDDIR=../build/") ; need forward slashes
	    (format "CONVERT_RACKET=~a" chain-racket)
            (format "BOOTSTRAPPED=~a" "done")
            (format "EXTRA_COMPILE_DEPS=~a/petite.boot ~a/scheme.boot" rel-scheme-boot rel-scheme-boot)))

;; ----------------------------------------

(system! "rktio.bat")

;; ----------------------------------------

;; The library name changes with the version, so extract it from the
;; Chez Scheme makefile
(define scheme-lib
  (call-with-input-file*
   (build-path scheme-dir "c" "Makefile.nt")
   (lambda (i)
     (for/or ([l (in-lines i)])
       (define m (regexp-match #rx"MTKernelLib *= *.*(csv.*mt.lib)" l))
       (and m
            (cadr m))))))

(define rel2-scheme-dir (build-path 'up
				    (if (relative-path? scheme-dir)
					scheme-dir
					(find-relative-path (current-directory) scheme-dir))))

(parameterize ([current-directory "cs"])
  (system*! "nmake"
	    "all"
	    (format "SCHEME_DIR=~a" rel2-scheme-dir)
	    (format "MACHINE=~a" machine)
	    (format "SCHEME_LIB=~a" scheme-lib)
	    (format "COMP_SUBDIR=/DCS_COMPILED_SUBDIR=~a"
                    (if (string=? cs-suffix "")
                        "0"
                        "1"))))

;; ----------------------------------------

(define compress-flag
  (and (not (getenv "PLT_BOOTFILE_NO_COMPRESS"))
       "--compress"))

(system*! scheme
	  "--script"
	  "../cs/c/convert-to-boot.ss"
	  "../build/racket.so"
	  "../build/racket.boot"
	  machine)

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
          compress-flag
	  (build-path scheme-dir machine "boot" machine "petite.boot")
	  "../build/petite-v.boot")

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
          compress-flag
	  (build-path scheme-dir machine "boot" machine "scheme.boot")
	  "../build/scheme-v.boot"
          "petite")

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
          compress-flag
	  "../build/racket.boot"
	  "../build/racket-v.boot"
          "petite"
          "scheme")

(define (bootstrap-racket! . args)
  (apply system*! (find-exe)
         "-O" "info@compiler/cm"
         "-l-" "setup" boot-mode "../setup-go.rkt" "../build/compiled"
         "ignored" "../build/ignored.d"
         args))

(make-directory* "../../lib")
(bootstrap-racket! "../cs/c/embed-boot.rkt"
                   compress-flag
                   "++exe" "../build/raw_racketcs.exe" (format "../../Racket~a.exe" cs-suffix)
                   "++exe" "../build/raw_gracketcs.exe" (format "../../lib/GRacket~a.exe" cs-suffix)
                   "++rewrite" "libracketcsxxx_raw.dll" "libracketcsxxxxxxx.dll"
                   "../build/libracketcsxxx_raw.dll" "../../lib/libracketcsxxxxxxx.dll"
                   "../build/petite-v.boot"
                   "../build/scheme-v.boot"
                   "../build/racket-v.boot")

(system*! "mt"
	  "-manifest" "racket/racket.manifest"
	  (format "-outputresource:../../Racket~a.exe;1" cs-suffix))
(system*! "mt"
	  "-manifest" "gracket/gracket.manifest"
	  (format "-outputresource:../../lib/GRacket~a.exe;1" cs-suffix))

;; ----------------------------------------
;; Finish installation with "mzstart", "mrstart", and other
;; implementation-independent details as in "build.bat"

(define (get-status src)
  (system*! "cl" src)
  (system* (path-replace-suffix src #".exe")))

(define buildmode (if (get-status "rbuildmode.c")
                      "x64"
                      "win32"))
(define pltslnver (cond
                    [(not (get-status "checkvs9.c")) "9"]
                    [(not (get-status "genvsx.c")) "X"]
                    [else ""]))

(make-directory* "../../etc")
(make-directory* "../../doc")
(make-directory* "../../share")
(make-directory* "../../include")

(copy-file "../LICENSE-libscheme.txt"
           "../../share/LICENSE-libscheme.txt"
           #t)
(copy-file "../LICENSE-MIT.txt"
           "../../share/LICENSE-MIT.txt"
           #t)
(copy-file "../LICENSE-APACHE.txt"
           "../../share/LICENSE-APACHE.txt"
           #t)
(copy-file "../LICENSE-LGPL.txt"
           "../../share/LICENSE-LGPL.txt"
           #t)
(copy-file "../LICENSE-GPL.txt"
           "../../share/LICENSE-GPL.txt"
           #t)

(copy-file "../cs/c/api.h"
           "../../include/racketcs.h"
           #t)
(copy-file "../cs/c/boot.h"
           "../../include/racketcsboot.h"
           #t)
(copy-file (build-path scheme-dir machine "boot" machine "scheme.h")
           "../../include/chezscheme.h"
           #t)

(parameterize ([current-directory "mzstart"])
  (system*! "msbuild"
            (format "mzstart~a.sln" pltslnver)
            "/p:Configuration=Release"
            (format "/p:Platform=~a" buildmode)))

(parameterize ([current-directory "mrstart"])
  (system*! "msbuild"
            (format "mrstart~a.sln" pltslnver)
            "/p:Configuration=Release"
            (format "/p:Platform=~a" buildmode)))

(system*! (find-exe)
          "../cs/c/gen-system.rkt"
          (format "../../lib/system~a.rktd" cs-suffix)
          machine
          machine
          "machine"
          "../cs/c"
          "")

(when install-boot?
  (bootstrap-racket! "../cs/c/add-terminator.rkt"
                     "../build/petite-v.boot"
                     "../../lib/petite.boot")

  (bootstrap-racket! "../cs/c/add-terminator.rkt"
                     "../build/scheme-v.boot"
                     "../../lib/scheme.boot")

  (bootstrap-racket! "../cs/c/add-terminator.rkt"
                     "../build/racket-v.boot"
                     "../../lib/racket.boot"))
