#lang racket/base
(require racket/cmdline
	 racket/path
	 racket/file
         racket/runtime-path
	 compiler/find-exe
	 racket/system
	 "cs/prep.rkt")

(define-runtime-path here ".")

(define scheme-dir-provided? #f)
(define abs-scheme-dir (build-path here 'up "build" "ChezScheme"))
(define pull? #f)
(define machine (if (= 32 (system-type 'word))
		    "ti3nt"
		    "ta6nt"))
(define cs-suffix "CS")
(define boot-mode "--chain")
(define extra-repos-base #f)
(define git-clone-args '())

(command-line
 #:once-each
 [("--scheme-dir") dir "Select the Chez Scheme build directory, unless <dir> is \"\""
                   (unless (equal? dir "")
                     (set! scheme-dir-provided? #t)
                     (set! abs-scheme-dir (path->complete-path dir)))]
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
 #:args
 clone-arg
 (set! git-clone-args clone-arg))

(current-directory here)

(define scheme-dir (find-relative-path (current-directory)
                                       (simplify-path abs-scheme-dir)))

(define (system*! prog . args)
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

(let ([submodules '("nanopass"  "stex"   "zlib"   "lz4")]
      [readmes    '("ReadMe.md" "ReadMe" "README" "README.md")])  
  (define (clone from to [git-clone-args '()])
    (apply system*! (append
                     (list "git"
                           "clone")
                     git-clone-args
                     (list from to))))
  (cond
    [extra-repos-base
     ;; Intentionally not using `git-clone-args`, because dumb transport
     ;; (likely for `extra-repos-base`) does not support shallow copies
     (unless (directory-exists? scheme-dir)
       (clone (format "~aChezScheme/.git" extra-repos-base)
              scheme-dir))
     (for ([submodule (in-list submodules)]
           [readme (in-list readmes)])
       (define dir (build-path scheme-dir submodule))
       (unless (file-exists? (build-path dir readme))
         (clone (format "~a~a/.git" extra-repos-base submodule)
                (build-path scheme-dir submodule))))
     (when pull?
       (parameterize ([current-directory scheme-dir])
         (system*! "git" "pull")
         (for ([submodule (in-list submodules)])
           (parameterize ([current-directory (build-path submodule)])
             (system*! "git" "pull" "origin" "master")))))]
    [else
     (unless (directory-exists? scheme-dir)
       (clone "https://github.com/mflatt/ChezScheme"
              scheme-dir
              git-clone-args))
     (when pull?
       (parameterize ([current-directory scheme-dir])
         (system*! "git" "pull")
         (system*! "git" "submodule" "init")
         (system*! "git" "submodule" "update")))]))

(prep-chez-scheme scheme-dir machine)

(parameterize ([current-directory (build-path scheme-dir machine "c")])
  (system*! "nmake"
	    (format "Makefile.~a" machine)))

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
(build-layer "known" #:dir "schemify")

;; ----------------------------------------

(define scheme (build-path scheme-dir machine "bin" machine "scheme.exe"))
(define rel-scheme (build-path 'up "worksp"
			       (if (relative-path? scheme)
				   scheme
				   (find-relative-path (current-directory) scheme))))

;; Environment variable used by ".sls" files to find ".scm" files
(putenv "COMPILED_SCM_DIR" "../build/compiled/")

(parameterize ([current-directory (build-path 'up "cs")])
  (define convert.d (build-path build-dir "compiled" "convert.d"))
  (unless (file-exists? convert.d) (call-with-output-file convert.d void))
  (system*! "nmake"
	    (build-path "../build/racket.so") ; need forward slashes
	    (format "RACKET=~a" rel-racket)
	    (format "SCHEME=~a" rel-scheme)
	    (format "BUILDDIR=../build/") ; need forward slashes
	    (format "CONVERT_RACKET=~a" chain-racket)))

;; ----------------------------------------

(system! "rktio.bat")

;; ----------------------------------------

;; The library name changes with the version:
(define scheme-lib
  (parameterize ([current-directory (build-path scheme-dir machine "boot" machine)])
    (for/or ([f (in-list (directory-list))]
	     #:when (regexp-match? #rx"^csv.*mt.lib$" f))
      f)))

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

(system*! scheme
	  "--script"
	  "../cs/c/convert-to-boot.ss"
	  "../build/racket.so"
	  "../build/racket.boot"
	  machine)

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
	  (build-path scheme-dir machine "boot" machine "petite.boot")
	  "../build/petite-v.boot")

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
	  (build-path scheme-dir machine "boot" machine "scheme.boot")
	  "../build/scheme-v.boot"
          "petite")

(system*! scheme
	  "--script"
	  "../cs/c/to-vfasl.ss"
	  "../build/racket.boot"
	  "../build/racket-v.boot"
          "petite"
          "scheme")

(system*! (find-exe)
          "-O" "info@compiler/cm"
          "-l-" "setup" boot-mode "../setup-go.rkt" "..//build/compiled"
          "ignored" "../build/ignored.d"
          "../cs/c/embed-boot.rkt"
	  "++exe" "../build/raw_racketcs.exe" (format "../../Racket~a.exe" cs-suffix)
	  "++exe" "../build/raw_gracketcs.exe" (format "../../lib/GRacket~a.exe" cs-suffix)
          "../build/raw_libracketcs.dll" "../../lib/libracketcsxxxxxxx.dll"
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

(copy-file "../COPYING-libscheme.txt"
           "../../share/COPYING-libscheme.txt"
           #t)
(copy-file "../COPYING_LESSER.txt"
           "../../share/COPYING_LESSER.txt"
           #t)
(copy-file "../COPYING.txt"
           "../../share/COPYING.txt"
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
          "machine")
