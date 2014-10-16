#lang racket/base

(use-compiled-file-paths null)

(require racket/system)

(define (system- s)
  (eprintf "~a\n" s)
  (system s))

(define backtrace-gc? #f)
(define opt-flags "/O2 /Oy-")
(define re:only #f)

(define win64?
  (equal? "win32\\x86_64" (path->string (system-library-subpath #f))))

(define cl.exe
  (let ([p (find-executable-path "cl.exe" #f)])
    (unless p
      (error (string-append "Cannot find executable \"cl.exe\".\n"
                            "You may need to find and run \"vsvars32.bat\".")))
    "cl.exe"))

(unless (directory-exists? "xsrc")
  (make-directory "xsrc"))

(define srcs
  '("salloc"
    "bignum"
    "bool"
    "builtin"
    "char"
    "compenv"
    "compile"
    "complex"
    "dynext"
    "env"
    "error"
    "eval"
    "file"
    "future"
    "fun"
    "hash"
    "jit"
    "jitalloc"
    "jitarith"
    "jitcall"
    "jitcommon"
    "jitinline"
    "jitprep"
    "jitstack"
    "jitstate"
    "letrec_check"
    "list"
    "marshal"
    "module"
    "mzrt"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
    "optimize"
    "place"
    "port"
    "portfun"
    "print"
    "rational"
    "read"
    "regexp"
    "resolve"
    "sema"
    "setjmpup"
    "sfs"
    "string"
    "struct"
    "symbol"
    "syntax"
    "thread"
    "type"
    "validate"
    "vector"))

(define common-cpp-defs " /D _CRT_SECURE_NO_DEPRECATE /D _USE_DECLSPECS_FOR_SAL=0 /D _USE_ATTRIBUTES_FOR_SAL=0 /nologo ")

(define (check-timestamp t2 dep)
  (when (t2 . > . (current-seconds))
    (eprintf "WARNING: timestamp is in the future: ~e\n" dep)))

(define (try src deps dest objdest includes use-precomp extra-compile-flags expand-extra-flags msvc-pch indirect?)
  (when (or (not re:only) (regexp-match re:only dest))
    (unless (and (file-exists? dest)
		 (let ([t (file-or-directory-modify-seconds dest)])
		   (andmap
		    (lambda (dep)
		      (let ([dep (cond
				  [(bytes? dep) (bytes->path dep)]
				  [else dep])])
			(let ([t2 (file-or-directory-modify-seconds dep)])
			  (check-timestamp t2 dep)
			  (> t t2))))
		    (append deps
			    (if use-precomp (list use-precomp) null)
			    (let ([deps (path-replace-suffix dest #".sdep")])
			      (if (file-exists? deps)
				  (with-input-from-file deps read)
				  null))))))
      (define success? #f)
      (sync
       (thread
        (lambda ()
          (parameterize ([use-compiled-file-paths (list "compiled")]
                         [current-namespace (make-base-namespace)]
                         [current-command-line-arguments
                          (list->vector 
                           (append
                            (list "--setup"
                                  ".")
                            (if objdest
                                (if use-precomp
                                    (list "--precompiled" use-precomp)
                                    null)
                                (list "--precompile"))
                            (if indirect?
                                '("--indirect")
                                null)
                            (list
                             "--depends"
                             "--cpp"
                             (format "~a /MT /E ~a ~a ~a" 
                                     cl.exe
                                     common-cpp-defs
                                     expand-extra-flags 
                                     includes)
                             "-o"
                             dest
                             src)))])
            (dynamic-require "../../racket/gc2/xform.rkt" #f)
            (set! success? #t)))))
      (unless success? 
        (when (file-exists? dest)
          (delete-file dest))
        (error "error xforming")))
    (when objdest
      (c-compile dest objdest null (string-append
				    extra-compile-flags
				    common-cpp-defs
				    (if msvc-pch
					(format " /Fp~a" msvc-pch)
					""))))))

(define (c-compile c o deps flags)
  (unless (and (file-exists? o)
	       (let ([t (file-or-directory-modify-seconds o)])
		 (and (>= t (file-or-directory-modify-seconds c))
		      (andmap
		       (lambda (f)
			 (let ([t2 (file-or-directory-modify-seconds f)])
			   (check-timestamp t2 f)
			   (>= t t2)))
		       deps))))
    (unless (system- (format "~a ~a /MT /Zi /GS- ~a /c ~a /Fdxsrc/ /Fo~a" cl.exe flags opt-flags c o))
      (error "failed compile"))))

(define common-deps (list "../../racket/gc2/xform.rkt"
			  "../../racket/gc2/xform-mod.rkt"))

(define (find-build-file d f)
  (define (find-release d2)
    ;; An ".obj" location may depend on whether CGC was
    ;; built with SGC or the Boehm GC
    (or (for/or ([d '("srelease" "brelease")])
          (define p (string-append d2 d "/"))
          (and (file-exists? (build-path p f))
               p))
        (string-append d2 "release/")))
  (string-append
   (if win64?
     (find-release (string-append "../" d "/x64/"))
     (let ([d2 (find-release (string-append "../" d "/win32/"))])
       (if (directory-exists? d2) d2 (find-release (string-append "../" d "/")))))
   "/" f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mz-inc "/I ../../racket/include /I .. ")

(try "precomp.c" (list* "../../racket/src/schvers.h"
			common-deps)
     "xsrc/precomp.h" #f 
     (string-append mz-inc "/I ../../racket/src")
     #f "" "" #f #f)

(for-each
 (lambda (x)
   (try (format "../../racket/src/~a.c" x)
	(list* (format "../../racket/src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	mz-inc
	"xsrc/precomp.h"
	""
	(string-append "/D LIBMZ_EXPORTS "
		       (if backtrace-gc?
			   "/D MZ_GC_BACKTRACE "
			   ""))
	"mz.pch"
	#f))
 srcs)

(try "../../racket/main.c"
     (list* "../../racket/main.c"
	    common-deps)
     "xsrc/main.c"
     "xsrc/main.obj"
     mz-inc
     #f
     ""
     ""
     #f
     #t)

(try "../../foreign/foreign.c"
     (list* "../../foreign/foreign.c"
	    common-deps)
     "xsrc/foreign.c"
     "xsrc/foreign.obj"
     (string-append
      mz-inc
      "/I../libffi "
      "/I../../foreign/libffi/src/x86 "
      "/I../../foreign/libffi/include "
      "/I../../racket/src ")
     #f
     ""
     ""
     #f
     #f)

(c-compile "../../racket/gc2/gc2.c" "xsrc/gc2.obj"
           (append
	    (list "../mzconfig.h")
	    (map (lambda (f) (build-path "../../racket/" f))
		 '("include/scheme.h"
		   "include/schthread.h"
		   "sconfig.h"
		   "src/schpriv.h"
		   "src/stypes.h"))
	    (map (lambda (f) (build-path "../../racket/gc2/" f))
		 '("gc2.c"
		   "newgc.c"
		   "vm_win.c"
		   "sighand.c"
		   "msgprint.c"
		   "gc2.h"
		   "gc2_obj.h")))
	   (string-append
	    "/D GC2_AS_EXPORT "
	    (if backtrace-gc?
		"/D MZ_GC_BACKTRACE "
		"")
	    mz-inc))
(c-compile "../../racket/src/mzsj86.c" "xsrc/mzsj86.obj" '() mz-inc)

(define dll "../../../lib/libracket3mxxxxxxx.dll")
(define exe "../../../Racket.exe")

(define libs "kernel32.lib user32.lib ws2_32.lib shell32.lib advapi32.lib")

(define (link-dll objs delayloads sys-libs dll link-options exe?)
  (let ([ms (if (file-exists? dll)
		(file-or-directory-modify-seconds dll)
		0)])
    (when (ormap
	   (lambda (f)
	     (> (file-or-directory-modify-seconds f)
		ms))
	   objs)
      (unless (system- (format "~a ~a /MT /Zi /Fe~a ~a ~a /link ~a~a~a~a~a"
			       cl.exe
			       (if exe? "" "/LD /DLL")
			       dll
			       (let loop ([objs (append objs sys-libs)])
				 (if (null? objs)
				     ""
				     (string-append
				      (car objs)
				      " "
				      (loop (cdr objs)))))
			       libs
			       (let loop ([delayloads delayloads])
				 (if (null? delayloads)
				     ""
				     (string-append
				      " /DELAYLOAD:"
				      (car delayloads)
				      " "
				      (loop (cdr delayloads)))))
			       link-options
			       (if exe? " /STACK:8388608" "")
			       (if win64? " /MACHINE:x64" "")
			       (let ([s (regexp-match "^(.*)/([a-z0-9]*)[.]dll$" dll)])
				 (if s
				     (format " /IMPLIB:~a/msvc/~a.lib"
					     (cadr s) (caddr s))
				     ""))))
	(error 'winmake "~a link failed" (if exe? "EXE" "DLL"))))))

(let ([objs (list*
	     "xsrc/gc2.obj"
	     "xsrc/mzsj86.obj"
	     "xsrc/foreign.obj"
	     (find-build-file "libracket" "gmp.obj")
	     (find-build-file "racket" "libffi.lib")
             (append
              (let ([f (and win64?
                            (find-build-file "libracket" "mzsj86w64.obj"))])
                (if (and f (file-exists? f))
                    (list f)
                    null))
              (map (lambda (n) (format "xsrc/~a.obj" n)) srcs)))])
  (link-dll objs null null dll "" #f))

(define (check-rc res rc)
  (unless (and (file-exists? res)
	       (>= (file-or-directory-modify-seconds res)
		   (file-or-directory-modify-seconds rc)))
	  (system- (string-append 
		    "rc /l 0x409 "
		    (format "/fo~a ~a" res rc)))))

(check-rc "racket.res" "../racket/racket.rc")

(let ([objs (list
	     "racket.res"
	     "xsrc/main.obj"
	     "../../../lib/msvc/libracket3mxxxxxxx.lib")])
  (link-dll objs 
	    '("libracket3mxxxxxxx.dll")
	    '("delayimp.lib")
	    exe "" #t))
(system- "mt.exe -manifest ../racket/racket.manifest -outputresource:../../../Racket.exe;1")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wx-inc (string-append "/I ../../racket/include "
			      "/I .. "
			      "/I ../../racket "
			      "/I ../../racket/gc2 "))

(try "../../gracket/grmain.c"
     (list* "../../gracket/grmain.c"
	    common-deps)
     "xsrc/grmain.c"
     "xsrc/grmain.obj"
     wx-inc
     #f
     ""
     "/DWIN32 "
     #f
     #t)

(check-rc "gracket.res" "../gracket/gracket.rc")

(let ([objs (list
	     "gracket.res"
	     "xsrc/grmain.obj"
	     "../../../lib/msvc/libracket3mxxxxxxx.lib")])
  (link-dll objs 
	    '("libracket3mxxxxxxx.dll")
	    '("advapi32.lib" 
	      "delayimp.lib") 
	    "../../../lib/GRacket.exe" " /subsystem:windows" #t))
(system- "mt.exe -manifest ../gracket/gracket.manifest -outputresource:../../../lib/GRacket.exe;1")

(system- (format "~a /MT /O2 /DMZ_PRECISE_GC /I../../racket/include /I.. /c ../../racket/dynsrc/mzdyn.c /Fomzdyn3m.obj"
		 cl.exe))
(system- "lib.exe -def:../../racket/dynsrc/mzdyn.def -out:mzdyn3m.lib")

(define (copy-file/diff src dest)
  (unless (and (file-exists? dest)
	       (string=? (with-input-from-file src (lambda () (read-string (file-size src))))
			 (with-input-from-file dest (lambda () (read-string (file-size dest))))))
    (printf "Updating ~a\n" dest)
    (when (file-exists? dest) (delete-file dest))
    (copy-file src dest)))

(copy-file/diff "mzdyn3m.exp" "../../../lib/msvc/mzdyn3m.exp")
(copy-file/diff "mzdyn3m.obj" "../../../lib/msvc/mzdyn3m.obj")
