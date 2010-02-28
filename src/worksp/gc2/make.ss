
#lang scheme/base

(use-compiled-file-paths null)

(require mzlib/restart
	 mzlib/process)

(define (system- s)
  (fprintf (current-error-port) "~a~n" s)
  (system s))

(define accounting-gc? #t)
(define backtrace-gc? #f)
(define opt-flags "/O2 /Oy-")
(define re:only #f)

(unless (find-executable-path "cl.exe" #f)
  (error (string-append
	  "Cannot find executable \"cl.exe\".\n"
	  "You may need to find and run \"vsvars32.bat\".")))

(unless (directory-exists? "xsrc")
  (make-directory "xsrc"))

(define srcs
  '("salloc"
    "bignum"
    "bool"
    "builtin"
    "char"
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
    "list"
    "module"
    "mzrt"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
    "places"
    "port"
    "portfun"
    "print"
    "rational"
    "read"
    "regexp"
    "sema"
    "setjmpup"
    "string"
    "struct"
    "symbol"
    "syntax"
    "stxobj"
    "thread"
    "type"
    "vector"))

(define common-cpp-defs " /D _CRT_SECURE_NO_DEPRECATE /D _USE_DECLSPECS_FOR_SAL=0 /D _USE_ATTRIBUTES_FOR_SAL=0 ")

(define (check-timestamp t2 dep)
  (when (t2 . > . (current-seconds))
    (fprintf (current-error-port)
	     "WARNING: timestamp is in the future: ~e\n" 
	     dep)))

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
      (unless (parameterize
	       ([use-compiled-file-paths (list "compiled")])
	       (restart-mzscheme #() (lambda (x) x)
				 (list->vector 
				  (append
				   (list "-u"
					 "../../mzscheme/gc2/xform.ss"
					 "--setup"
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
				    (format "cl.exe /MT /E ~a ~a ~a" 
					    common-cpp-defs
					    expand-extra-flags 
					    includes)
				    "-o"
				    dest
				    src)))
				 void))
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
    (unless (system- (format "cl.exe ~a /MT /Zi ~a /c ~a /Fdxsrc/ /Fo~a" flags opt-flags c o))
      (error "failed compile"))))

(define common-deps (list "../../mzscheme/gc2/xform.ss"
			  "../../mzscheme/gc2/xform-mod.ss"))

(define (find-obj f d) (format "../~a/release/~a.obj" d f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mz-inc "/I ../../mzscheme/include /I .. ")

(try "precomp.c" (list* "../../mzscheme/src/schvers.h"
			common-deps)
     "xsrc/precomp.h" #f 
     (string-append mz-inc "/I ../../mzscheme/src")
     #f "" "" #f #f)

(for-each
 (lambda (x)
   (try (format "../../mzscheme/src/~a.c" x)
	(list* (format "../../mzscheme/src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	mz-inc
	"xsrc/precomp.h"
	""
	(string-append "/D LIBMZ_EXPORTS "
		       (if accounting-gc?
			   "/D NEWGC_BTC_ACCOUNT "
			   "")
		       (if backtrace-gc?
			   "/D MZ_GC_BACKTRACE "
			   ""))
	"mz.pch"
	#f))
 srcs)

(try "../../mzscheme/main.c"
     (list* "../../mzscheme/main.c"
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
      "/I../../foreign/libffi_msvc "
      "/I../../mzscheme/src ")
     #f
     ""
     ""
     #f
     #f)

(c-compile "../../mzscheme/gc2/gc2.c" "xsrc/gc2.obj"
           (append
	    (list "../mzconfig.h")
	    (map (lambda (f) (build-path "../../mzscheme/" f))
		 '("include/scheme.h"
		   "include/schthread.h"
		   "src/schpriv.h"
		   "src/stypes.h"))
	    (map (lambda (f) (build-path "../../mzscheme/gc2/" f))
		 '("gc2.c"
		   "newgc.c"
		   "vm_win.c"
		   "sighand.c"
		   "msgprint.c"
		   "gc2.h"
		   "gc2_obj.h")))
	   (string-append
	    "/D GC2_AS_EXPORT "
	    "/D NEWGC_BTC_ACCOUNT "
	    (if backtrace-gc?
		"/D MZ_GC_BACKTRACE "
		"")
	    mz-inc))
(c-compile "../../mzscheme/src/mzsj86.c" "xsrc/mzsj86.obj" '() mz-inc)

(define dll "../../../lib/libmzsch3mxxxxxxx.dll")
(define exe "../../../MzScheme.exe")

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
      (unless (system- (format "cl.exe ~a /MT /Zi /Fe~a unicows.lib ~a ~a /link ~a~a~a"
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
			       (let ([s (regexp-match "^(.*)/([a-z0-9]*)[.]dll$" dll)])
				 (if s
				     (format " /IMPLIB:~a/msvc/~a.lib"
					     (cadr s) (caddr s))
				     ""))))
	(error 'winmake "~a link failed" (if exe? "EXE" "DLL"))))))

(c-compile "../mzscheme/uniplt.c"
	   "xsrc/uniplt.obj"
	   null
	   " -Dwx_msw")

(let ([objs (list*
	     "../libmzsch/Release/uniplt.obj"
	     "xsrc/gc2.obj"
	     "xsrc/mzsj86.obj"
	     "xsrc/foreign.obj"
	     (find-obj "gmp" "libmzsch")
	     (find-obj "ffi" "libmzsch")
	     (find-obj "win32" "libmzsch")
	     (find-obj "prep_cif" "libmzsch")
	     (find-obj "types" "libmzsch")
	     (map
	      (lambda (n)
		(format "xsrc/~a.obj" n))
	      srcs))])
  (link-dll objs null null dll "" #f))

(define (check-rc res rc)
  (unless (and (file-exists? res)
	       (>= (file-or-directory-modify-seconds res)
		   (file-or-directory-modify-seconds rc)))
	  (system- (string-append 
		    "rc /l 0x409  /I ../../wxwindow/include/msw /I ../../wxwindow/contrib/fafa "
		    (format "/fo~a ~a" res rc)))))

(check-rc "mzscheme.res" "../mzscheme/mzscheme.rc")

(let ([objs (list
	     "mzscheme.res"
	     "xsrc/main.obj"
	     "xsrc/uniplt.obj"
	     "../../../lib/msvc/libmzsch3mxxxxxxx.lib")])
  (link-dll objs 
	    '("libmzsch3mxxxxxxx.dll")
	    '("delayimp.lib")
	    exe "" #t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wx-inc (string-append "/I ../../mzscheme/include "
			      "/I .. "
			      "/I ../../mzscheme/gc2 "
			      "/I ../../wxwindow/include/msw "
			      "/I ../../wxwindow/include/base "
			      "/I ../../mred/wxme "
			      "/I ../../wxwindow/contrib/wxxpm/libxpm.34b/lib "
			      "/I ../../wxWindow/contrib/fafa "
			      "/I ../../wxcommon/jpeg /I ../jpeg /I ../../wxcommon/zlib "))
(try "wxprecomp.cxx" (list* "../../mzscheme/src/schvers.h" common-deps)
     "xsrc/wxprecomp.h" #f wx-inc #f "" "-DGC2_AS_IMPORT" #f #f)

(define (wx-try base proj x use-precomp? suffix indirect?)
  (let ([cxx-file (format "../../~a/~a.~a" base x suffix)])
    (try cxx-file
	 (list* cxx-file
		common-deps)
	 (format "xsrc/~a.~a" x suffix)
	 (format "xsrc/~a.obj" x)
	 wx-inc
	 (and use-precomp? "xsrc/wxprecomp.h")
	 "-DGC2_JUST_MACROS /FI../../../mzscheme/gc2/gc2.h"
	 (string-append "-DGC2_AS_IMPORT"
			(if backtrace-gc?
			    " /D MZ_GC_BACKTRACE"
			    ""))
	 "wx.pch"
	 indirect?)))

(define wxwin-base-srcs
  '("wb_canvs"
    "wb_cmdlg"
    "wb_data"
    "wb_dc"
    "wb_dialg"
    "wb_frame"
    "wb_gdi"
    "wb_hash"
    "wb_item"
    "wb_list"
    "wb_main"
    "wb_obj"
    "wb_panel"
    "wb_print"
    "wb_ps"
    "wb_stdev"
    "wb_sysev"
    "wb_timer"
    "wb_types"
    "wb_utils"
    "wb_win"))

(for-each (lambda (x)
	    (wx-try "wxwindow/src/base" "wxwin" x #t "cxx" #f))
	  wxwin-base-srcs)

(define wxwin-msw-srcs
  '("wx_buttn"
    "wx_canvs"
    "wx_check"
    "wx_choic"
    "wx_clipb"
    "wx_cmdlg"
    "wx_dc"
    "wx_dialg"
    "wx_frame"
    "wx_gauge"
    "wx_gbox"
    "wx_gdi"
    "wx_graph_glue"
    "wx_item"
    "wx_lbox"
    "wx_main"
    "wx_menu"
    "wx_messg"
    "wx_panel"
    "wx_pdf"
    "wx_rbox"
    "wx_slidr"
    "wx_tabc"
    "wx_timer"
    "wx_utils"
    "wx_win"
    "wximgfil"))

(for-each (lambda (x)
	    (wx-try "wxwindow/src/msw" "wxwin" x #t "cxx" #f))
	  wxwin-msw-srcs)

(define wxs-srcs
  '("wxs_bmap"
    "wxs_butn"
    "wxs_chce"
    "wxs_ckbx"
    "wxs_cnvs"
    "wxs_dc"
    "wxs_evnt"
    "wxs_fram"
    "wxs_gage"
    "wxs_gdi"
    "wxs_glob"
    "wxs_item"
    "wxs_lbox"
    "wxs_menu"
    "wxs_misc"
    "wxs_obj"
    "wxs_panl"
    "wxs_rado"
    "wxs_slid"
    "wxs_tabc"
    "wxs_win"
    "wxscheme"))

(for-each (lambda (x)
	    (wx-try "mred/wxs" "wxs" x #t "cxx" #f))
	  wxs-srcs)

(define mred-srcs
  '("mred"
    "mredmsw"))

(for-each (lambda (x)
	    (wx-try "mred" "libmred" x #t "cxx" #f))
	  mred-srcs)

(wx-try "wxcommon" "wxme" "wxJPEG" #t "cxx" #f)
(wx-try "mzscheme/utils" "wxme" "xcglue" #f "c" #f)
(c-compile "../../wxcommon/wxGC.cxx"
	   "xsrc/wxGC.obj"
	   null
	   (string-append wx-inc " -DMZ_PRECISE_GC -DGC2_AS_IMPORT -Dwx_msw"))

(let ([objs (append (list
		     "xsrc/uniplt.obj"
		     "xsrc/wxGC.obj"
		     "xsrc/wxJPEG.obj"
		     "xsrc/xcglue.obj")
		    (map
		     (lambda (n)
		       (format "xsrc/~a.obj" n))
		     (append wxwin-base-srcs
			     wxwin-msw-srcs
			     wxs-srcs
			     mred-srcs)))]
      [libs (list
	     "../../../lib/msvc/libmzsch3mxxxxxxx.lib"
	     "../wxutils/Release/wxutils.lib"
	     "../jpeg/Release/jpeg.lib"
	     "../png/Release/png.lib"
	     "../zlib/Release/zlib.lib")]
      [win-libs (list
		 "comctl32.lib" "glu32.lib" "opengl32.lib"
		 "gdi32.lib" "comdlg32.lib" "advapi32.lib" 
		 "shell32.lib" "ole32.lib" "oleaut32.lib"
		 "winmm.lib")])
  (link-dll (append objs libs) null win-libs "../../../lib/libmred3mxxxxxxx.dll" "" #f))

(wx-try "mred" "mred" "mrmain" #f "cxx" #t)

(check-rc "mred.res" "../mred/mred.rc")

(let ([objs (list
	     "mred.res"
	     "xsrc/mrmain.obj"
	     "xsrc/uniplt.obj"
	     "../../../lib/msvc/libmzsch3mxxxxxxx.lib"
	     "../../../lib/msvc/libmred3mxxxxxxx.lib")])
  (link-dll objs 
	    '("libmzsch3mxxxxxxx.dll" 
	      "libmred3mxxxxxxx.dll")
	    '("advapi32.lib" 
	      "delayimp.lib") 
	    "../../../MrEd.exe" " /subsystem:windows" #t))

(system- "cl.exe /MT /O2 /DMZ_PRECISE_GC /I../../mzscheme/include /I.. /c ../../mzscheme/dynsrc/mzdyn.c /Fomzdyn3m.obj")
(system- "lib.exe -def:../../mzscheme/dynsrc/mzdyn.def -out:mzdyn3m.lib")

(define (copy-file/diff src dest)
  (unless (and (file-exists? dest)
	       (string=? (with-input-from-file src (lambda () (read-string (file-size src))))
			 (with-input-from-file dest (lambda () (read-string (file-size dest))))))
    (printf "Updating ~a~n" dest)
    (when (file-exists? dest) (delete-file dest))
    (copy-file src dest)))

(copy-file/diff "mzdyn3m.exp" "../../../lib/msvc/mzdyn3m.exp")
(copy-file/diff "mzdyn3m.obj" "../../../lib/msvc/mzdyn3m.obj")

