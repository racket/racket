
(use-compiled-file-paths null)

(require (lib "restart.ss")
	 (lib "process.ss"))

(define (system- s)
  (fprintf (current-error-port) "~a~n" s)
  (system s))

(define accounting-gc? #t)
(define opt-flags "/O2")
(define re:only #f)

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
    "fun"
    "hash"
    "image"
    "list"
    "module"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
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

(define (try src deps dest objdest includes use-precomp extra-compile-flags expand-extra-flags msvc-pch)
  (when (or (not re:only) (regexp-match re:only dest))
    (unless (and (file-exists? dest)
		 (let ([t (file-or-directory-modify-seconds dest)])
		   (andmap
		    (lambda (dep)
		      (let ([dep (cond
				  [(bytes? dep) (bytes->path dep)]
				  [else dep])])
			(> t (file-or-directory-modify-seconds dep))))
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
				   (list "-r"
					 "../../mzscheme/gc2/xform.ss"
					 "--setup")
				   (if objdest
				       (if use-precomp
					   (list "--precompiled" use-precomp)
					   null)
				       (list "--precompile"))
				   (list
				    "--depends"
				    (format "cl.exe /MT /E ~a ~a" expand-extra-flags includes)
				    src
				    dest)))
				 void))
        (when (file-exists? dest)
	  (delete-file dest))
	(error "error xforming")))
    (when objdest
      (compile dest objdest null (string-append
				  extra-compile-flags
				  (if msvc-pch
				      (format " /Fp~a" msvc-pch)
				      ""))))))

(define (compile c o deps flags)
  (unless (and (file-exists? o)
	       (let ([t (file-or-directory-modify-seconds o)])
		 (and (>= t (file-or-directory-modify-seconds c))
		      (andmap
		       (lambda (f)
			 (>= t (file-or-directory-modify-seconds f)))
		       deps))))
    (unless (system- (format "cl.exe ~a /MT /Zi ~a /c ~a /Fdxsrc/ /Fo~a" flags opt-flags c o))
      (error "failed compile"))))

(define common-deps (list "../../mzscheme/gc2/xform.ss"
			  "../../mzscheme/gc2/xform-mod.ss"))

(define (find-obj f d) (format "../../worksp/~a/release/~a.obj" d f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mz-inc "/I ../../mzscheme/include /I .. ")

(try "precomp.c" (list* "../../mzscheme/src/schvers.h"
			common-deps)
     "xsrc/precomp.h" #f 
     (string-append mz-inc "/I ../../mzscheme/src")
     #f "" "" #f)

(for-each
 (lambda (x)
   (try (format "../../mzscheme/src/~a.c" x)
	(list* ; (find-obj x "libmzsch")
	       (format "../../mzscheme/src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	mz-inc
	"xsrc/precomp.h"
	""
	""
	"mz.pch"))
 srcs)

(try "../../mzscheme/main.c"
     (list* ; (find-obj "main" "mzscheme")
	    "../../mzscheme/main.c"
	    common-deps)
     "xsrc/main.c"
     "xsrc/main.obj"
     mz-inc
     #f
     ""
     ""
     #f)

(try "../../foreign/foreign.c"
     (list* ; (find-obj "main" "mzscheme")
	    "../../foreign/foreign.c"
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
     #f)

(compile "../../mzscheme/gc2/gc2.c" "xsrc/gc2.obj"
	 (map (lambda (f) (build-path "../../mzscheme/gc2/" f))
	      '("gc2.c"
		"compact.c"
		"newgc.c"
		"vm_win.c"
		"sighand.c"
		"msgprint.c"))
	 (string-append
	  "/D GC2_AS_EXPORT "
	  (if accounting-gc?
	      "/D NEWGC_BTC_ACCOUNT "
	      "/D USE_COMPACT_3M_GC")
	  mz-inc))
(compile "../../mzscheme/src/mzsj86.c" "xsrc/mzsj86.obj" '() mz-inc)

(define dll "../../../libmzsch3mxxxxxxx.dll")
(define exe "../../../MzScheme3m.exe")

(define libs "kernel32.lib user32.lib wsock32.lib shell32.lib advapi32.lib")

(define (link-dll objs sys-libs dll link-options exe?)
  (let ([ms (if (file-exists? dll)
		(file-or-directory-modify-seconds dll)
		0)])
    (when (ormap
	   (lambda (f)
	     (> (file-or-directory-modify-seconds f)
		ms))
	   objs)
      (unless (system- (format "cl.exe ~a /MT /Zi /Fe~a unicows.lib ~a ~a ~a"
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
			       link-options))
	(error 'winmake "~a link failed" (if exe? "EXE" "DLL"))))))

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
  (link-dll objs null dll "" #f))

(let ([objs (list
	     "xsrc/main.obj"
	     "../libmzsch/Release/uniplt.obj"
	     "../../../libmzsch3mxxxxxxx.lib")])
  (link-dll objs null exe "" #t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wx-inc (string-append "/I ../../mzscheme/include "
			      "/I .. "
			      "/I ../../mzscheme/gc2 "
			      "/I ../../wxwindow/include/msw "
			      "/I ../../wxwindow/include/base "
			      "/I ../../mred/wxme "
			      "/I ../../wxwindow/contrib/wxxpm/libxpm.34b/lib "
			      "/I ../../wxWindow/contrib/fafa "
			      "/I ../../wxcommon/jpeg /I ../../worksp/jpeg /I ../../wxcommon/zlib "))
(try "wxprecomp.cxx" (list* "../../mzscheme/src/schvers.h" common-deps)
     "xsrc/wxprecomp.h" #f wx-inc #f "" "-DGC2_AS_IMPORT" #f)

(define (wx-try base proj x use-precomp? suffix)
  (let ([cxx-file (format "../../~a/~a.~a" base x suffix)])
    (try cxx-file
	 (list* ; (find-obj x proj)
		cxx-file
		common-deps)
	 (format "xsrc/~a.~a" x suffix)
	 (format "xsrc/~a.obj" x)
	 wx-inc
	 (and use-precomp? "xsrc/wxprecomp.h")
	 "-DGC2_JUST_MACROS /FI../../../mzscheme/gc2/gc2.h"
	 "-DGC2_AS_IMPORT"
	 "wx.pch")))

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

(map (lambda (x)
       (wx-try "wxwindow/src/base" "wxwin" x #t "cxx"))
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

(map (lambda (x)
       (wx-try "wxwindow/src/msw" "wxwin" x #t "cxx"))
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
    "wxs_madm"
    "wxs_mede"
    "wxs_medi"
    "wxs_menu"
    "wxs_mio"
    "wxs_misc"
    "wxs_mpb"
    "wxs_obj"
    "wxs_panl"
    "wxs_rado"
    "wxs_slid"
    "wxs_snip"
    "wxs_styl"
    "wxs_tabc"
    "wxs_win"
    "wxscheme"))

(map (lambda (x)
       (wx-try "mred/wxs" "wxs" x #t "cxx"))
     wxs-srcs)

(define wxme-srcs
  '("wx_cgrec"
    "wx_keym"
    "wx_mbuf"
    "wx_medad"
    "wx_media"
    "wx_medio"
    "wx_mline"
    "wx_mpbrd"
    "wx_mpriv"
    "wx_msnip"
    "wx_snip"
    "wx_style"))

(map (lambda (x)
       (wx-try "mred/wxme" "wxme" x #t "cxx"))
     wxme-srcs)

(define mred-srcs
  '("mred"
    "mredmsw"))

(map (lambda (x)
       (wx-try "mred" "libmred" x #t "cxx"))
     mred-srcs)

(wx-try "wxcommon" "wxme" "wxJPEG" #t "cxx")
(wx-try "mzscheme/utils" "wxme" "xcglue" #f "c")
(compile "../../wxcommon/wxGC.cxx"
	 "xsrc/wxGC.obj"
	 (list "../../worksp/wxme/Release/wxGC.obj")
	 (string-append wx-inc " -DMZ_PRECISE_GC -DGC2_AS_IMPORT -Dwx_msw"))

(let ([objs (append (list
		     "../libmzsch/Release/uniplt.obj"
		     "xsrc/wxGC.obj"
		     "xsrc/wxJPEG.obj"
		     "xsrc/xcglue.obj")
		    (map
		     (lambda (n)
		       (format "xsrc/~a.obj" n))
		     (append wxwin-base-srcs
			     wxwin-msw-srcs
			     wxs-srcs
			     wxme-srcs
			     mred-srcs)))]
      [libs (list
	     "../../../libmzsch3mxxxxxxx.lib"
	     "../../worksp/wxutils/Release/wxutils.lib"
	     "../../worksp/jpeg/Release/jpeg.lib"
	     "../../worksp/png/Release/png.lib"
	     "../../worksp/zlib/Release/zlib.lib")]
      [win-libs (list
		 "comctl32.lib" "glu32.lib" "opengl32.lib"
		 "gdi32.lib" "comdlg32.lib" "advapi32.lib" 
		 "shell32.lib" "ole32.lib" "oleaut32.lib"
		 "winmm.lib")])
  (link-dll (append objs libs) win-libs "../../../libmred3mxxxxxxx.dll" "" #f))

(wx-try "mred" "mred" "mrmain" #f "cxx")

(unless (file-exists? "mred.res")
  (system- (string-append 
	    "rc /l 0x409 /I ../../wxwindow/include/msw /I ../../wxwindow/contrib/fafa "
	    "/fomred.res ../../worksp/mred/mred.rc")))

(let ([objs (list
	     "mred.res"
	     "xsrc/mrmain.obj"
	     "../libmzsch/Release/uniplt.obj"
	     "../../../libmzsch3mxxxxxxx.lib"
	     "../../../libmred3mxxxxxxx.lib")])
  (link-dll objs (list "advapi32.lib") "../../../MrEd3m.exe" "/link /subsystem:windows" #t))

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
(copy-file/diff "../../../libmzsch3mxxxxxxx.lib" "../../../lib/msvc/libmzsch3mxxxxxxx.lib")
