
(define sgc? #f)
(define debug? #f)
(define debug-only null)

(require (lib "thread.ss"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mk-src-path . base)
  (lambda args
    (let loop ([l (append '(up up) base (cond 
					 [(null? args) null]
					 [(string? (car args)) args]
					 [else (car args)]))])
      (cond
       [(null? l)
	(if (null? args)
	    ":"
	    "")]
       [(eq? 'up (car l))
	(string-append ":" (loop (cdr l)))]
       [else
	(format ":~a~a" (car l) (loop (cdr l)))]))))

(define (mk-includes-cmdline includes)
  (let loop ([includes includes])
    (if (null? includes)
	""
	(format "-i ~a ~a"
		(car includes)
		(loop (cdr includes))))))

(define cpp-defines
  (format "-d FOR_MAC -d SILENT -d WX_CARBON -d __STDC__~a"
	  (if sgc? 
	      " -d USE_SENORA_GC" 
	      "")))

(define (make-comp includes)
  (lambda (f)
    (let* ([base (cadr (regexp-match ".*:([^:]*)$" f))]
	   [obj (format "~a.o" base)]
	   [is-c? (regexp-match "[.]c$" base)])
      (list
       obj
       f
       (format "~a \304~a ~a\r\t~a ~a ~a -w off -noMapCR ~a -includes unix -make ~a.dep -curdir~a -o ~a.o"
	       obj
	       (if is-c? "" " carbon.dump")
	       f
	       (if is-c? 
		   "MrC" 
		   "MrCpp -load carbon.dump -rtti off")
	       f
	       (mk-includes-cmdline includes)
	       cpp-defines
	       base
	       (if debug?
		   " -sym full"
		   "")
	       base)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sources
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mz-include ((mk-src-path "mzscheme" "include")))

(define mz-files
  (map (mk-src-path "mzscheme" "src")
       '("salloc.c"
	 "bignum.c"
	 "bool.c"
	 "builtin.c"
	 "char.c"
	 "complex.c"
	 "dynext.c"
	 "env.c"
	 "error.c"
	 "eval.c"
	 "file.c"
	 "fun.c"
	 "gmp:gmp.c"
	 "hash.c"
	 "image.c"
	 "list.c"
	 "module.c"
	 "network.c"
	 "numarith.c"
	 "number.c"
	 "numcomp.c"
	 "numstr.c"
	 "port.c"
	 "portfun.c"
	 "print.c"
	 "rational.c"
	 "read.c"
	 "regexp.c"
	 "sema.c"
	 "setjmpup.c"
	 "string.c"
	 "struct.c"
	 "stxobj.c"
	 "symbol.c"
	 "syntax.c"
	 "thread.c"
	 "type.c"
	 "vector.c")))

(define mz-srcs
  (map
   (make-comp (list mz-include))
   mz-files))

(define gc-include 
  (if sgc?
      ((mk-src-path "mzscheme" "sgc"))
      ((mk-src-path "mzscheme" "gc" "include"))))

(define gc-files
  (if sgc?
      (list ((mk-src-path "mzscheme" "sgc") "sgc.c"))
      (map (mk-src-path "mzscheme" "gc")
	   '("alloc.c"
	     "reclaim.c"
	     "allchblk.c"
	     "misc.c"
	     "mach_dep.c"
	     "os_dep.c"
	     "mark_rts.c"
	     "headers.c"
	     "mark.c"
	     "obj_map.c"
	     "blacklst.c"
	     "finalize.c"
	     "new_hblk.c"
	     "dbg_mlc.c"
	     "malloc.c"
	     "stubborn.c"
	     "checksums.c"
	     "typd_mlc.c"
	     "ptr_chck.c"
	     "mallocx.c"
	     "MacOS.c"))))

(define gc-srcs
  (map
   (make-comp (list gc-include))
   gc-files))

(set! debug-only (append gc-files mz-files))

(define wxb-include ((mk-src-path "wxmac" "include" "base")))
(define wxm-include ((mk-src-path "wxmac" "include" "mac")))
(define wxme-include ((mk-src-path "mred" "wxme")))
(define wxc-include ((mk-src-path "wxcommon")))
(define al-include ((mk-src-path "a-list" "c-headers")))
(define xpm-include ((mk-src-path "wxmac" "contrib" "wxxpm" "libxpm.34b" "lib")))
(define image-include ((mk-src-path "wxmac" "utils" "image" "src")))
(define jpeg-include ((mk-src-path "wxcommon" "jpeg")))
(define mrjpeg-include ((mk-src-path "mac" "mred")))
(define zlib-include ((mk-src-path "wxcommon" "zlib")))
(define simpledrop-include ((mk-src-path "mac" "mzscheme")))

(define wxs-srcs
  (map
   (make-comp (list mz-include
		    gc-include
		    wxm-include
		    wxb-include
		    wxme-include
		    wxc-include
		    al-include))
   (map (mk-src-path "mred" "wxs")
	'("wxs_bmap.cxx"
	  "wxs_butn.cxx"
	  "wxs_chce.cxx"
	  "wxs_ckbx.cxx"
	  "wxs_cnvs.cxx"
	  "wxs_dc.cxx"
	  "wxs_evnt.cxx"
	  "wxs_fram.cxx"
	  "wxs_gage.cxx"
	  "wxs_gdi.cxx"
	  "wxs_glob.cxx"
	  "wxs_item.cxx"
	  "wxs_lbox.cxx"
	  "wxs_madm.cxx"
	  "wxs_mede.cxx"
	  "wxs_medi.cxx"
	  "wxs_menu.cxx"
	  "wxs_mio.cxx"
	  "wxs_misc.cxx"
	  "wxs_mpb.cxx"
	  "wxs_obj.cxx"
	  "wxs_panl.cxx"
	  "wxs_rado.cxx"
	  "wxs_slid.cxx"
	  "wxs_snip.cxx"
	  "wxs_styl.cxx"
	  "wxs_tabc.cxx"
	  "wxs_win.cxx"
	  "wxscheme.cxx"))))

(define wxme-srcs
  (map
   (make-comp (list mz-include
		    gc-include
		    wxm-include
		    wxb-include
		    wxme-include
		    wxc-include))
   (map (mk-src-path "mred" "wxme")
	'("wx_style.cxx"
	  "wx_keym.cxx"
	  "wx_mbuf.cxx"
	  "wx_media.cxx"
	  "wx_mpriv.cxx"
	  "wx_msnip.cxx"
	  "wx_mline.cxx"
	  "wx_mpbrd.cxx"
	  "wx_medad.cxx"
	  "wx_snip.cxx"
	  "wx_cgrec.cxx"
	  "wx_medio.cxx"))))

(define mred-srcs
  (map
   (make-comp (list mz-include
		    gc-include
		    wxm-include
		    wxb-include
		    wxme-include
		    wxc-include
		    al-include
		    jpeg-include
		    mrjpeg-include
		    zlib-include
		    simpledrop-include))
   (map (mk-src-path "mred")
	'((up "mzscheme" "utils" "xcglue.c")
	  (up "wxcommon" "wxGC.cxx")
	  (up "wxcommon" "wxJPEG.cxx")
	  "mred.cxx"
	  "mredmac.cxx"
	  "mrmain.cxx"
	  (up "mac" "mzscheme" "simpledrop.cpp")))))


(define wxbase-srcs
  (map
   (make-comp (list al-include
		    gc-include
		    mz-include
		    wxm-include
		    wxb-include
		    xpm-include
		    image-include
		    wxme-include
		    wxc-include))
   (map (mk-src-path "wxmac" "src" "base")
	'("wb_item.cc"
	  "wb_panel.cc"
	  "wb_timer.cc"
	  "wb_frame.cc"
	  (up up up "wxcommon" "wb_list.cxx")
	  (up up up "wxcommon" "PSDC.cxx")
	  "wb_types.cc"
	  "wb_canvs.cc"
	  "wb_gdi.cc"
	  "wb_main.cc"
	  "wb_utils.cc"
	  "wb_data.cc"
	  (up up up "wxcommon" "wb_hash.cxx")
	  "wb_stdev.cc"
	  "wb_win.cc"
	  "wb_dc.cc"
	  "wb_sysev.cc"
	  "wb_dialg.cc"
	  "wb_obj.cc"
	  "xfspline.cc"))))

(define wxmac-srcs
  (map
   (make-comp (list al-include
		    gc-include
		    mz-include
		    wxm-include
		    wxb-include
		    xpm-include
		    image-include
		    wxme-include
		    wxc-include))
   (map (mk-src-path "wxmac" "src" "mac")
	'("wx_choic.cc"
	  "wx_main.cc"
	  "wx_clipb.cc"
	  "wx_menu.cc"
	  "wx_messg.cc"
	  "wxBorder.cc"
	  "wx_dc.cc"
	  "wxBorderArea.cc"
	  "wx_dccan1.cc"
	  "wx_mnuit.cc"
	  "wxButtonBorder.cc"
	  "wx_dccan2.cc"
	  "wx_dccan3.cc"
	  "wx_panel.cc"
	  "wxLabelArea.cc"
	  "wx_dcmem.cc"
	  "wx_print.cc"
	  "wxMacDC.cc"
	  "wx_dcpr1.cc"
	  "wx_rbox.cc"
	  "wx_rbut.cc"
	  "wx_sbar.cc"
	  "wxMargin.cc"
	  "wx_dialg.cc"
	  "wx_screen.cc"
	  "wxRectBorder.cc"
	  "wx_slidr.cc"
	  "wxScroll.cc"
	  "wx_frame.cc"
	  "wxScrollArea.cc"
	  "wx_gauge.cc"
	  "wxScrollData.cc"
	  "wx_gdi.cc"
	  "wx_app.cc"
	  "wx_util.cc"
	  "wx_area.cc"
	  "wx_tabc.cc"
	  "wx_gbox.cc"
	  "wx_win.cc"
	  "wx_buttn.cc"
	  "wx_item.cc"
	  "wximgfil.cc"
	  "wx_canvs.cc"
	  "wx_lbox.cc"
	  "wx_check.cc"
	  "wx_mac_utils.cc"
	  (up up "utils" "image" "src" "wx_bmp.cc")
	  (up up "utils" "image" "src" "wx_24to8.cc")
	  (up up "utils" "image" "src" "wx_image.cc")
	  (up up "utils" "image" "src" "wx_xbm.cc")))))

(define xpm-srcs
  (map
   (make-comp null)
   (map (mk-src-path "wxmac" "contrib" "wxxpm" "libxpm.34b" "lib")
	'("crbuffri.cpp"
	  "crdatfri.cpp"
	  "create.cpp"
	  "crifrbuf.cpp"
	  "crifrdat.cpp"
	  "data.cpp"
	  "hashtab.cpp"
	  "miscellaneous.cpp"
	  "parse.cpp"
	  "rdftodat.cpp"
	  "rdftoi.cpp"
	  "rgb.cpp"
	  "scan.cpp"
	  "simx.cpp"
	  "wrffrdat.cpp"
	  "wrffri.cpp"))))

(define a-list-srcs
  (map
   (make-comp (list al-include))
   (map (mk-src-path "a-list" "a-list-1.1.9")
	'("ALBirthDeath.c"
	  "ALKeyboard.c"
	  "ALUserPane.c"
	  "LongControls.c"
	  "ALCellData.c"
	  "ALMouse.c"
	  "ALUtilities.c"
	  "ALDrawing.c"
	  "ALScrolling.c"
	  "LongCoords.c"
	  "ALEditing.c"
	  "ALSelecting.c"
	  "QDDrawingState.c"
	  "ALHeirarchical.c"
	  "ALSelectors.c"))))

(define jpeg-srcs
  (map
   (make-comp (list jpeg-include
		    mrjpeg-include))
   (map (mk-src-path "wxcommon" "jpeg")
	'("jcmarker.c"
	  "jdapimin.c"
	  "jdmainct.c"
	  "jfdctflt.c"
	  "jquant1.c"
	  "jcapimin.c"
	  "jcmaster.c"
	  "jdapistd.c"
	  "jdmarker.c"
	  "jfdctfst.c"
	  "jquant2.c"
	  "jcapistd.c"
	  "jcomapi.c"
	  "jdatadst.c"
	  "jdmaster.c"
	  "jfdctint.c"
	  "jutils.c"
	  "jccoefct.c"
	  "jdatasrc.c"
	  "jdmerge.c"
	  "jidctflt.c"
	  "jccolor.c"
	  "jcparam.c"
	  "jdcoefct.c"
	  "jdphuff.c"
	  "jidctfst.c"
	  "jcdctmgr.c"
	  "jcphuff.c"
	  "jdcolor.c"
	  "jdpostct.c"
	  "jidctint.c"
	  "jchuff.c"
	  "jcprepct.c"
	  "jddctmgr.c"
	  "jdsample.c"
	  "jidctred.c"
	  "jcinit.c"
	  "jcsample.c"
	  "jdhuff.c"
	  "jdtrans.c"
	  "jmemmgr.c"
	  "jcmainct.c"
	  "jctrans.c"
	  "jdinput.c"
	  "jerror.c"
	  "jmemnobs.c"))))

(define png-srcs
  (map
   (make-comp (list zlib-include))
   (map (mk-src-path "wxcommon" "libpng")
	'("png.c"
	  "pngerror.c"
	  "pngrio.c"
	  "pngwio.c"
	  "pngmem.c"
	  "pngset.c"
	  "pngget.c"
	  "pngread.c"
	  "pngrtran.c"
	  "pngrutil.c"
	  "pngtest.c"
	  "pngtrans.c"
	  "pngwrite.c"
	  "pngwtran.c"
	  "pngwutil.c"
	  "pngpread.c"))))

(define zlib-srcs
  (map
   (make-comp (list zlib-include))
   (map (mk-src-path "wxcommon" "zlib")
	'("adler32.c"
	  "compress.c"
	  "crc32.c"
	  "gzio.c"
	  "uncompr.c"
	  "deflate.c"
	  "trees.c"
	  "zutil.c"
	  "inflate.c"
	  "infblock.c"
	  "inftrees.c"
	  "infcodes.c"
	  "infutil.c"
	  "inffast.c"))))

(define starter-srcs
  (map
   (make-comp (list ((mk-src-path "mac" "mzscheme"))))
   (list ((mk-src-path "mac" "starter") "starter.cpp"))))

(define all-srcs ; except starter
  (append mz-srcs
	  gc-srcs
	  wxbase-srcs
	  wxmac-srcs
	  wxs-srcs
	  wxme-srcs
	  mred-srcs
	  xpm-srcs
	  a-list-srcs
	  jpeg-srcs
	  png-srcs
	  zlib-srcs))

(define src-dirs
  (list (mk-src-path "mzscheme" "src")
	(mk-src-path "mzscheme" "src" "gmp")
	(mk-src-path "mzscheme" "gc")
	(mk-src-path "mred" "wxs")
	(mk-src-path "mred" "wxme")
	(mk-src-path "mred")
	(mk-src-path "wxcommon")
	(mk-src-path "wxcommon" "jpeg")
	(mk-src-path "wxmac" "src" "base")
	(mk-src-path "wxmac" "src" "mac")
	(mk-src-path "wxmac" "utils" "image" "src")
	(mk-src-path "wxmac" "contrib" "wxxpm" "libxpm.34b" "lib")
	(mk-src-path "a-list" "a-list-1.1.9")))

(define (with-spaces prefix f src-dirs)
  (let loop ([src-dirs src-dirs])
    (if (null? src-dirs)
	""
	(format "~a ~a ~a"
		prefix
		(f (car src-dirs))
		(loop (cdr src-dirs))))))

(with-output-to-file "Makefile"
  (lambda ()
    (printf "# Don't edit this file directly\r")
    (printf "# It's generated by make-make.ss\r\r")
    (printf "OBJS = \266\r")
    (for-each (lambda (p)
		(printf "  ~a\266\r" (car p)))
	      all-srcs)
    (printf "\r\rMZOBJS = \266\r")
    (for-each (lambda (p)
		(printf "  ~a\266\r" (car p)))
	      (append mz-srcs gc-srcs))
    
    (printf "\r\r::::netglue \304 nethack.c\r")
    (printf "\tMrC nethack.c -o nethack.c.o\r")
    (printf "\tPPCLink nethack.c.o -export FillInNetPointers -o ::::netglue \"{SharedLibraries}InterfaceLib\" -xm s\r\r")

    (printf "\r\r::::collects:launcher:GoMr \304 starter.cpp.o\r")
    (printf "\tPPCLink starter.cpp.o \"{SharedLibraries}CarbonLib\" \"{SharedLibraries}StdCLib\"  \"{PPCLibraries}MrCPlusLib.o\" \"{PPCLibraries}PPCCRuntime.o\" \"{PPCLibraries}StdCRuntime.o\" -o ::::collects:launcher:GoMr -c 'MrSt' -m __appstart\r")
    (printf "\tRez ::cw:MrStarter.r -o ::::collects:launcher:GoMr -append\r\r")

    (printf "\rall \304 ::::MrEd ::::netglue ::::collects:launcher:GoMr\r\r")
    (printf "\rmz \304 ::::MzSchemeLib\r\r")

    (printf "\r::::MzSchemeLib \304 {MZOBJS}\r")
    (printf "\tPPCLink {MZOBJS} \"{SharedLibraries}CarbonLib\" \"{SharedLibraries}StdCLib\"  \"{PPCLibraries}StdCRuntime.o\" \"{PPCLibraries}PPCCRuntime.o\" \"{PPCLibraries}MrCPlusLib.o\" -xm s -o ::::MzSchemeLib~a\r"
	    (if debug? " -sym big" ""))

    (printf "\r::::MrEd \304 {OBJS}\r")
    (printf "\tPPCLink {OBJS} \"{SharedLibraries}CarbonLib\" \"{SharedLibraries}StdCLib\"  \"{PPCLibraries}StdCRuntime.o\" \"{PPCLibraries}PPCCRuntime.o\" \"{PPCLibraries}MrCPlusLib.o\" -o ::::MrEd -c 'mReD' -m __appstart~a\r"
	    (if debug? " -sym big" ""))
    (printf "\tRez ::cw:MrEd.r ::cw:MrEd_classic.r -o ::::MrEd -append\r")
    (when debug?
      (printf "\tMakeSym MrEd.xcoff -P -sym big -o MrEd.sym ~a~a\r"
	      (if (null? debug-only)
		  ""
		  (format "-only ~a" (with-spaces "" 
						  (lambda (x) (cadr (regexp-match "([^:]*)$" x)))
						  debug-only)))
	      (with-spaces " -i" (lambda (x) (x)) src-dirs)))
    
    (printf "\rcarbon.dump \304 carbon.c\r\tMrCpp carbon.c -rtti off -dump carbon.dump\r\r")
    
    (for-each (lambda (p)
		(printf "~a\r\r" (caddr p)))
	      all-srcs)

    (for-each (lambda (p)
		(printf "~a\r\r" (caddr p)))
	      starter-srcs)
    
    ;; Append all .dep files:
    (for-each (lambda (f)
		(when (regexp-match "[.]dep$" f)
		  (call-with-input-file f
		    (lambda (f)
		      (copy-port f (current-output-port))))
		  (printf "\r")))
	      (directory-list)))
  'truncate)

